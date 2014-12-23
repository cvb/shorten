package shorten

import scala.concurrent.{ Future, ExecutionContext }
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.Actor
import spray.routing._
import spray.http._
import spray.httpx._
import spray.routing.authentication.ContextAuthenticator
import StatusCodes._
import MediaTypes._
import AuthenticationFailedRejection._
import Directives._

import shapeless._

import scalikejdbc._
import scalikejdbc.config._

import spray.json._

import shorten.models._

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class ShortenActor extends Actor with ShortenSrv {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  val secret = Settings(context.system).secret
  DBsWithEnv("development").setupAll()
  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(provide(secret)(myRoute))
}

object MyJsonProtocol extends DefaultJsonProtocol {
   implicit object LinkJsonFormat extends RootJsonFormat[Link] {
     def write(c: Link) = JsObject(
       "url"      -> JsString(c.url),
       "code"     -> JsString(c.code),
       "folderId" -> c.folderId.toJson
     )
     def read(c: JsValue) = ???
   }
  implicit object LinkWithClicksJsonFormat
      extends RootJsonFormat[LinkWithClicks] {
    def write(c: LinkWithClicks) =
      new JsObject(
        c.link.toJson.asJsObject.fields + ("clicks" -> JsNumber(c.clicks)))

    def read(j: JsValue) = ???
  }

  implicit object FolderJsonFormat extends RootJsonFormat[Folder] {
    def write(c: Folder) =
      JsObject("id" -> JsNumber(c.id), "title" -> JsString(c.title))

    def read(j: JsValue) = ???
  }

  implicit object ClickJsonFormat extends RootJsonFormat[Click] {
    private val dateTimeFmt = org.joda.time.format.ISODateTimeFormat.dateTime

    def write(c: Click) = JsObject(
      "id"       -> JsNumber(c.id),
      "date"     -> JsString(dateTimeFmt.print(c.date)),
      "referer"  -> JsString(c.referer),
      "remoteIp" -> JsString(c.remoteIp)
      )

    def read(j: JsValue) = ???
  }

}

// this trait defines our service behavior independently from the service actor
trait ShortenSrv extends HttpService with SprayJsonSupport {

  import MyJsonProtocol._
  // implicit val session = AutoSession

  def checkToken: Directive1[User] = {
    anyParam('token) hflatMap { case token :: HNil =>
      val usr = User.findByToken(token)
      authorize(usr.nonEmpty) hflatMap { _ => provide(usr.get) }
    }
  }

  val limOff = parameters('limit.as[Int] ? 10, 'offset.as[Int] ? 0)

  def authUser(secret: String) =
    parameters('user_id.as[Int], 'secret.as[String]) { (userId, s) =>
      authenticate(SecretAuth(secret, s, userId)) { user =>
        complete(user.token)
      }
    }

  val extractNewLinkParams =
    formFields('url, 'code.as[Option[String]], 'folder_id.as[Option[Long]])

  def createNewLink( usr: User
                   , url: String
                   , code: Option[String]
                   , fldId: Option[Long]) =
    (code, Link.createNew(usr.id, url, code, fldId)) match {
      // Built successfully
      case (_, Some(lnk)) =>
        complete(lnk)
      // Get uniq code violation with user code
      case (Some(code), None) =>
        complete(Conflict)
      // Can't make code recursively, too many collisions
      case (None, None) =>
        complete(InternalServerError, "Can't create link")
    }

  def findLinkByCode(code: String) =
    Link.findByCode(code) match {
      case None    => complete(NotFound)
      case Some(l) => complete(LinkWithClicks(l, Click.clicksCount(l.id)))
    }

  def resolveCode(code: String)(referer: String, ip: String) =
    Link.findByCode(code) match {
      case None    => complete(NotFound)
      case Some(l) =>
        Click.createForLink(l.id, referer, ip)
        complete(l)
    }

  val myRoute = { secret: String =>
    path("login") { get { authUser(secret) }
    } ~
    path("link") {
      get {
        (checkToken & limOff) { (_: User, lim, off) =>
          complete(Link.getAll(lim, off))
        }
      } ~
      post {
        (checkToken & extractNewLinkParams)(createNewLink)
      }
    } ~
    path("link" / Segment) { code =>
      get {
        checkToken { _ => findLinkByCode(code) }
      } ~
      post {
        formFields('referer, 'remote_ip)(resolveCode(code))
      }
    } ~
    path("link" / Segment / "clicks") { code =>
      get {
        (checkToken & limOff) { (usr, lim, off) =>
          complete(Click.clicksForCode(code, usr, lim, off))
        }
      }
    }~
    path("folder") {
      get {
        (checkToken & limOff) { (usr, lim, off) =>
          complete(Folder.findByUser(usr.id, lim, off))
        }
      }
    } ~
    path("folder" / IntNumber) { id =>
      get {
        (checkToken & limOff) { (usr, lim, off) =>
          Folder.findById(id) match {
            // check that requested folder belongs to authed user
            case Some(f) if f.userId == usr.id =>
              complete(Link.findByFolder(id, lim, off))
            case _ => complete(Forbidden)
          }
        }
      }
    }
  }
}

case class SecretAuth(haveSecret: String, gotSecret: String, uid: Int)(
  implicit val executionContext: ExecutionContext)
    extends ContextAuthenticator[User]{
  def apply(ctx: RequestContext) = Future {
    haveSecret == gotSecret match {
      case true  => Right(User.findById(uid).getOrElse(User.createNew))
      case false =>
        Left(AuthenticationFailedRejection(CredentialsRejected, List()))
    }
  }
}
