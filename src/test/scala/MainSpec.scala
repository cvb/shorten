package shorten

import akka.actor.Actor
import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http._
import spray.httpx.RequestBuilding._
import spray.httpx._
import StatusCodes._
import spray.routing._
import AuthenticationFailedRejection._

import scalikejdbc._
import scalikejdbc.config._

import com.typesafe.config._

import spray.json._
import DefaultJsonProtocol._ // if you don't supply your own Protocol (see below)
import spray.httpx.unmarshalling._
import spray.httpx.marshalling._

import shorten.models._

class ShortenSrvSpec extends Specification with Specs2RouteTest
    with ShortenSrv with SprayJsonSupport {

  sequential

  def actorRefFactory = system

  val settings = Settings(system)

  DBsWithEnv("test").setupAll()
  implicit val session = AutoSession
  sql"""truncate table users cascade""".execute.apply()

  import DefaultJsonProtocol._

  val routes = provide(settings.secret)(myRoute)

  "ShortenSrv" should {
    val user = User.createNew
    val secret = settings.secret

    "return token when get correct user_id and secret" in {
      Get(s"/login?user_id=${user.id}&secret=${secret}") ~> routes ~> check {
        status === OK
        body.asString must contain(user.token)
      }
    }

    "return Unauthorized when got wrong secret" in {
      Get(s"/login?user_id=${user.id}&secret=${secret + 'f}") ~>
      routes ~> check {
        rejection === AuthenticationFailedRejection(CredentialsRejected, List())
        }
    }

    "return link and code after post with new link" in {
      Post("/link", FormData(Map(
        ("token" -> user.token),
        ("url" -> "http://somedomain.org")))
      ) ~> routes ~> check {
        status === OK
        contentType === ContentTypes.`application/json`
        val resp = responseAs[Map[String,Option[String]]]
        resp.get("url").flatten === Some("http://somedomain.org")
      }
    }

    "add click and return url" in {
      val link = Link.createNew(user.id, "http://somedomain.org", None, None)
      val clicks = Click.clicksCount(link.get.id)
      Post(s"/link/${link.get.code}", FormData(Map(
        ("referer" -> "http://somedomain.org"),
        ("remote_ip" -> "127.0.0.1")))
      ) ~> routes ~> check {
        status === OK
        contentType === ContentTypes.`application/json`
        val resp = responseAs[Map[String,Option[String]]]
        resp.get("url").flatten  === Some(link.get.url)
        resp.get("code").flatten === Some(link.get.code)
        (clicks + 1) === Click.clicksCount(link.get.id)
      }
    }

    "return link info by code" in {
      val link = Link.createNew(user.id, "http://somedomain.org", None, None)
      Get(s"/link/${link.get.code}?token=${user.token}") ~> routes ~> check {
        status === OK
        contentType === ContentTypes.`application/json`
        val resp = responseAs[JsObject].fields
        resp.get("url")    === Some(JsString(link.get.url))
        resp.get("code")   === Some(JsString(link.get.code))
        resp.get("clicks") === Some(JsNumber(Click.clicksCount(link.get.id)))
      }
    }

    val fid1 = sql"""insert into folders (user_id, title)
                        values (${user.id}, 'testFolder1')"""
      .updateAndReturnGeneratedKey.apply()
    val fid2 = sql"""insert into folders (user_id, title)
                        values (${user.id}, 'testFolder2')"""
      .updateAndReturnGeneratedKey.apply()
    Link.createNew(user.id, "somedomain", None, Some(fid1))
    Link.createNew(user.id, "somedomain", None, Some(fid1))
    Link.createNew(user.id, "somedomain", None, Some(fid1))

    "return links from folder" in {
      Get(s"/folder/${fid1}?token=${user.token}&offset=0&limit=10") ~>
        routes ~> check {
          status === OK
          val resp = responseAs[JsArray]
          resp.elements.length === 3
      }
    }

    "return list of user links" in {
      Get(s"/link?token=${user.token}&offset=0&limit=10")  ~> routes ~> check {
        status === OK
        val resp = responseAs[JsArray]
        resp.elements must not be empty
      }
    }

    "return list of user folders" in {
      Get(s"/folder?token=${user.token}&offset=0&limit=10")  ~> routes ~> check {
        status === OK
        responseAs[JsArray].elements must have length(2)
      }
    }

    "return list of clicks" in {
      val link = Link.createNew(user.id, "somedomain", None, Some(fid1)).get
      val click = Click.createForLink(link.id, "ref", "127.0.0.1")
      Get(s"/link/${link.code}/clicks?token=${user.token}&offset=0&limit=10") ~>
      routes ~> check {
        status === OK
      }
    }

  }
}
