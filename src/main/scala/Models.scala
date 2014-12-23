package shorten
package models

import scala.util.Random
import scalikejdbc._
import org.joda.time._
import org.postgresql.util.PSQLException

case class User(id: Int, token: String)

object User extends SQLSyntaxSupport[User] with RecRunner {
  override val tableName = "users"
  def apply(rs: WrappedResultSet, rn: ResultName[User]): User =
    autoConstruct(rs, rn)

  def createNew()(implicit s: DBSession = AutoSession): User = {
    createNewRec(5, () =>
      sql"""insert into users (token) values
              (${Random.alphanumeric.take(50).mkString}) """)
      .flatMap(findById(_)).get
  }

  def findById(id: Long)(implicit s: DBSession = AutoSession) = {
    sql""" select * from users where id = ${id} """
      .map(rs => User(rs.int("id"), rs.string("token")))
      .single.apply()
  }

  def findByToken(token: String)(implicit s: DBSession = AutoSession) = {
    sql""" select * from users where token = ${token} """
      .map(rs => User(rs.int("id"), rs.string("token")))
      .single.apply()
  }

}

case class Folder(id: Int, title: String, userId: Int, user: Option[User] = None)
object Folder extends SQLSyntaxSupport[Folder]{
  override val tableName = "folders"
  def apply(rn: ResultName[Folder])(rs: WrappedResultSet): Folder =
    autoConstruct(rs, rn, "user")

  def findById(id: Long)(implicit s: DBSession = AutoSession) = {
    val c = Folder.syntax("link")
    sql"select ${c.result.*} from ${Folder.as(c)} where id = ${id}"
      .map(Folder(c.resultName)).single.apply()
  }

  def findByUser(userId: Long, lim: Int, off: Int)
    (implicit s: DBSession = AutoSession) = {
    val c = Folder.syntax("folder")
    sql"""select ${c.result.*} from ${Folder.as(c)}
            where user_id = ${userId}
            limit $lim offset $off"""
      .map(Folder(c.resultName)).list.apply()
  }

}

case class LinkWithClicks(link: Link, clicks: Long)
case class Link(id: Int, url: String, code: String,
                userId: Int, folderId: Option[Int],
                user: Option[User] = None,
                folder: Option[Option[Folder]] = None)

object Link extends SQLSyntaxSupport[Link] with RecRunner{
  override val tableName = "links"
  def apply(rn: ResultName[Link])(rs: WrappedResultSet): Link =
    autoConstruct(rs, rn, "user", "folder")

  def findById(id: Long)(implicit s: DBSession = AutoSession) = {
    val c = Link.syntax("link")
    sql"select ${c.result.*} from ${Link.as(c)} where id = ${id}"
      .map(Link(c.resultName)).single.apply()
  }

  def findByCode(code: String)(implicit s: DBSession = AutoSession) = {
    val c = Link.syntax("link")
    sql"select ${c.result.*} from ${Link.as(c)} where code = ${code}"
      .map(Link(c.resultName)).single.apply()
  }

  def findByFolder(folderId: Long, lim: Int, off: Int)
    (implicit s: DBSession = AutoSession) = {
    val c = Link.syntax("link")
    sql"""select ${c.result.*} from ${Link.as(c)}
            where folder_id = ${folderId}
            limit $lim offset $off"""
      .map(Link(c.resultName)).list.apply()
  }

  def createNew(
    userId: Int, url: String, code: Option[String], folderId: Option[Long])
    (implicit s: DBSession = AutoSession) = code match {

    case None => createNewRec(5, () => {
      val randStr = Random.alphanumeric.take(50).mkString
      sql"""insert into links (user_id, url, folder_id, code) values
              (${userId}, ${url}, ${folderId}, ${randStr})
         """}) flatMap { findById(_) }

    case Some(c) => findById(
      sql"""insert into links (user_id, url, folder_id, code) values
              (${userId}, ${url}, ${folderId}, ${c}) """
        .updateAndReturnGeneratedKey.apply())
  }

  def getAll(limit: Int, offset: Int)(implicit s: DBSession = AutoSession) = {
    val c = Link.syntax("link")
    sql"select ${c.result.*} from ${Link.as(c)} limit ${limit} offset ${offset}"
      .map(Link(c.resultName)).list.apply()
  }
}

case class Click(id: Int, date: DateTime, referer: String, remoteIp: String,
                 linkId: Int, link: Option[Link] = None)
object Click extends SQLSyntaxSupport[Click]{
  override val tableName = "clicks"
  def apply(rn: ResultName[Click])(rs: WrappedResultSet): Click =
    autoConstruct(rs, rn, "link")

  def clicksCount(linkId: Long)(implicit s: DBSession = AutoSession): Long = {
    sql"select count(1) from clicks where link_id = ${linkId}"
      .map(_.long("count")).single.apply().getOrElse(0)
  }

  def createForLink(linkId: Long, ref: String, remIp: String)
    (implicit s: DBSession = AutoSession): Long = {
    sql"""insert into clicks (referer, remote_ip, link_id)
            values (${ref}, ${remIp}, ${linkId})"""
      .updateAndReturnGeneratedKey.apply()
  }
}

trait RecRunner {
  // recursively trying to create model when catch uniq violation
  def createNewRec(n:Int, r: () => SQL[Nothing, NoExtractor])(
    implicit s: DBSession = AutoSession): Option[Long] =
    try { Some(r().updateAndReturnGeneratedKey.apply())
    } catch  {
      // 23505 - unique_violation
      // http://www.postgresql.org/docs/9.3/static/errcodes-appendix.html
      case e: PSQLException if(e.getSQLState == "23505")  =>
        if (n > 0) createNewRec(n-1, r) else None
  }
}
