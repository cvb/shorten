package shorten
package models

import scala.util.Random
import scalikejdbc._
import org.joda.time._
import org.postgresql.util.PSQLException

case class User(id: Int, token: String)

object User extends SQLConstruct[User] with RecRunner {
  override val tableName = "users"
  def apply(rn: ResultName[User])(rs: WrappedResultSet): User =
    autoConstruct(rs, rn)

  def createNew()(implicit s: DBSession = AutoSession): User = {
    createNewRec(5, () =>
      sql"""insert into users (token) values
              (${Random.alphanumeric.take(50).mkString}) """)
      .flatMap(findById(_)).get
  }
  def findByToken(token: String)(implicit s: DBSession = AutoSession) = {
    Helper.findOne(User, sqls" token = ${token} ")
  }

}

case class Folder(id: Int, title: String, userId: Int, user: Option[User] = None)
object Folder extends SQLConstruct[Folder]{
  override val tableName = "folders"
  def apply(rn: ResultName[Folder])(rs: WrappedResultSet): Folder =
    autoConstruct(rs, rn, "user")

  def findByUser(userId: Long, lim: Int, off: Int)
    (implicit s: DBSession = AutoSession) = {
    Helper.findMany(Folder, Some(sqls"user_id = ${userId}"), lim, off)
  }

}

case class LinkWithClicks(link: Link, clicks: Long)
case class Link(id: Int, url: String, code: String,
                userId: Int, folderId: Option[Int],
                user: Option[User] = None,
                folder: Option[Option[Folder]] = None)

object Link extends SQLConstruct[Link] with RecRunner{
  override val tableName = "links"
  def apply(rn: ResultName[Link])(rs: WrappedResultSet): Link =
    autoConstruct(rs, rn, "user", "folder")

  def findByCode(code: String)(implicit s: DBSession = AutoSession) = {
    Helper.findOne(Link, sqls"code = ${code}")
  }

  def findByFolder(folderId: Long, lim: Int, off: Int)
    (implicit s: DBSession = AutoSession) = {
    Helper.findMany(Link, Some(sqls"folder_id = ${folderId}"), lim, off)
  }

  def createNew(
    userId: Int, url: String, code: Option[String], folderId: Option[Long])
    (implicit s: DBSession = AutoSession) = code match {

    case None => createNewRec(5, () => {
      val randStr = Random.alphanumeric.take(5).mkString
      sql"""insert into links (user_id, url, folder_id, code) values
              (${userId}, ${url}, ${folderId}, ${randStr})
         """}) flatMap { findById(_) }

    case Some(c) => findById(
      sql"""insert into links (user_id, url, folder_id, code) values
              (${userId}, ${url}, ${folderId}, ${c}) """
        .updateAndReturnGeneratedKey.apply())
  }

  def getAll(limit: Int, offset: Int)(implicit s: DBSession = AutoSession) = {
    Helper.findMany(Link, None, limit, offset)
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

trait SQLConstruct[T] extends SQLSyntaxSupport[T] {
  def apply(rn: ResultName[T])(rs: WrappedResultSet): T

  def findById(id: Long): Option[T] = {
    Helper.findOne(this, sqls" id = ${id} ")
  }
}

object Helper {

  def findOne[T](model: SQLConstruct[T], whereSyntax: SQLSyntax)
    (implicit s: DBSession = AutoSession): Option[T] = {
    val m = model.syntax("model")
    sql"select ${m.result.*} from ${model.as(m)} where ${whereSyntax} limit 1"
      .map(model(m.resultName)).single.apply()
  }

  def findMany[T](model: SQLConstruct[T], whereSyntax: Option[SQLSyntax],
    limit: Int = 1000, offset: Int = 0)
    (implicit s: DBSession = AutoSession): List[T] = {
    val m = model.syntax("model")
    val whereClause = whereSyntax.map(w => sqls""" where $w """)
      .getOrElse(sqls"")
    sql"""select ${m.result.*} from ${model.as(m)}
            ${ whereClause }
            limit ${limit} offset ${offset}
       """
      .map(model(m.resultName)).list.apply()
  }
}
