package shorten

case class LinkResp(url: String, code: String)
case class LinkInfo(link: LinkResp, folder: Int, clicks: Int)
case class FolderResp(id: Int, title: String)

