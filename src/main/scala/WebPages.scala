class WebPage(val id: String, val name: String, val url: String,
              val text: String, val links: List[String]) {
}

class RankedWebPage(override val id: String, override val name: String, override val url: String, override val text: String,
                    override val links: List[String], val weight: Double) extends WebPage( id, name, url, text, links ) {
    def this(page: WebPage, rank: Double) {
        this(page.id,page.name,page.url,page.text,page.links,rank)
    }
}

class SearchedWebPage(override val id: String, override val name: String, override val url: String,
                      override val text: String, override val links: List[String], override val weight: Double, val textmatch: Double)
                      extends RankedWebPage( id, name, url, text, links, weight ) {
    def this(page: RankedWebPage, tm: Double){
        this(page.id,page.name,page.url,page.text,page.links,page.weight,tm)
    }

}

