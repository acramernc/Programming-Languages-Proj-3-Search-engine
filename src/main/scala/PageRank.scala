import scala.util.Random
//import scala.collection.parallel.CollectionConverters._

object PageRank {

    // Method should be complete - all strings in pages map are mapped to 1.0 --> Needs to be tested
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        val result = pages.foldLeft(Map.empty[String, Double]) { case (map, (k, v)) =>
            if (map contains k) {
                map
            }
            else {
                map + (k -> 1.0)
            }
        }
        result.toMap
    }

    /*
    This method should map the id of each page in pages to the number of other pages that link to this
    page. That is, it should count the number of other pages for which this page's id appears in that
    page's list of links. This will need to be converted to a double
    */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        val map = scala.collection.mutable.Map[String, Double]()
        for (page <- pages.values) {
            for (item <- page.links) {
                if (map contains item) {
                    map(item) = map(item) + 1.0
                }
                else {
                    map += (item -> 1.0)
                }
            }
        }
        map.toMap
    }

    /*
    This method is an extension of the indegree method that accounts for differences in importance
    between the pages that link to a given page. Most simply a link from an important page should 
    provide more link than a link from a relatively obscure page. This as you might imagine can be 
    difficult to compute and is usually done with linear algebra. For our purposes, we will take the 
    random walk approach outlined in Appendix B. In short, we drop a large number of independent walkers 
    on random pages. These walkers then repeatedly choose a page the current page links to and go to a new 
    page. The distribution of where these walkers are positioned after a large number of steps is the 
    distribution of the importance of the pages in our system. 
    */
    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        // 1. Choose a random starting vertex
        val rng = scala.util.Random  
        def walk(current: WebPage, steps: Int): WebPage = steps match {
            case 0 => current
            case s if (rng.nextDouble <= 0.15 || current.links.length < 1) => {
                val pagesList = pages.values.toList
                walk(pagesList(rng.nextInt(pages.values.size)), steps - 1)
            }
            case _ => {
                val pageId = current.links(rng.nextInt(current.links.length))
                val nextPage = pages(pageId)
                walk(nextPage, steps - 1)
            }
        }
        val list = pages.values.toList
        println(s"Start Point: ${list(1).id}")
        println(s"Start Point Links: ${list(1).links}")
        println(s"End Point: ${walk(list(1), 100).id}")

        // Try to do it first not in parallel
        val map = scala.collection.mutable.Map[String, Double]()
        val webPages = pages.values.toList
        val dataList = 1 to 10000
        for (i <- 1 to 10000) {
            val random = rng.nextInt(webPages.size)
            val walkResult = walk(webPages(random), 100)
            if (map contains walkResult.id) { map(walkResult.id) = map(walkResult.id) + 1.0 }
            else { map += (walkResult.id -> 1.0) }
        }
        map.toMap
    }
}