import scala.util.Random
import scala.collection.parallel.CollectionConverters._
import scala.collection.mutable.Map

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
        result
    }

    /*
    This method should map the id of each page in pages to the number of other pages that link to this
    page. That is, it should count the number of other pages for which this page's id appears in that
    page's list of links. This will need to be converted to a double
    */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: complete implementation
        val indegreeMap = scala.collection.mutable.Map.empty[String, Double]
        for (page <- pages.values) {
            for (linkId <- page.links) {
                if (indegreeMap contains linkId) {
                    indegreeMap(linkId) = indegree(linkId) + 1.0
                }
                else {
                    indegreeMap + (link -> 1.0)
                }
            }
        }
        indegreeMap
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
        // TODO: complete implementation
    }
}