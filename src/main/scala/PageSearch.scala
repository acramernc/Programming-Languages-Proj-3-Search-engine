import scala.math.log
import scala.collection.parallel.CollectionConverters._

object PageSearch {
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        var searchResult = scala.collection.mutable.ListBuffer.empty[Double]
        for (page <- pages) {
            var total = 0
            for (q <- query) {
                val count = page.text.sliding(q.length).count(_ == q) //count the number of times q appears in page
                total += count
            }
            searchResult.addOne(total)
        }
        searchResult.toList
    }

    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        // TODO: complete implementation
    }

    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        // TODO: complete implementation
    }
}