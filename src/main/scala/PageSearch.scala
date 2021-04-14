import scala.math.log
import scala.collection.parallel.CollectionConverters._

object PageSearch {
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.map((p: RankedWebPage) => {
            (for (q <- query) yield p.text.split(q,-1).length - 1).sum
        })
    }

    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.map((p: RankedWebPage) => {
            (for (q <- query) yield p.text.split(q,-1).length - 1).sum /  p.text.length.toDouble
        })
    }

    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {

    }

    def idf(pages: List[RankedWebPage], term:String): Double ={
        math.log((for(p <- pages) yield p.text.split(term).length).sum / (pages.length +1))
    }
}