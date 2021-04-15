import scala.math.log
import scala.collection.parallel.CollectionConverters._

object PageSearch {
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.par.map((p: RankedWebPage) => {
            (for (q <- query) yield p.text.split(q,-1).length - 1).sum.toDouble
        }).toList
    }

    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.par.map((p: RankedWebPage) => {
            (for (q <- query) yield p.text.split(q,-1).length - 1).sum /  p.text.length.toDouble
        }).toList
    }

    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.par.map((p: RankedWebPage) => {
            (for (q<-query) yield tf(List[RankedWebPage](p), List[String](q))(0) * idf(pages, q)).sum
        }).toList
    }

    def idf(pages: List[RankedWebPage], term:String): Double ={
        math.log( pages.length.toDouble / ((for(p <- pages if p.text.contains(term)) yield 1).sum + 1) )
    }
}