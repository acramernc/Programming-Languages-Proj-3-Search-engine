import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Sorting

object AskWillie {
    def main(args: Array[String]) {
        println("=============================================================")
        println("   _____          __      __      __.__.__  .__  .__ ")
        println("  /  _  \\   _____|  | __ /  \\    /  \\__|  | |  | |__| ____  ")
        println(" /  /_\\  \\ /  ___/  |/ / \\   \\/\\/   /  |  | |  | |  |/ __ \\")
        println("/    |    \\___ \\|     <   \\        /|  |  |_|  |_|  \\  ___/ ")
        println("\\____|__  /____  >__|_ \\   \\__/\\  / |__|____/____/__|\\___  >")
        println("        \\/     \\/     \\/        \\/                       \\/")
        println("=============================================================")

        //Instructions print out
        println(Console.YELLOW + "INSTRUCTIONS:\nWait For the " + Console.GREEN + "\"Search:\"" + Console.YELLOW + " to appear.")
        println(Console.YELLOW + "To exit the search type: " + Console.RED + "\":quit\"")

        // Load WebPage.id -> WebPage map to better handle graph
        val pages: Map[String, WebPage] = mapWebPages(loadWebPages)

        //Variables to change for the algorithm
        val pr = PageRank.pagerank _
        val ps = PageSearch.tfidf _
        val meanclass = GeometricMeanOrdering

        //Rank the pages and then normalize them from 0 to 1
        val pagesRanked = pr(pages)
        val normalizedRankedPages = normalizeRanks(pages,pagesRanked)
        
        //Loop for the searching
        while(true){
            print(Console.GREEN + "Search: " + Console.WHITE)
            val queryList = scala.io.StdIn.readLine().split(" ").toList.map(s => s.toLowerCase)
            if(queryList(0) == ":quit") System.exit(0)  //Exit the program if the user types :quit

            //Zip together the normalized Pages and the searches and then send those to SearchedWebPage objects
            val zippedRankedAndSearched = normalizedRankedPages zip ps(normalizedRankedPages,queryList)
            val normalizedMatchedRatingsSearchedWebPages = normalizeMatchRatings(zippedRankedAndSearched).toArray

            //Sort and print the results
            Sorting.quickSort(normalizedMatchedRatingsSearchedWebPages)(meanclass)
            for(p <- Range(normalizedMatchedRatingsSearchedWebPages.length-1, normalizedMatchedRatingsSearchedWebPages.length-11,-1)) println( Console.WHITE + (normalizedMatchedRatingsSearchedWebPages.length - p) + ". " + Console.WHITE + normalizedMatchedRatingsSearchedWebPages(p).name + " " + Console.BLUE + normalizedMatchedRatingsSearchedWebPages(p).url)
        }
        
    }

    //For sorting with arithmetic mean
    object ArithmeticMeanOrdering extends Ordering[SearchedWebPage]{
        def compare(a: SearchedWebPage, b: SearchedWebPage) = ((a.weight + a.textmatch)/2) compare ((b.weight + b.textmatch)/2)
    }

    //For sorting with geometric mean
    object GeometricMeanOrdering extends Ordering[SearchedWebPage]{
        def compare(a: SearchedWebPage, b: SearchedWebPage) = (scala.math.sqrt(a.weight * a.textmatch)) compare (scala.math.sqrt(b.weight * b.textmatch))
    }

    //For sorting with harmonic mean
    object HarmonicMeanOrdering extends Ordering[SearchedWebPage]{
        def compare(a: SearchedWebPage, b: SearchedWebPage) = 2*(a.weight * a.textmatch)/(a.weight + a.textmatch) compare 2*(b.weight * b.textmatch)/(b.weight + b.textmatch)
    }

    //Normalize the page ranks
    def normalizeRanks(p: Map[String, WebPage], m: Map[String,Double]): List[RankedWebPage] = {
        val min = m.minBy{case (key,value) => (value)}
        val max = m.maxBy{case (key,value) => (value)}
        def nf(x: Double): Double = {
            (x - min._2) / (max._2 - min._2)
        }
        //If we have the same max and min then we assign the value to 0.5
        if(min._2 == max._2) (for(key <- m.keys) yield new RankedWebPage(p(key),0.5)).toList else (for(key <- m.keys) yield new RankedWebPage(p(key),nf(m(key)))).toList
    }

    //Normalize the match ratinngs
    def normalizeMatchRatings(l: List[(RankedWebPage,Double)]): List[SearchedWebPage] = {
        val min = l.minBy{case(key,value) => (value)}
        val max = l.maxBy{case(key,value) => (value)}
        def nf(x: Double): Double = {
            (x - min._2) / (max._2 - min._2)
        }
        //If we have the same max and min then we assign the value to 0.5
        if(min._2 == max._2) (for(tup <- l) yield new SearchedWebPage(tup._1,0.5)).toList else (for(tup <- l) yield new SearchedWebPage(tup._1,nf(tup._2))).toList
    }

    
    // Load a List of WebPage objects from the packaged prolandwiki.csv file
    def loadWebPages: List[WebPage] = {
        // create an input stream to the proglangwiki.csv
        val fh = Source.fromInputStream(
            getClass.getClassLoader.getResourceAsStream("proglangwiki.csv"))
        // load all pages from the file line by line
        val pages = (for (line <- fh.getLines) yield {
            val id::name::url::text::links = line.split(",").toList
            new WebPage(id, name, url, text.toLowerCase, links)
        }).toList
        fh.close
        pages
    }

    // Convert a List[WebPage] to a Map[String, WebPage]
    def mapWebPages(pages: List[WebPage]): Map[String, WebPage] = {
        (for (page <- pages) yield (page.id, page)).toMap
    }
}