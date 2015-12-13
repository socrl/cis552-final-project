# cis552-final-project
Lingbin Cai, Kathy Chen, Socrates Li

====
Names & PennKeys
Kathleen Chen (katch)
Lingbin Cai (lingbinc)
Socrates Li (socrali)

====
Instructions
Run in command line: export LANG=C
      This step is necessary because some unicode may not be parsed properly (See http://stackoverflow.com/questions/5047626/matching-specific-unicode-char-in-haskell-regexp).

Compile with: ghc -o crawler Main.hs
      This step is necessary because ghci by default does not handle the delete key well, so we have to compile it as an executable

Run with: ./crawler
      Make sure to provide an absolute URL, such as http://www.cis.upenn.edu/index.php.

You can clean the directory with: rm *.o *.hi

To write the output to a file, provide the filename as an argument.
      The file will be truncated if it exists or created if it doesn’t exist.

====
Overview of files in project (in the order they are read)
            Main.hs - Provide the command line interface that gets user input for crawling, sends requests to Downloader and reroute the results to Postprocessor, and eventually outputs the ranked results.

            Downloader.hs - Schedule the downloading requests in BFS manner. When crawling a new page, (1) fetch the robots.txt file from the server if not yet attained, parse and store the crawling information; (2) schedule the requests following the rules set by robots.txt; (3) fetch content of the page, parse and store the ones that contain keywords as result; (4) get URLs contained in the page, put those into the queue and keep crawling the next one. All parsing are done through PageParser module.

            PageParser.hs - The function of PageParser is two-fold: (1) to process the webpage HTML document and (2) to parse the robots.txt file of the site that we are crawling. The HTML document parser leverages HandsomeSoup to extract links for further crawling and checks whether the input queries appear in the page. It extracts and returns only the text contents of the HTML file if any such queries are found. The robots.txt parser returns the allowed and disallowed URLs, along with a crawl-delay specification for the site prior to crawling.

            PostProcessor.hs - Post processing the results from the crawler, including calculating a ranking score and finding the best snippet to demonstrate for each page. Ranking considers the number of keywords, number of distinct keywords and proximity of keywords if there are multiple keywords.

            UrlUtils.hs - Helper module that process URLs, including getting the domain, file type, pattern matching relative URLs, etc.

            Parser.hs, ParserCombinators.hs, Queue.hs - Helper modules which we use to build the main components. (Parser.hs & ParserCombinators.hs are from the previous homeworks & class lectures.)

            Test.hs - Contains all HUnit test cases.

====
Additional dependencies
            Data.String.Utils - cabal install MissingH
            Text.HandsomeSoup, Text.XML.HXT.Core - cabal install HandsomeSoup
            Network.URL - cabal install url
            Text.Regex - cabal install regex-compat

====
Example
            Provide a starting URL.
            http://www.cis.upenn.edu
            Provide whitespace-delimited keywords you want to search for.
            research
            Provide the maximum number of pages to crawl.
            6
            Crawling...
            Crawled 6 pages.
            ***** http://www.cis.upenn.edu/index.php *****
            extension services transformation next awards july 20 2015 benjamin pierce receives honorary doctorate from chalmers university of technology march 13 2015 susan davidson elected chair of the computing research

            ***** http://www.seas.upenn.edu/media/feature-visualizing-the-future.php *****
            as a member of the faculty diversity committee at the school of engineering and applied science through his interactions with other faculty involved in shared leadership activities and research

            ***** http://www.science2034.org/technology/robotic-systems-in-2034/ *****
            world his work has been supported by the national science foundation office of naval research air force office of scientific research department of transportation and the defense advanced research

            ***** http://www.upenn.edu/pennnews/news/penn-s-grasp-lab-receives-55-million-fast-light-and-autonomous-flying-robots *****
            submit a story contact for the media faculty experts source sheets campus photographs filming photography guidelines publications penn news today daily news service penn current penn current express research

