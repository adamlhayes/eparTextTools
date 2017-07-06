
#' Cleans documents performing many common tasks .
#'
#' @param corpus corpus to clean
#' @return a cleaned tm corpus
#' @seealso \code{\link{tm_map}} 
#' @export
#' @description  This function strips whitespace, removes stop words, removes punctuation and special characters, and stems documents. It can also merge synonyms.
#' @param synonymList A list of character vectors. For each vector, the first word is the word that the other words in the vector will be transformed into.
#' @param exact A boolean vector with length equal to number of synonyms. True (or 1) indicates the system should use an exact match, False (or 0) matches on the stemmed word.
#' @examples
#' doc_clean_process(corpus1)
#' synonyms <- list(c("monopoly","market power", "market concentration"),
                    c("monopoly","monopolize", "monpolies"),
                    c("tuberculosis", "t.b."), 
                    c("need", "want", "require"))
#' exact <- (T,F,T,F)             
#' doc_clean_process(corpus1,synonymList=synonyms,exact=exact)

doc_clean_process<-function(corpusname,synonymList=NULL,exact=NULL){
  
  corpus.tmp <- corpusname
    
    if(length(synonymList) != length(exact)){
      print("Error: Synonym list should match up with vector that indicates whether to return an exact match")
    next}
    
    for(i in which(as.numeric(exact)==1)){
      
      #Unpack the list of lists
      synonymSet <- synonymList[[i]]
      #define the main word and its synonyms
      main <- tolower(synonymSet[1])
      synonyms <- gsub("\\.","\\\\.", tolower(paste(synonymSet[2:length(synonymSet)], collapse = '|')) )
      synonymize <- content_transformer(function(x) gsub(synonyms,main,tolower(x)))
      
      corpus.tmp <- tm::tm_map(corpus.tmp,synonymize)
    }
  
  removeSpecialChars <- content_transformer(function(x) gsub("[^a-zA-Z&-\\.]"," ",x))
  stopWords <- function(x) tm::removeWords(x, tm::stopwords("en"))
  
  funs <- c(removeSpecialChars, 
            tm::stripWhitespace,
            stopWords,
            tm::removeNumbers,
            tm::removePunctuation,
            tm::stemDocument,
            tm::content_transformer(tolower))
  corpus.tmp<-tm::tm_map(corpus.tmp, FUN = tm::tm_reduce, tmFuns = funs, mc.cores=1)
  
  for(i in which(as.numeric(exact)==0)){
    
    #Unpack the list of lists
    synonymSet <- synonymList[[i]]
    #define the main word and its synonyms
    main <- tolower(SnowballC::wordStem(synonymSet[1]))
    
    synonyms <- gsub("\\.","\\\\.", tolower(paste(wordStem(synonymSet[2:length(synonymSet)]), collapse = '|')) )
    synonymize <- content_transformer(function(x) gsub(synonyms,main,tolower(x)))
    
    corpus.tmp <- tm::tm_map(corpus.tmp,synonymize)
  }
  
  corpus2 <- corpus.tmp
  corpus2
}
