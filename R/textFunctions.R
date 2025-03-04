
expandJava<-function(){options(java.parameters = "-Xmx6000m")}

#list.of.packages <- c("ggplot2", "Rcpp","tm","ggthemes","SnowballC","rvest","downloader","DT","wordcloud","d3heatmap","plyr","reshape2","dplyr","qdapTools","stringr","openNLP","NLP","stm","LDAvis","servr","Rtsne","geometry","downloader","corrplot","pryr","openNLPmodels.en","lubridate","pbapply","devtools","tm.plugin.mail","plotly","data.table","igraph","networkD3")

#if("StanfordCoreNLP"%in%c(installed.packages()[,"Package"])==FALSE){install.packages('StanfordCoreNLP',repos="http://datacube.wu.ac.at/",type="source")}
#if("openNLPmodels.en"%in%c(installed.packages()[,"Package"])==FALSE){install.packages('openNLPmodels.en',repos="http://datacube.wu.ac.at/",type="source")}

#packages.Req <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#if(length(packages.Req)) install.packages(packages.Req)

#lapply(list.of.packages, function(X) library(X,character=TRUE))

#package depends on xpdf... on mac, install via brew install xpdf and follow instructions.

demandwordvec<-function(){
  try({if (!require(wordVectors)) {
  if (!(require(devtools))) {
    install.packages("devtools")
  }
  devtools::install_github("bmschmidt/wordVectors")
}})
}


#' Build Example Data.
#'
#' @return a folder of documents
#' @export
#' @description  This function creates a folder if documents in the working directory.
#' @examples
#' example_documents()
example_documents<-function(){
 data(demo.docs)
if(file.exists("demo.docs.folder")==FALSE){dir.create("demo.docs.folder")}
demo.docs2<-demo.docs[which(file.exists(file.path("demo.docs.folder",sapply(demo.docs$links, function(X){tail(unlist(strsplit(X,split="/",fixed=TRUE)),n=1)})))==FALSE),]
lapply(demo.docs2$links, function(X){try(download.file(X, destfile=file.path("demo.docs.folder",tail(unlist(strsplit(X,split="/",fixed=TRUE)),n=1)),mode="wb"))})
}

#' Check to see if Tika is available.
#'
#' @param directory directory to look for tika in. 
#' @return If false will download tika to working directory.
#' @seealso \code{\link{http://apache.claz.org/tika}} 
#' @export
#' @description The check for tika command will install tika to the current working directory.
#' @examples
#' checkForTika()
checkForTika<-function(directory=getwd()){if("tika-app-1.13.jar"%in%list.files(path=directory)) {cat("success")} else {download.file("http://apache.claz.org/tika/tika-app-1.13.jar",file.path(directory,"tika-app-1.13.jar"))}}
#' Loads and processes doc,docx,pdf,and txt files into tm corpus.
#'
#'This command loads files into R from a directory into a corpus. Currently it reads doc, docx, pdf, and txt files.
#' @param fname Directory name
#' @param tika Should tika be used? 
#' @param gen_pdf_tools if using a pc, set to true. if on linux, setting to false and installing the pdf2text library will be more useful. See readPDF in the tm package for more info.
#' @param tikapath Path to the Tika application
#' @return TM Text Corpus.
#' @seealso \code{\link{tm::corpus}} 
#' @export
#' @examples
#' corpus1<-getTextR(file.path())
getTextR<-function(fname,tika=FALSE,gen_pdf_tools=T,tikapath="tika-app-1.13.jar"){
  if(tika==TRUE){
    pdoc<-system(command=paste("java -jar",tikapath,"-t",gsub(" ","\\ ",fname,fixed=TRUE)),intern=TRUE,wait=TRUE)
  } else {
    pdoc<-if(stringr::str_detect(fname,".docx+$")==TRUE){read_docxtm(fname)} else {
      if(stringr::str_detect(fname,".doc+$")==TRUE){tm::readDOC()(language="en",elem=list(uri=fname))} else {pdoc<-if(stringr::str_detect(fname, stringr::fixed(".pdf"))==TRUE){
        if(gen_pdf_tools==F){readPDF2(engine="xpdf")(elem=list(uri=fname), language="en")} else {readPDF2(engine="pdftools")(elem=list(uri=fname), language="en")}} else {if(stringr::str_detect(fname,stringr::fixed(".txt"))==TRUE){tm::readPlain(elem=list(uri=fname,content=iconv(enc2utf8(readLines(fname)), sub = "byte")),language="en")} else {"FILETYPE NA"}}}}}
  pdoc
}

#' Read Emails into Corpus.
#'
#' @param folder_in The path to the folder where the emails are located
#' @param newmailsdirectory The path to the folder where the processed emails will be stored.
#' @return A tm Corpus of processed emails.
#' @seealso \code{\link{http://www.matijs.net/software/msgconv/}} 
#' @export
#' @description  A tm Corpus of processed emails which can be combined with a corpus created by tm using the c() command. The command calls to msgconvert, and will only work if you have msgconvert installed.
#' @examples
#' readMails("../doc","../procdocs")
readMails<-function(folder_in,newmailsdirectory){
  dir.create(newmailsdirectory)
  messages<-list.files(folder_in,full.names=TRUE)[str_detect(tools::file_ext(list.files(folder_in)),"msg")]
  plyr::l_ply(messages,function(X) system(paste(paste("msgconvert --mbox",file.path(folder_in,"msgs.mbox",sep=""),X))))
  file.remove(messages)
  mbases<-list.files(folder_in)[tools::file_ext(list.files(folder_in))%in%"mbox"==FALSE]
  file.copy(file.path(folder_in,"msgs.mbox"),file.path(newmailsdirectory,"msgs.mbox"))
  mails<-MBoxSource(file.path(newmailsdirectory,"msgs.mbox"))
  file.remove(file.path(folder_in,"msgs.mbox"))
  mailsc<-Corpus(mails)
  mailsc}

#' Tests if save directory exists and creates new one if not
#'
#' @param dir directory
#' @export
#' @description  This function does the quite useful task of making a workingfolder object in the environment.
makeworking<-function(dir){if(dir.exists(dir)==FALSE) {dir.create(dir)} else {cat("Good News, Directory Already Exists Containing:",paste(list.files("Research.Grants"),collapse="\n"))}
  workingfolder<<-dir}


#' Creates directory and copies from old to new.
#'
#' @param from Old directory
#' @param to New directory
#' @return NA, but he file will be relocated
#' @seealso \code{\link{file.copy}} 
#' @export
#' @description  This function is just a basic wrapper to copy and move folders recursively in linux. Should probably just use file.copy instead.
#' @examples
#' copyDir("../doc","../newdocs")
copyDir<-function(from,to){
  dir.create(to)
  system(paste("cp -r", from, to))}


#' Reads docx files into tm corpus.
#'
#' @param file File to read. Should end in docx
#' @param skip Value is settable for debugging. Skips lines. Set at 0 for most applications.
#' @return a plain text document corpus
#' @seealso \code{\link{PlainTextDocument}} 
#' @export
#' @description  This function reads a docx file into a tm corpus.
#' @examples
#' read_docxtm("demodoc.docx",skip=0)
read_docxtm<-function (file, skip = 0) {
  tmp <- tempfile()
  if (!dir.create(tmp)) 
    stop("Temporary directory could not be established.")
  utils::unzip(file, exdir = tmp)
  xmlfile <- file.path(tmp, "word", "document.xml")
  doc <- XML::xmlTreeParse(xmlfile, useInternalNodes = TRUE)
  xml_metadata<-read_xml(file.path(tmp, "docProps", "core.xml")) 
  unlink(tmp, recursive = TRUE)
  nodeSet <- XML::getNodeSet(doc, "//w:p")
  pvalues <- sapply(nodeSet, XML::xmlValue)
  pvalues <- pvalues[pvalues != ""]
  if (skip > 0) 
    pvalues <- pvalues[-seq(skip)]
  tempdes<-xml_text(xml_metadata)
  xml_metadata<-xml_metadata %>% as_list() 
  PlainTextDocument(x=pvalues,author=xml_metadata$creator[[1]],description=tempdes,datetimestamp=xml_metadata$modified[[1]], id=file)}



#' Worker function for processing URI info. From tm package.
#'
#' @param uri the path to the uri to be processed.
#' @return uri
#' @seealso \code{\link{tm}} 
#' @export
#' @description  Worker function for processing URI info. From tm package.
processURI2 <-function(uri) {
  uri <- as.character(uri)
  if (identical(substr(uri, 1, 7), "file://"))
    uri <- substr(uri, 8, nchar(uri))
  uri
}


#' Function call to read pdf files into tm corpus.
#'
#' @param engine which pdf library to use. see tm readPDF for more details
#' @return engine for reading pdf
#' @seealso \code{\link{readPDF}} 
#' @export
#' @description  This function is a port of readpdf.
#' @examples
#' readPDF2(engine="xpdf")(elem=list(uri=fname), language="en")
readPDF2<-function (engine = c("xpdf", "Rpoppler", "ghostscript", "Rcampdf","custom","pdftools"), control = list(info = NULL, text = NULL)) {
  stopifnot(is.character(engine), is.list(control))
  engine <- match.arg(engine)
  pdf_info <- switch(engine, xpdf = function(x) tm:::pdf_info_via_xpdf(x,control$info), Rpoppler = Rpoppler::PDF_info, pdftools=pdftools::pdf_info, ghostscript = tm:::pdf_info_via_gs,Rcampdf = Rcampdf::pdf_info, custom = control$info)
  pdf_text <- switch(engine, xpdf = function(x) system2("pdftotext", c(control$text, shQuote(x), "-"), stdout = TRUE), Rpoppler = Rpoppler::PDF_text, pdftools=pdftools::pdf_text, ghostscript = pdf_text_via_gs, Rcampdf = Rcampdf::pdf_text, custom = control$text)
  if (!is.function(pdf_info) || !is.function(pdf_text)) 
    stop("invalid function for PDF extraction")
  function(elem, language, id) {
    uri <- processURI2(elem$uri)
    meta <- pdf_info(uri)
    content <- pdf_text(uri)
    content<-iconv(enc2utf8(content), sub = "byte")
    tm::PlainTextDocument(content, meta$Author, meta$CreationDate, 
                          meta$Subject, meta$Title, basename(elem$uri), language, 
                          meta$Creator)
  }
}


#' Look up how to open a file. 
#'
#' @param ending the ending of the file in question.
#' @export
how_do_i_open<-function(ending){
  browseURL(paste("http://lmgtfy.com/?q=how+do+i+open+a+",ending,"+file+in+R",sep=""))
}

#' Look up how to do something in R
#'
#' @param theproblem the problem.
#' @description  A joke command to look up a method in R.
#' @export
how_do_i<-function(theproblem){
  browseURL(paste("http://lmgtfy.com/?q=how+do+i+",theproblem,"+in+R",sep=""))
}


#' Calls getTextR on all files in a directory joining into corpus. 
#'
#' @param directory Folder to read files from'
#' @param onError if skip skip documents that read wrong.
#' @param gen_pdf_tools if using a pc set to true. if on a unix server, setting to false will preserve pdf metadata.
#' @return Corpus of text documents.
#' @seealso \code{\link{corpus}} 
#' @export
#' @description  This function will read from a folder documents of the class pdf, docx, doc or txt.
#' @examples
#' allDocs("folder")
allDocs<-function (directory, SkiponError = FALSE,gen_pdf_tools=TRUE) {
  if(SkiponError==TRUE){
    temp <- lapply(file.path(directory, list.files(directory)), 
                   function(FILENAME) {
                     try(getTextR(FILENAME, gen_pdf_tools=gen_pdf_tools))
                   })
    temp <- temp[lapply(temp, class) != "character"]
    temp <- temp[lapply(temp, class) != "error"]
    temp <- temp[lapply(temp, is.null) != TRUE]
    temp <- temp[lapply(temp,class)!="try-error"]
    do.call(c, temp)
  } else {
    do.call(c, lapply(file.path(directory, list.files(directory)),getTextR))
  }
}
#' Cleans documents performing many common tasks .
#'
#' @param corpus corpus to clean
#' @return a cleaned tm corpus
#' @seealso \code{\link{tm_map}} 
#' @export
#' @description  This function strips whitespace, removes stop words,removes punctuation,and stems documents.
#' @examples
#' doc_clean_process(corpus1)
doc_clean_process<-function(corpusname){
  stopWords <- function(x) tm::removeWords(x, tm::stopwords("en"))
  funs <- list(tm::stripWhitespace,
               stopWords,
               tm::removeNumbers,
               tm::removePunctuation,
               tm::stemDocument,
               tm::content_transformer(tolower))
  corpus2<-tm::tm_map(corpusname, FUN = tm::tm_reduce, tmFuns = funs, mc.cores=1)
  corpus2}

#' Makes a pretty word association table .
#'
#' @param assoctable rough word association table
#' @param corpus word corpus for table
#' @param ngram
#' @return a cleaned tm corpus
#' @seealso \code{\link{tm_map}} 
#' @export
#' @description  This function takes a findAssoc object and returns a datatable. assocPrettyOneStep is wraps this to allow everything in one step.
#' @examples
#' assocPTable(findAssoc(corpus1,wordlist,corrVal),corpus)
assocPTable<-function(assoctable,corpus,ngram=FALSE){
  #assoctable<-assoctable[sapply(assoctable,length)>0]
  dft<-do.call(rbind,lapply(1:length(assoctable),function(i){tryCatch({data.frame("Word"=names(assoctable)[i],"Match"=names(assoctable[[i]]),"Association"=c(assoctable[[i]]))},error=function(e){data.frame("Word"=names(assoctable)[i],"Match"="too few words","Association"=c(0))})}))
  dft$Word<-as.character(dft$Word)
  dft$Match<-as.character(dft$Match)
  #dft$Word[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])]<-stemCompletion(dft$Word[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])],dictionary=corpus,type="prevalent")
  dft$Match[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])]<-stemCompletion(dft$Match[dft$Word%in%c(names(assoctable)[sapply(assoctable,length)>0])],dictionary=corpus,type="prevalent")
  DT::datatable(data=dft,rownames=FALSE,filter="top")}

#' Makes a pretty word association table all in one step.
#'
#' @param wordlist list of words for association
#' @param termDocumentMatrix tdm of corpus
#' @param corpus the corpus the words are from
#' @param corrVal the correlation value to report at or above
#' @return a word association table
#' @seealso \code{\link{findAssoc}} 
#' @export
#' @description  This function takes a wordlist, tdm, coprus, and settable correlation value and returns a datatable. 
#' @examples
#' assocPrettyOneStep(c("research","find"),termDocumentMatrix(corpus),corpus1,.8)
assocPrettyOneStep<-function(wordlist,termDocumentMatrix,corpus,corrVal=.8){
  assocPTable(findAssocs(termDocumentMatrix,wordlist,corrVal),corpus)
}


#' Creates plots of word frequencies
#'
#' @param termDocumentMatrix tm TermDocumentMatrix object
#' @param typeplot 1=word on x, 2=variable on x
#' @param wordcount How many words to graph 
#' @param minfreq minimum number of times a word should occur to be reported
#' @param shortendoc Should document names be shortened?
#' @param allCorpus For plot type 1, would you like the plot for all documents combined?
#' @return a ggplot2 object.
#' @seealso \code{\link{ggplot}} 
#' @export
#' @description  This function creates bar graphs in ggplot. It can be customized by adding items to the returned object.
#' @examples
#' wfplots(TermDocumentMatrix(corpus1),typePlot=1,5,minfreq=5,shortendoc=T)
wfplots<-function(termDocumentMatrix,typePlot=1,wordcount,minfreq=5,shortendoc=FALSE, allCorpus=FALSE){
  mfcomframe<-data.frame(as.matrix(termDocumentMatrix[findFreqTerms(termDocumentMatrix, lowfreq=minfreq),]))
  mfcomframe<-mfcomframe[sort(rowSums(mfcomframe),index.return=TRUE,decreasing=TRUE)$ix[1:wordcount],]
  mfcomframe$word<-row.names(mfcomframe)
  mfcomframe$word<-factor(mfcomframe$word, levels = mfcomframe$word)
  mfcomframe<-reshape2::melt(mfcomframe,id=c("word"))
  if(shortendoc==TRUE){
    mfcomframe$variable<-as.character(mfcomframe$variable)
    mfcomframe$variable<-stringr::str_trunc(mfcomframe$variable,side="left",width=20,ellipsis="...")
  }
  if(typePlot==1){
    if(allCorpus==TRUE){
      plotout<-ggplot(mfcomframe)+geom_bar(aes(x=word,y=value),position="stack",stat="identity")+coord_flip()+ggthemes::theme_pander()+ylab("Frequency")+xlab("Word")
      } else {
    plotout<-ggplot(mfcomframe)+geom_bar(aes(x=word,y=value,fill=variable),position="stack",stat="identity")+coord_flip()+ggthemes::theme_pander()+ggthemes::scale_fill_tableau(name="Document")+ylab("Frequency")+xlab("Word")
    }} else {
    plotout<-ggplot(mfcomframe)+geom_bar(aes(x=variable,y=value,fill=word),position="stack",stat="identity")+coord_flip()+ggthemes::theme_pander()+ggthemes::scale_fill_tableau(name="Word",palette="tableau10")+ylab("Frequency")+xlab("Document")
    }
  plotout
}



#' TermDocumentMatrix to interactive heatmap.
#'
#' @param termDocumentMatrix tm TermDocumentMatrix object
#' @param wordcount How many words to graph 
#' @param minfreq minimum number of times a word should occur to be reported
#' @param pickwords list of words to be included in heatmap. Default is all words sorted by frequency.
#' @param col_labels document labels in order of tdm columns
#' @param dendrows draw a dendogram for rows 
#' @param dendcolumns draw a dendogram fo columns
#' @return a d3heatmap object.
#' @seealso \code{\link{d3heatmap}} 
#' @export
#' @description  This function creates a d3 interactive heatmap
#' @examples
#' word_heatmap(TermDocumentMatrix(corpus1),10)
word_heatmap<-function(termDocumentMatrix,wordcount,minfreq=2,col_labels=paste(1:ncol(termDocumentMatrix),stringr::str_sub(colnames(termDocumentMatrix),-10,-1)),dendrows=TRUE,dendcolumns=TRUE,pickwords=c()){
  if(length(pickwords)<=0){
  mfcomframe<-data.frame(as.matrix(termDocumentMatrix[findFreqTerms(termDocumentMatrix, lowfreq=minfreq),]))
  mfcomframe<-mfcomframe[sort(rowSums(mfcomframe),index.return=TRUE,decreasing=TRUE)$ix[1:wordcount],]
  } else { 
    mfcomframe<-data.frame(as.matrix(termDocumentMatrix[pickwords,]))
  }                           
  mfcomframe$word<-row.names(mfcomframe)
  mtcells<-as.matrix(termDocumentMatrix[mfcomframe$word,])
  d3heatmap::d3heatmap(termDocumentMatrix[mfcomframe$word,], scale="column",colors="Purples",Rowv=dendrows,Colv=dendcolumns, labCol=col_labels)}

#' Creates ggplot of wordcounts!
#'
#' @param termDocumentMatrix tm TermDocumentMatrix object
#' @param wordlist list of words
#' @param by.var can specify variable to group documents by
#' @param name plottable name of by.var 
#' @return a ggplot2 object
#' @seealso \code{\link{ggplot2}} 
#' @export
#' @description  This function ggplot of wordcounts
#' @examples
#' interest_plot("research",TermDocumentMatrix(corpus1),by.var=variable,"DocID")
interest_plot<-function(wordlist,termDocumentMatrix,by.var=NULL,byvarname=""){
  tempframe<-data.frame(as.matrix(termDocumentMatrix[wordlist,]))
  tempframe<-data.frame("Count"=rowSums(tempframe),"word"=row.names(tempframe))
  if(is.null(by.var)){
    ggplot(tempframe, aes(word, Count)) + geom_bar(fill="#8ebfad", position = "dodge", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+xlab("")+ylab("Frequency")}
  else {
    ggplot(tempframe, aes(word, Count, fill=by.var)) + geom_bar(position = "dodge", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+xlab("")+ylab("Frequency")+ggthemes::scale_fill_pander(name=as.character(byvarname))}
}


#' Creates ggplot of wordcounts by document
#'
#' @param termDocumentMatrix tm TermDocumentMatrix object
#' @param wordlist list of words
#' @return a ggplot2 object
#' @seealso \code{\link{ggplot2}} 
#' @export
#' @description  This function ggplot of wordcounts by document id.
#' @examples
#' interest_plot_bydoc("research",TermDocumentMatrix(corpus1))
interest_plot_bydoc<-function(wordlist,termDocumentMatrix){
  tempframe<-data.frame(as.matrix(termDocumentMatrix[wordlist,]))
  tempframe$word<-row.names(tempframe)
  tempframe<-reshape2::melt(tempframe,id=c("word"))
  tempframe$variable<-as.character(tempframe$variable) %>% stringr::str_trunc(.,20,side="left")
  if(length(wordlist)>1){
    ggplot(tempframe, aes(variable, value, fill=word)) + geom_bar(position = "stack", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+xlab("")+ylab("Frequency")+ggthemes::scale_fill_pander()
  } else {
    ggplot(tempframe, aes(variable, value)) + geom_bar(fill="#8ebfad", position = "stack", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12), panel.background=element_blank())+xlab("")+ylab("Frequency")+ggthemes::scale_fill_pander()
  }}

#' Generates tornado plot
#'
#' @param termmatrix tm TermDocumentMatrix object
#' @param pickword a vector of words to select based on
#' @param frequency how often should the words occur in the document
#' @param nwords how many frequent words should be graphed?
#' @return a ggplot2 object
#' @seealso \code{\link{ggplot2}} 
#' @export
#' @description  This function makes a tornado plot comparing documents based on selected word frequencies. For example, can compare documents containing the word "gender" to all other documents
#' @examples
#' interest_plot_bydoc("research",TermDocumentMatrix(corpus1))
tornadoCompare<-function(termmatrix,pickword,frequency,nwords) {
  words_gender<-as.matrix(termmatrix[,as.vector(termmatrix[pickword,])>frequency]) %>% rowSums()
  words_gender<-words_gender/sum(words_gender)
  words_notgender<-as.matrix(tdm[,as.vector(termmatrix[pickword,])<=frequency]) %>% rowSums()
  words_notgender<-words_notgender/sum(words_notgender)
  bothgen<-cbind2(words_gender,words_notgender)
  bothgen<-as.TermDocumentMatrix(bothgen,weighting=weightTf) 
  bothgen$dimnames$Docs<-c("Chosen","Inverse")
  top10<-unique(c(sapply(1:2,function(X) sort(bothgen[,X]$v,decreasing=TRUE,index.return=T)$ix[1:nwords])))
  bothgen<-reshape2::melt(as.matrix(bothgen[top10,]))
  bothgen$value[bothgen$Docs=="Inverse"]<-bothgen$value[bothgen$Docs=="Inverse"]*-1
  ggplot(bothgen)+geom_bar(aes(x=Terms,y=value,fill=as.factor(Docs)),stat="identity",position=position_dodge(width=0.0))+coord_flip()+theme_minimal()+scale_fill_brewer("Document",palette="Dark2")+ylab("relative frequency")
}


#' Creates ggplot of wordcounts by document, allowing external characteristics.
#'
#' @param termDocumentMatrix tm TermDocumentMatrix object
#' @param wordlist list of words
#' @param doccharacteristic a vector the same length as tm specifying the trait you'd like to plot by
#' @return a ggplot2 object
#' @seealso \code{\link{ggplot2}} 
#' @export
#' @description  This function ggplot of wordcounts by document characteristic from an external list.
#' @examples
#' interest_plot_bydoc("research",TermDocumentMatrix(corpus1),rep("bacon",length(corpus1)))
interest_plot_bydoc_char<-function(wordlist,termDocumentMatrix,doccharacteristic){
  termDocumentMatrix$dimnames$Docs<-doccharacteristic
  tempframe<-data.frame(as.matrix(termDocumentMatrix[wordlist,]))
  tempframe$word<-row.names(tempframe)
  tempframe<-reshape2::melt(tempframe,id=c("word"))
  if(length(wordlist)>1){
    ggplot(tempframe, aes(variable, value, fill=word)) + geom_bar(position = "stack", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12),panel.background=element_blank())+xlab("")+ylab("Frequency")+ggthemes::scale_fill_pander()
  } else {
    ggplot(tempframe, aes(variable, value)) + geom_bar(fill="#8ebfad", position = "stack", stat="identity") + theme(axis.text.x=element_text(color="#000000",angle=50, hjust=1, size=12), panel.background=element_blank())+xlab("")+ylab("Frequency")+ggthemes::scale_fill_pander()
  }}


#' Suggests keywords based on the conceptnet API
#'
#' @param keyword word to search conceptnet for
#' @param path_root if true, can put in word path returned from previous calls otherwise defaults to keyword.
#' @export
#' @example suggestkeywords("polio") %>% suggestkeywords(.$word[3],path_root=T)
suggestkeywords<-function(keyword,path_root=F){
  cnetpath<-if(path_root==T){"http://api.conceptnet.io"} else {"http://api.conceptnet.io/c/en"}
  temp<-jsonlite::fromJSON(file.path(cnetpath,keyword))
  temp<-lapply(temp$edges$start$term, function(keyword1) {
    temp2<-jsonlite::fromJSON(file.path("http://api.conceptnet.io",keyword1))
    data.frame("label"=temp2$edges$start$label,"term"=temp2$edges$start$term)})
  list("word"=c(c(sapply(temp, function(X) as.character(X$term)) %>% unlist(.) %>% unique()) %>% unique),"term"=c(c(sapply(temp, function(X) as.character(X$label)) %>% unlist(.) %>% unique())%>% unique))
}




#' Creates a table of word counts within a set of documents.
#'
#' @param termDocumentMatrix tm TermDocumentMatrix object
#' @param wordlist list of words
#' @param doccorpus original document corpus
#' @param trunc should filenames be truncated in output table
#' @param weighting passed to tm::TermDocumentMatrix (by default is weightTf)
#' @param raw if true then outputs a data.table if false then produces an htmltable allowing csv/excel export. the resulting table can be saved using the saveWidgets command in htmlwidgets.
#' @param onlywordlist if true, bins words, otherwise returns all unique matches
#' @return a data.table or DT datatable depending on if raw is set to true or false.
#' @description  This function is useful for looking at occurences of words within documents
#' @export
#' @examples
#' wordcount_table(c("gender","nutrition"),TermDocumentMatrix(clcorp),corpus1)
wordcount_table<-function(wordlist,termDocumentMatrix,doccorpus,trunc=FALSE,raw=FALSE,onlywordlist=F){
  truelist<-unique(unlist(sapply(wordlist, function(X) which(str_detect(termDocumentMatrix$dimnames$Terms,X)),USE.NAMES = FALSE)))
  tempframe<-data.frame(as.matrix(termDocumentMatrix[truelist,]))
  tempframe$word<-row.names(tempframe)
  tempframe<-reshape2::melt(tempframe,id=c("word"))
  tempframe$variable<-as.character(tempframe$variable)
  tempframe<-dplyr::filter(tempframe, value>0)
  tempframe<-tempframe[order(nchar(tempframe$word),tempframe$value,decreasing=c(F,T)),]
  sents<-adply(tempframe,.margins=1,.fun=function(X){
    sframe<-unlist(tokenizers::tokenize_sentences(paste(textreg::convert.tm.to.character(doccorpus[which(gsub("%",".",names(doccorpus),fixed=TRUE)==X[,2])]),collapse=" ")))
    sframe<-sframe[unique(which(str_detect(sframe, X[,1])))] %>% paste(str_trim(.),collapse="...   ")
    sframe
  })
  if(trunc==TRUE) {sents$variable<-sents$variable %>% stringr::str_trunc(.,20,side="left")}
  colnames(sents)<-c("Word","Document","Count","String")
  sents<-filter(sents,nchar(String)>0)
  if(onlywordlist==TRUE){
    sents<-lapply(wordlist,function(W){
      temp1<-filter(sents, str_detect(Word,W))
      temp1<-ddply(temp1,.(Document), summarize,Count=sum(Count),String=paste(String,collapse="...   "))
    temp1
  })
    names(sents)<-wordlist
    sents<-bind_rows(sents,.id="Word")
}
if(raw==TRUE){data.table::data.table(sents)} else {
  DT::datatable(sents, options = list(columnDefs = list(list(
    targets = 4,
    render = DT::JS(
      "function(data, type, row, meta) {",
      "return type === 'display' && data.length > 50 ?",
      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
      "}")
  ),list(
    targets = 2,
    render = DT::JS(
      "function(data, type, row, meta) {",
      "return type === 'display' && data.length > 20 ?",
      "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
      "}")
  )),
  dom = 'Bfrtip',
  buttons = c('csv', 'excel')), extensions = 'Buttons', callback = DT::JS('table.page(3).draw(false);'),filter="top")
  }}
