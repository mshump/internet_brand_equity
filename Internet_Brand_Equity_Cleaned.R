##################
## Enhancements ##
##################

## consider cleaning dictionaries and text
## consider dealing with negation words i.e. 'not'
## consider more cleaning of text
## consider steming of text
## currently scoring -1,0,1, can change to -N,0,N by count matches
## consider basic visuals like word clouds?



####################
## Load Libraries ##
####################
#install.packages("RCurl")
#install.packages("XML")
#install.packages("RTextTools")
#install.packages("e1071")
#install.packages("plyr")
#install.packages("stringr")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("data.table")
#install.pakcages("RODBC")


library(RCurl)
library(XML)
library(RTextTools)
library(e1071)
library(plyr)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(data.table)
#library(RODBC) 



###########################
## INSTANTIATE FUNCTIONS ##
###########################


#######################################
## FUNCITON: Get URLS and clean them ##
#######################################


getGoogleURL <- function(search.term,nlinks, domain = '.com', quotes=TRUE) {
  search.term <- gsub(' ', '%20', search.term)
  if(quotes) search.term <- paste('%22', search.term, '%22', sep='') 
  getGoogleURL <- paste('http://www.google', domain, '/search?q=',search.term,"&num=", nlinks, sep='')
}

#options(browser="C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
# the SSL load fixes the Cigna web block
getGoogleLinks <- function(google.url) {
  #download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm") #SSL
  #doc <- getURL(google.url, httpheader = c("User-Agent" = "chome"),followlocation = TRUE, cainfo = "cacert.perm") #SSL
  doc <- getURL(google.url, httpheader = c("User-Agent" = "chome"),followlocation = TRUE)
  html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function (...){})
  nodes <- getNodeSet(html, "//h3[@class='r']//a") ### data-href
  return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))
}


clean.google.urls <- function(url_in) {
  left.border  <- "http"
  right.border <- "&sa"
  pattern <- paste0("(?<=", left.border, ").+(?=", right.border, ")")
  rx <- regexpr(pattern, text=url_in, perl=TRUE)
  clean_link <- paste0(left.border,substring(url_in, rx, rx+attr(rx, "match.length")-1))
  return(clean_link) 
}

##############################
## FUNCITON: Text from HTML ##
##############################
# the SSL load fixes the Cigna web block
htmlToText <- function(urlin) {
  #download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
  #html <- getURL(urlin, followlocation = TRUE, cainfo = "cacert.perm") SSL
  html <- getURL(urlin, followlocation = TRUE)
  doc <- htmlParse(html, asText = TRUE) 
  htmlastext <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)

  return(htmlastext)
}


##########################
## FUNCITON: Clean Text ##
##########################
# this can be improved and simplified, but works for the most part
# can probably condense down to a few REGEXs 
# or there is a more elquent function online
clean_text <- function(data_in){
  tmptxt <- data_in
  tmptxt <- gsub("^\\s+|\\s+$", "",tmptxt)
  tmptxt <- gsub("\n", "", tmptxt)
  tmptxt <- gsub("\t", "", tmptxt)
  tmptxt <- gsub("\r", "", tmptxt)
  tmptxt <- gsub("<.*?>", "", tmptxt)
  tmptxt <- gsub("/","",tmptxt)
  tmptxt <- gsub("\\*","",tmptxt)
  tmptxt <- gsub("[\r\n]", "", tmptxt)
  #tmptxt <- gsub("^ *|(?<= ) | *$", "",tmptxt, perl=T)
  tmptxt <- gsub("^\\s+|\\s+$", "", tmptxt)
  tmptxt <- noquote(tmptxt)
  return(tmptxt)   
}

############################
## FUNCITON: Clean Corpus ##
############################

text_list_clean <- function(text_corp_in){
  txt_tmp <- Corpus(VectorSource(text_corp_in))
  #txt_tmp <- text_corp_in
  #dropList <- c("oh","uh","hmmm", "mmmm", " re ","huh", "um", "m")
  txt_tmp <- tm_map(txt_tmp, tolower)
  #txt1 <- tm_map(txt1, stemDocument, language = "english")
  txt_tmp <- tm_map(txt_tmp, removePunctuation)
  txt_tmp <- tm_map(txt_tmp, removeWords, stopwords("english"))  #or: stopwords(stopList) #if using custom list
  #txt_tmp <- tm_map(txt_tmp, removeWords, dropList)
  txt_tmp <- tm_map(txt_tmp, removeNumbers)
  txt_tmp <- tm_map(txt_tmp, stripWhitespace)
  #inspect(txt_tmp) 
  txt_df <- data.frame(text=unlist(txt_tmp), stringsAsFactors=FALSE)
  txt_ls <- as.list(txt_df)$text
  #txt_ls <- NULL
  return(txt_ls)
}

########################################
## FUNCITON: Score Polarity (Neg,Pos) ##
########################################

# currently scoring -1,0,1, can change to -N,0,N by count matches

# not currently addressing "negatives"
# negateive i.e. "not" consider condensing to not-word and keeping hyphens?

score.polarity = function(sentences, pos.words, neg.words, .progress='none')
{
  #require(plyr)
  #require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # do a bit more cleaning
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    # standardize to -1,0,1
    score = ifelse(score > 0,1, ifelse(score < 0,-1,0 ) )
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#####################################
#           Code Execution          #
#####################################

#######################
## Load Dictionaries ##
#######################
# some dictionaries : http://stackoverflow.com/questions/4188706/sentiment-analysis-dictionaries
# read in topic dictionaries
options(stringsAsFactors=FALSE)
setwd("C:/Users/mshump/Documents/Projects/IBE/Dictionaries/") 
dict <- read.csv("Lexicon_bag.csv",header=TRUE, sep="," , stringsAsFactors=FALSE)
#str(dict)

dict <- data.frame(lapply(dict, function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
}))

#str(dict)

#themes <- unique(dict$Theme)
topics <- unique(dict$Topic)
theme_topic <- t(as.data.frame(strsplit(unique(paste(dict$Theme, dict$Topic, sep=",")), ",")))
theme_topic <- data.frame(theme_topic, row.names=NULL)
names(theme_topic) <- c("theme", "topic")

theme_topic$theme_id <- with(theme_topic, ave(theme,
                       FUN = function(x) cumsum(!duplicated(x))))
theme_topic$topic_id <- with(theme_topic, ave(topic,
                                              FUN = function(x) cumsum(!duplicated(x))))


# Make a function to auto generate dictionary objects by # of topics
# Subset dictionaries by topic and polarity 
pos_list <- list()
neg_list <- list()

for( i in topics ){
  
  #i <- 'availability'
  pos_list[i] <- list( '1'=dict[dict$Topic == i & dict$polarity == 'positive' ,  c("word")] )
  neg_list[i] <- list( '1'=dict[dict$Topic == i & dict$polarity == 'negative' ,  c("word")] )
  
  #str(pos_list)
  
  #message(i)
  
}


# read in sentiment dictionaries by polarity
# These are from: https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
pos_sent <- readLines(url("https://raw.githubusercontent.com/dipanjanS/MyShinyApps/master/twitter-analysis/positive_words.txt"))
neg_sent <- readLines(url("https://raw.githubusercontent.com/dipanjanS/MyShinyApps/master/twitter-analysis/negative_words.txt"))

#pos_sent <- posterms_GI()
#neg_sent <- negterms_GI()

###############################
# STEP 3: Get URLS via google #
###############################
# first 10 results only 

brand_table <- read.csv("brand_lookups.csv",header=TRUE, sep="," , stringsAsFactors=FALSE)

brand_list <- brand_table$brand_name
nlinks <- 20

for( j in 1:length(brand_list)){

#j<-1  
message(paste("start brand", brand_list[j], "(", j , ")", sep=" ") )
  
brand_name <- brand_list[j]
brand_id <-  j
dt_collected <- Sys.Date()

# serach term
search.term <- paste(brand_list[j], "news", sep=" ")
quotes <- "FALSE"

# create google search string
search.url <- getGoogleURL(search.term=search.term, nlinks=nlinks, quotes=quotes)
# get google links from search page
links <- getGoogleLinks(search.url)

#clean google links for use
#clean.google.urls(unlist(links[3]))
clean_links <- lapply(links, clean.google.urls)



##################################
# STEP 2: Extract text from html #
##################################

# can add an apply function here -however, may be too much data in memory
# might want to apply text extraction after cleansing for each site 
# then save the features / scores per site, and then step to the next site
# will be less" efficient" but will reduce overhead of storing each site's text
# in memore for R

# convert html code to plain text (i.e. remove tags etc.)
#clean_links[[1]] <- NULL 

#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm") SSL
text<-NULL
text <- lapply(clean_links, function(x) try(htmlToText(unlist(x)), FALSE))

#str(text)
#text <- htmlToText(unlist(clean_links[4]))

#############################
# STEP 3: clean / structure #
#############################

# additional text cleaining maintaining vector struc
text_clean <- clean_text(text) 
# collapse into a single doc if desired
#collapse_txt <- paste(clean_txt, collapse = " ") 
#text <- gsub('\\"','', text)
# split on sentences
text_clean <- unlist(strsplit(text_clean,"\\."))
# remove elements <= 3 characters
#text_clean <- gsub("[\r\n]", "", text_clean)
text_clean <- text_clean[nchar(text_clean) > 10]

# clean text for analysis
text_clean <- text_list_clean(text_clean)
text_clean <- text_clean[nchar(text_clean) > 10]

#text_clean[20:30]
#str(text_clean)
#######################
##    Score Text     ##
#######################
#score_topic <- score.polarity(sent_in, pos_topic, neg_topic)
#score_sent <- score.polarity(sent_in, pos_sent, neg_sent)

score_sent <- score.polarity(text_clean, pos_sent, neg_sent)
#str(score_sent)

# a warning gets thrown here - but stillw orks
score_topic <- vector("list", length(topics))

for( i in 1:length(topics) ){

  score_topic[i] <- score.polarity(sentence=text_clean, pos.words= unlist(pos_list[i], use.names=FALSE), neg.words= unlist(neg_list[i], use.names=FALSE))
  
}

#str(score_topic)


tmp0 <- lapply(topics, function(x) cbind(row.names(score_sent),rep(x, nrow(score_sent))))
tmp1 <- lapply(lapply(score_topic, data.frame), function(x) cbind(x,data.frame(score_sent$score)))
tmp2<-cbind(ldply(tmp0, data.frame) , ldply(tmp1, data.frame))
names(tmp2) <- c('sentence_id','topic','topic_score', 'sent_score')
tmp2$topic_net_score  <- ifelse(tmp2$topic_score == 0,0, ifelse(  tmp2$sent_score == -1,-1,tmp2$topic_score) )

tmp2$brand <- rep(brand_name, nrow(tmp2))
tmp2$brand_id <- rep(brand_id, nrow(tmp2))
tmp2$dt_collected <- as.character(rep(dt_collected, nrow(tmp2)))


txt_scores <- merge(tmp2,theme_topic)

txt_scores <- txt_scores[c("dt_collected","brand_id", "brand"
                          ,"theme_id","theme"
                          ,"topic_id","topic"
                          ,"sentence_id"
                          ,"topic_score"
                          ,"sent_score"
                          ,"topic_net_score")]

#str(txt_scores)
#head(txt_scores)

link_table <- data.frame(unlist(clean_links))
names(link_table) <- c("link_name")
link_table$link_id <- row.names(link_table)
link_table$brand <- brand_name
link_table$brand_id <- brand_id
link_table$dt_collected <- as.character(dt_collected)


message("done with scores")

setwd("C:/Users/mshump/Documents/Projects/IBE/outputs")

score_fn <- paste0("scores_fn_",dt_collected,".csv")
link_fn  <- paste0("links_fn_",dt_collected,".csv")

# scores
if(j==1){
  message("make score file")
  fwrite(txt_scores, score_fn)
}else{
  message("append to score file")
  fwrite(txt_scores, score_fn, append=TRUE)
}

# links
if(j==1){
  message("make link file")
  fwrite(link_table, link_fn)
}else{
  message("append to link file")
  fwrite(link_table, link_fn, append=TRUE)
}



#odbcClose(conn1)

message("written to table")
}


message("done with brands")

message("done with IBE code")
