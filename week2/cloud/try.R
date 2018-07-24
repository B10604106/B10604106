
library(bitops)
library(httr)
library(RCurl)
library(XML)
library(tm)
library(NLP)
library(tmcn)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
library(xml2)
library(tmcn)
library(rvest)


library(xml2)
library(tmcn)
library(rvest)

from <- 4465 # 2018-06-01
to <- 4472 # 2018-06-30
prefix = "https://www.ptt.cc/bbs/Stock/index"

data <- list()
for( id in c(from:to) )
{
  url <- paste0( prefix, as.character(id), ".html" )
  html <- htmlParse( GET(url) )
  url.list <- xpathSApply( html, "//div[@class='title']/a[@href]", xmlAttrs )
  data <- rbind( data, as.matrix(paste('https://www.ptt.cc', url.list, sep='')) )
}
data <- unlist(data)

head(data)

library(dplyr)
getdoc <- function(url)
{
  html <- htmlParse( getURL(url))
  doc <- xpathSApply( html, "//div[@id='main-content']", xmlValue )
  time <- xpathSApply( html, "//*[@id='main-content']/div[4]/span[2]", xmlValue )
  temp <- gsub(" "," 0", unlist(time) )
  part <- strsplit( temp, split=" ", fixed=T )
  #date <- paste(part[[1]][2], part[[1]][3], part[[1]][5], sep="-")
  #date <- paste(part[[1]][2], part[[1]][5], sep="_")
  #date <- paste(part[[1]][1], part[[1]][2], sep="_")
  timestamp <- part[[1]][4]
  timestamp <- strsplit( timestamp, split=":", fixed=T )
  hour <- timestamp[[1]][1]
  #print(hour)
  name <- paste0('./DATA/', hour, ".txt")
  write(doc, name, append = TRUE)
}

sapply(data, getdoc)


library("tm")
rm(list=ls(all.names = TRUE))
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)

#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
d.corpus <- Corpus( DirSource("ptt") )
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})
d.corpus <- tm_map(d.corpus, toSpace, "※")
d.corpus <- tm_map(d.corpus, toSpace, "◆")
d.corpus <- tm_map(d.corpus, toSpace, "‧")
d.corpus <- tm_map(d.corpus, toSpace, "的")
d.corpus <- tm_map(d.corpus, toSpace, "我")
d.corpus <- tm_map(d.corpus, toSpace, "是")
d.corpus <- tm_map(d.corpus, toSpace, "看板")
d.corpus <- tm_map(d.corpus, toSpace, "作者")
dd.corpus <- tm_map(d.corpus, toSpace, "發信站")
d.corpus <- tm_map(d.corpus, toSpace, "批踢踢實業坊")
d.corpus <- tm_map(d.corpus, toSpace, "一")
d.corpus <- tm_map(d.corpus, toSpace, "二")
d.corpus <- tm_map(d.corpus, toSpace, "台")
d.corpus <- tm_map(d.corpus, toSpace, "了")
d.corpus <- tm_map(d.corpus, toSpace, "變")
d.corpus <- tm_map(d.corpus, toSpace, "下")
d.corpus <- tm_map(d.corpus, toSpace, "不會")
d.corpus <- tm_map(d.corpus, toSpace, "不")
d.corpus <- tm_map(d.corpus, toSpace, "巴")
d.corpus <- tm_map(d.corpus, toSpace, "文章")
d.corpus <- tm_map(d.corpus, toSpace, "網址")
d.corpus <- tm_map(d.corpus, toSpace, "標題")
d.corpus <- tm_map(d.corpus, toSpace, "發信站")
d.corpus <- tm_map(d.corpus, toSpace, "[a-zA-Z]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, stripWhitespace)
d.corpus

mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

n = length(seg)
TDM = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0
library(knitr)
kable(head(TDM))

kable(tail(TDM))

tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum)

library(Matrix)
idfCal <- function(word_doc)
{ 
  log2( n / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)

doc.tfidf <- TDM
# for(x in 1:nrow(TDM))
#{
# for(y in 2:ncol(TDM))
#{
# doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
# }
#}

tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX

stopLine = rowSums(doc.tfidf[,2:(n+1)])
delID = which(stopLine == 0)

kable(head(doc.tfidf[delID,1]))

kable(tail(doc.tfidf[delID,1]))

TDM = TDM[-delID,]
doc.tfidf = doc.tfidf[-delID,]

TopWords = data.frame()
for( id in c(1:n) )
{
  dayMax = order(doc.tfidf[,id+1], decreasing = TRUE)
  showResult = t(as.data.frame(doc.tfidf[dayMax[1:5],1]))
  TopWords = rbind(TopWords, showResult)
}
rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
TopWords = droplevels(TopWords)
kable(TopWords)

TDM$d = as.character(TDM$d)
AllTop = as.data.frame( table(as.matrix(TopWords)) )
AllTop = AllTop[order(AllTop$Freq, decreasing = TRUE),]

kable(head(AllTop))

TopNo = 5
tempGraph = data.frame()
for( t in c(1:TopNo) )
{
  word = matrix( rep(c(as.matrix(AllTop$Var1[t])), each = n), nrow = n )
  temp = cbind( colnames(doc.tfidf)[2:(n+1)], t(TDM[which(TDM$d == AllTop$Var1[t]), 2:(n+1)]), word )
  colnames(temp) = c("hour", "freq", "words")
  tempGraph = rbind(tempGraph, temp)
  names(tempGraph) = c("hour", "freq", "words")
}

library(ggplot2)

library(varhandle)
tempGraph$freq = unfactor(tempGraph$freq)
ggplot(tempGraph, aes(hour, freq)) + 
  geom_point(aes(color = words, shape = words), size = 5) +
  geom_line(aes(group = words, linetype = words))

kable(tail(AllTop))

filenames = as.array(paste0("./ptt/",colnames(doc.tfidf)[2:(n+1)],".txt"))
sizeResult = apply(filenames, 1, file.size) / 1024
showSize = data.frame(colnames(doc.tfidf)[2:(n+1)], sizeResult)
names(showSize) = c("hour", "size_KB")
library(ggplot2)
ggplot(showSize, aes(x = hour, y = size_KB)) + geom_bar(stat="identity")
