library(tidyr)
library(dplyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

COVID19_open_line_list <- read.csv("COVID19_open_line_list.csv")

df<-data.frame(COVID19_open_line_list$symptoms)
symp<-df %>% 
  separate_rows(COVID19_open_line_list.symptoms)

word.corpus<-Corpus(VectorSource(symp))
word.corpus<-word.corpus%>%
  tm_map(removePunctuation)%>% 
  tm_map(removeNumbers)%>% 
  tm_map(stripWhitespace)
word.corpus<-word.corpus%>%
  tm_map(tolower)%>%
  tm_map(removeWords, stopwords("english"))

word.corpus<-tm_map(word.corpus)

word.counts<-as.matrix(TermDocumentMatrix(word.corpus))
word.freq<-sort(rowSums(word.counts), decreasing=TRUE)
head(word.freq)

set.seed(32) 
par(bg = 'black')
wordcloud(words=names(word.freq), freq=word.freq,rot.per=0.35,
          max.words = 200, random.order = FALSE,colors=brewer.pal(8,"Paired"))

par(bg = 'white')
barplot(word.freq, las = 2,
        col ="lightblue", main ="Symptoms of COVID19",
        ylab = "Symptom frequency")


#Wordcloud2 plot
require(devtools)
install_github("lchiffon/wordcloud2")
library(wordcloud2)
corpus <- VCorpus(VectorSource(symp))
dtm <- DocumentTermMatrix(corpus)
freq <- colSums(as.matrix(dtm))
freq <- data.frame(names(freq), count = as.numeric(freq))
freq <- freq[order(-freq$count),]
head(freq)
df1 <- as.data.frame(freq)
wcloud2 <- wordcloud2(data = df1, size = 1)
wcloud2


