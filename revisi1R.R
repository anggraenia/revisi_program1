#install.packages("gdata")
#install.packages("quanteda")
#install.packages("syuzhet")
#install.packages("tidytext")
#install.packages("text2vec")
#install.packages("glmnet")
#install.packages("klaR")
#install.packages("pROC")
#install.packages("qdap")
#install.packages("randomcoloR")

library(gdata)
library(wordcloud)
library(tm)
library(quanteda)
library(syuzhet)
library(SnowballC)
library(tidytext)
library(devtools)
library(text2vec)
library(glmnet)
library(e1071)
library(klaR)
library(pROC)
library(data.table)
library(magrittr)
library(qdap)
library(readr)
library(qdapTools)
library(qdapRegex)
library(randomcoloR)
library(class)
library(readxl)

library(dplyr)
library(caret)
library(stopwords)

df <- read_excel("C:/Users/anik/Documents/data/data.xlsx")
df$class <- as.factor(df$class)
table(df$class)
plot(df$class, col = c("green", "red"))

#ambil kata dasar dari file sesuai dengan data yg ada di en_stopwords.csv
stopwords <- read.csv("C:/Users/anik/Documents/data/en_stopwords.csv", header = FALSE)
stopwords <- as.character(stopwords$V1)
stopwords <- c(stopwords, stopwords())

corpus <- Corpus(VectorSource(df$deskripsi))
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(removePunctuation)) %>%
  tm_map(content_transformer(removeNumbers)) %>%
  tm_map(removeWords, stopwords) %>%
  tm_map(stripWhitespace)
#inspect(corpus.clean[1:10])

dtm <- DocumentTermMatrix(corpus.clean)
show(dtm)
set.seed(333)

df <- df[sample(nrow(df)), ]

glimpse(df)

df$class <- as.factor(df$class)
corpus <- Corpus(VectorSource(df$deskripsi))
dtm <- DocumentTermMatrix(corpus.clean)


df.train <- df[1:100,]
df.test <- df[101:200,]

dtm.train <- dtm[1:100,]
dtm.test <- dtm[101:200,]
inspect(dtm.train)
inspect(dtm.test)

#algoritma Naive Bayes
modelNB = naiveBayes(class~.,data=df.train)
prediksi = predict(modelNB,df.test)
hasil=confusionMatrix(table(prediksi,df.test$class))
hasil

dta <- TermDocumentMatrix(corpus.clean)
m <- as.matrix(dta)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 9)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

head(d, 9)
barplot(d[1:9,]$freq, las = 2, names.arg = d[1:9,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

dr<- read.csv("C:/Users/anik/Documents/data/final_data.csv", stringsAsFactors = FALSE)
factor(dr$class)
w=table(dr$class,paste(dr$penjualan,sep ="."))
w=table(dr$penjualan, dr$class)
class(w)
t=as.data.frame(w)
names(t)[1] = 'penjualan'
t
