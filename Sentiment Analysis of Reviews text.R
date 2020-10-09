install.packages("RCurl")
install.packages("httr")
install.packages("syuzhet")

library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)
library(randomForest)

################################### AWS ####################################

library(readxl)
AWS <- read_excel("AWS.xlsx")

reviews<-AWS
reviews$text<-paste(reviews$Review,reviews$`Descriptive Review`)

## CLEANING Reviews
reviews$text=gsub("&amp", "", reviews$text)
reviews$text = gsub("&amp", "", reviews$text)
reviews$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", reviews$text)
reviews$text = gsub("@\\w+", "", reviews$text)
reviews$text = gsub("[[:punct:]]", "", reviews$text)
reviews$text = gsub("[[:digit:]]", "", reviews$text)
reviews$text = gsub("http\\w+", "", reviews$text)
reviews$text = gsub("[ \t]{2,}", "", reviews$text)
reviews$text = gsub("^\\s+|\\s+$", "", reviews$text)

reviews$text <- iconv(reviews$text, "UTF-8", "ASCII", sub="")

sent.value <- get_sentiment(reviews$text)
nrc_sent.value = get_nrc_sentiment(reviews$text)

simple_plot(sent.value)

corpusABG = Corpus(VectorSource(reviews$text))

wordcloud(corpusABG,colors=rainbow(7),max.words=100)

corpusABG = tm_map(corpusABG, tolower)

corpusABG = tm_map(corpusABG, removePunctuation)

corpusABG = tm_map(corpusABG, removeWords, c("aws",stopwords("english")))

#install.packages("SnowballC")
library(SnowballC)
corpusABG = tm_map(corpusABG, stemDocument)

frequenciesABG = DocumentTermMatrix(corpusABG)

sparseABG = removeSparseTerms(frequenciesABG, 0.995)

ABGSparse = as.data.frame(as.matrix(sparseABG))

colnames(ABGSparse) = make.names(colnames(ABGSparse))

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

ABGSparse$Polarity = category_senti

AWS<-table(ABGSparse$Polarity)

#write.csv(ABGSparse,"Sentiments.csv")

############################## GOOGLE HOME ##################################

Google_Home <- read_excel("Google Home.xlsx")
View(Google_Home)

reviews<-Google_Home
reviews$text<-paste(reviews$Review,reviews$`Descriptive Review`)

## CLEANING Reviews
reviews$text=gsub("&amp", "", reviews$text)
reviews$text = gsub("&amp", "", reviews$text)
reviews$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", reviews$text)
reviews$text = gsub("@\\w+", "", reviews$text)
reviews$text = gsub("[[:punct:]]", "", reviews$text)
reviews$text = gsub("[[:digit:]]", "", reviews$text)
reviews$text = gsub("http\\w+", "", reviews$text)
reviews$text = gsub("[ \t]{2,}", "", reviews$text)
reviews$text = gsub("^\\s+|\\s+$", "", reviews$text)

reviews$text <- iconv(reviews$text, "UTF-8", "ASCII", sub="")

sent.value <- get_sentiment(reviews$text)
nrc_sent.value = get_nrc_sentiment(reviews$text)

simple_plot(sent.value)

corpusABG = Corpus(VectorSource(reviews$text))

wordcloud(corpusABG,colors=rainbow(7),max.words=100)

corpusABG = tm_map(corpusABG, tolower)

corpusABG = tm_map(corpusABG, removePunctuation)

corpusABG = tm_map(corpusABG, removeWords, c("aws",stopwords("english")))

#install.packages("SnowballC")
library(SnowballC)
corpusABG = tm_map(corpusABG, stemDocument)

frequenciesABG = DocumentTermMatrix(corpusABG)

sparseABG = removeSparseTerms(frequenciesABG, 0.995)

ABGSparse = as.data.frame(as.matrix(sparseABG))

colnames(ABGSparse) = make.names(colnames(ABGSparse))

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

ABGSparse$Polarity = category_senti

GoogleHome<-table(ABGSparse$Polarity)

############################## WORDSTREAM #################################
Wordstream <- read_excel("Wordstream.xlsx")
View(Wordstream)

reviews<-Wordstream
reviews$text<-paste(reviews$Review,reviews$`Descriptive Review`)

## CLEANING Reviews
reviews$text=gsub("&amp", "", reviews$text)
reviews$text = gsub("&amp", "", reviews$text)
reviews$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", reviews$text)
reviews$text = gsub("@\\w+", "", reviews$text)
reviews$text = gsub("[[:punct:]]", "", reviews$text)
reviews$text = gsub("[[:digit:]]", "", reviews$text)
reviews$text = gsub("http\\w+", "", reviews$text)
reviews$text = gsub("[ \t]{2,}", "", reviews$text)
reviews$text = gsub("^\\s+|\\s+$", "", reviews$text)

reviews$text <- iconv(reviews$text, "UTF-8", "ASCII", sub="")

sent.value <- get_sentiment(reviews$text)
nrc_sent.value = get_nrc_sentiment(reviews$text)

simple_plot(sent.value)

corpusABG = Corpus(VectorSource(reviews$text))

wordcloud(corpusABG,colors=rainbow(7),max.words=100)

corpusABG = tm_map(corpusABG, tolower)

corpusABG = tm_map(corpusABG, removePunctuation)

corpusABG = tm_map(corpusABG, removeWords, c("aws",stopwords("english")))

#install.packages("SnowballC")
library(SnowballC)
corpusABG = tm_map(corpusABG, stemDocument)

frequenciesABG = DocumentTermMatrix(corpusABG)

sparseABG = removeSparseTerms(frequenciesABG, 0.995)

ABGSparse = as.data.frame(as.matrix(sparseABG))

colnames(ABGSparse) = make.names(colnames(ABGSparse))

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

ABGSparse$Polarity = category_senti

Wordstream<-table(ABGSparse$Polarity)

################################ CLEO ####################################

Cleo <- read_excel("Cleo.xlsx")
View(Cleo)

reviews<-Cleo
reviews$text<-paste(reviews$Review,reviews$`Descriptive Review`)

## CLEANING Reviews
reviews$text=gsub("&amp", "", reviews$text)
reviews$text = gsub("&amp", "", reviews$text)
reviews$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", reviews$text)
reviews$text = gsub("@\\w+", "", reviews$text)
reviews$text = gsub("[[:punct:]]", "", reviews$text)
reviews$text = gsub("[[:digit:]]", "", reviews$text)
reviews$text = gsub("http\\w+", "", reviews$text)
reviews$text = gsub("[ \t]{2,}", "", reviews$text)
reviews$text = gsub("^\\s+|\\s+$", "", reviews$text)

reviews$text <- iconv(reviews$text, "UTF-8", "ASCII", sub="")

sent.value <- get_sentiment(reviews$text)
nrc_sent.value = get_nrc_sentiment(reviews$text)

simple_plot(sent.value)

corpusABG = Corpus(VectorSource(reviews$text))

wordcloud(corpusABG,colors=rainbow(7),max.words=100)

corpusABG = tm_map(corpusABG, tolower)

corpusABG = tm_map(corpusABG, removePunctuation)

corpusABG = tm_map(corpusABG, removeWords, c("aws",stopwords("english")))

#install.packages("SnowballC")
library(SnowballC)
corpusABG = tm_map(corpusABG, stemDocument)

frequenciesABG = DocumentTermMatrix(corpusABG)

sparseABG = removeSparseTerms(frequenciesABG, 0.995)

ABGSparse = as.data.frame(as.matrix(sparseABG))

colnames(ABGSparse) = make.names(colnames(ABGSparse))

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

ABGSparse$Polarity = category_senti

CleoApp<-table(ABGSparse$Polarity)

############################## DONOTPAY ##################################

DonotPay <- read_excel("DonotPay.xlsx")
View(DonotPay)

reviews<-DonotPay
reviews$text<-paste(reviews$Review,reviews$`Descriptive Review`)

## CLEANING Reviews
reviews$text=gsub("&amp", "", reviews$text)
reviews$text = gsub("&amp", "", reviews$text)
reviews$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", reviews$text)
reviews$text = gsub("@\\w+", "", reviews$text)
reviews$text = gsub("[[:punct:]]", "", reviews$text)
reviews$text = gsub("[[:digit:]]", "", reviews$text)
reviews$text = gsub("http\\w+", "", reviews$text)
reviews$text = gsub("[ \t]{2,}", "", reviews$text)
reviews$text = gsub("^\\s+|\\s+$", "", reviews$text)

reviews$text <- iconv(reviews$text, "UTF-8", "ASCII", sub="")

sent.value <- get_sentiment(reviews$text)
nrc_sent.value = get_nrc_sentiment(reviews$text)

simple_plot(sent.value)

corpusABG = Corpus(VectorSource(reviews$text))

wordcloud(corpusABG,colors=rainbow(7),max.words=100)

corpusABG = tm_map(corpusABG, tolower)

corpusABG = tm_map(corpusABG, removePunctuation)

corpusABG = tm_map(corpusABG, removeWords, c("aws",stopwords("english")))

#install.packages("SnowballC")
library(SnowballC)
corpusABG = tm_map(corpusABG, stemDocument)

frequenciesABG = DocumentTermMatrix(corpusABG)

sparseABG = removeSparseTerms(frequenciesABG, 0.995)

ABGSparse = as.data.frame(as.matrix(sparseABG))

colnames(ABGSparse) = make.names(colnames(ABGSparse))

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

ABGSparse$Polarity = category_senti

DonotPayApp<-table(ABGSparse$Polarity)
DonotPayApp

################################# Table ###################################

install.packages("kableExtra")

# For dev version
#install.packages("devtools")
#devtools::install_github("haozhu233/kableExtra")

Table<-rbind(AWS,GoogleHome,Wordstream,CleoApp,DonotPayApp)
Table

library(knitr)
library(kableExtra)

kable(Table,caption ="<b>Sentiment Analysis</b>") %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(1:4, bold = T) %>%
  row_spec(1:4, bold = T, color = "white", background = "#008000")%>%
  row_spec(5, bold = T,color = "white",background = "#D7261E") %>%
  footnote(general = " Here is a general comment of the table. ",
           number = c("Green ones have Highest Positive Sentiments "),
           general_title = "General: ", number_title = "Type I: ",
           footnote_as_chunk = T, title_format = c("italic", "underline")
  )
