library(quanteda)
library(readtext)
library(dplyr)


#Reading Data
Inp_Data <- readtext("C:/Users/Arindam/DataSc/NLP app/AmzData.csv",text_field = "Review" )
Inp_Data_df <- as.data.frame(Inp_Data)


table(Inp_Data_df$Sentiment)

#Lengths

L<- as.data.frame(nchar(Inp_Data_df$text))
L$Sentiment <- as.factor((Inp_Data_df$Sentiment))
names(L)[names(L)=="nchar(Inp_Data_df$text)"] <- "len"


library(ggplot2)

ggplot(L, aes(x = len, fill = Sentiment)) +
  
  theme_bw() +
  
  geom_histogram(binwidth = 5) +
  
  labs(y = "Text Count", x = "Length of Text",
       
       title = "Distribution of Review Lengths by Sentiment")

#Creating Corpus

fulltext<-corpus(Inp_Data,text_field = "text")


#Creating DTM with the DFM function

dtm1 <- dfm(fulltext, remove_punct=TRUE)
dtm1.m <- as.matrix(dtm1)
dim(dtm1.m)





