fulltext<-corpus(Inp_Data,text_field = "text")

dtm <- dfm(fulltext, tolower = TRUE ,stem = TRUE, 
           remove_punct=TRUE, remove = stopwords("english"))

dtm.m <- as.matrix(dtm)

dim(dtm.m)

# Token frequency per document (Exploratory Analysis)
tokens_in_doc <- as.data.frame(rowSums(dtm))
names(tokens_in_doc)[names(tokens_in_doc)=="rowSums(dtm)"] <- "f"



# Token frequency per document (Exploratory Analysis) - alternate method using quanteda function
tokfreq <- textstat_frequency(dtm, n = 100)

library(ggplot2)

ggplot(tokens_in_doc, aes(x = f)) +
  
  theme_bw() +
  
  geom_histogram(binwidth = 1) +
  
  labs(y = "# Doc", x = "Token Count per Doc",
       
       title = "Distribution of Tokens per Document")



