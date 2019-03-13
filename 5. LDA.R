
#LDA Core Task:  

# Factorize DTM into = (Matrix of Topics and Terms) X (Matrix of Topics and Documents)

# (Matrix of Topics and Terms) ----> probability distribution of Terms given Topic

# (Matrix of Topics and Documents) ----> probability distribution of Topics given Document

#Libraries Required

library(tidyverse) # general utility library
library(tidytext)  # general utility library for texts/NLP tasks
library(topicmodels) # Required for LDA 
library(quanteda)
library(readtext)

#Read the data
#The data is about reviews scraped from Playstore about banking apps

TopicModelData <- readtext("C:/Users/Arindam/DataSc/BankApReview/Playstore_Reviews.csv",text_field = "Reviews_Text" )
TopicModelData_df <- as.data.frame(TopicModelData)

#Converting data to a text corpus
fulltext_topicmodel<-corpus(TopicModelData,text_field = "text")

#Creating Document term matrix and doing basic cleaning
dtm_topicmodel <- dfm(fulltext_topicmodel, tolower = TRUE ,stem = TRUE, 
           remove_punct=TRUE, remove = stopwords("english"))


#Check for rows in DTM with all null entries, this step is important
row_total = apply(dtm_topicmodel, 1, sum)

#Remove rows in DTM with all null entries
dtm.clean = dtm_topicmodel [row_total>0,]


#Check Row Reduction
nrow(dtm_topicmodel)
nrow(dtm.clean)

#Run LDA
lda <- LDA(dtm.clean, k = 4, control = list(seed = 1234))

#Extract the term given topic matrix from the LDA object
topics <- tidy(lda, matrix = "beta")

#Little bit of data wrangling
library(reshape2)
Distribution_TermOverTopic = dcast(topics,term~topic,value.var = "beta")

#Extract the top 20 terms for every topic
top_terms <- topics  %>% # for the topic dataframe
  group_by(topic) %>% # treat each topic as a different group
  top_n(20, beta) %>% # get the top 20 important terms
  ungroup() %>% # ungroup
  arrange(topic, -beta) # arrange terms in decreasing order of importance


top_terms %>% # take the top terms
  #mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
  ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
  geom_col(show.legend = FALSE) + # as a bar plot
  facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
  labs(x = NULL, y = "Beta") + # no x label, change y label 
  coord_flip() # turn bars sideways




#------------------------------------------------------------------------------------------

#Extract the Topic given document matrix from the LDA object
topicsByDoc <- tidy(lda, matrix = "gamma")

Distribution_TopicOverDoc = dcast(topicsByDoc,document~topic,value.var = "gamma")

Distribution_TopicOverDoc$TotProb = 
  (Distribution_TopicOverDoc$`1`+Distribution_TopicOverDoc$`2`+
  Distribution_TopicOverDoc$`3`+Distribution_TopicOverDoc$`4`)


#Average Representation of Topics over documents
sum(Distribution_TopicOverDoc$`1`)/nrow(Distribution_TopicOverDoc)
sum(Distribution_TopicOverDoc$`2`)/nrow(Distribution_TopicOverDoc)
sum(Distribution_TopicOverDoc$`3`)/nrow(Distribution_TopicOverDoc)
sum(Distribution_TopicOverDoc$`4`)/nrow(Distribution_TopicOverDoc)