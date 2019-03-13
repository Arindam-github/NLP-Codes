#Wordcloud

textplot_wordcloud(dtm,random_order = FALSE,rotation=0.20, random_color = FALSE,
                   min_count = 3)

#Comparison Wordcloud
dtm_comp <- dfm(fulltext, tolower = TRUE ,stem = TRUE, 
                remove_punct=TRUE, remove = stopwords("english"), groups = "Sentiment")

textplot_wordcloud(dtm_comp, comparison = TRUE, 
                   random_order = FALSE,rotation=0.20, random_color = FALSE,
                   min_count = 3)


# Context Window Analysis
Tok_data <- tokens(fulltext,
                   remove_numbers = TRUE,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_twitter = TRUE,
                   include_docvars = TRUE
)

Tok_data <- tokens_tolower(Tok_data)
Tok_data<- tokens_remove(Tok_data, stopwords('en'), padding = TRUE)

Tok_data <- tokens_wordstem(Tok_data)

context_words <- c('phone','work')

#View(Tok_data_kw)



Tok_data_custom <- tokens_select(Tok_data,
                                 context_words,
                                 padding = TRUE, 
                                 window = 5,
                                 selection = "keep")

dtm_Cust_Tok <- dfm(Tok_data_custom)
doc_freq_Cust_Tok <- as.data.frame(docfreq(dtm_Cust_Tok,use.names=TRUE))
names(doc_freq_Cust_Tok)[names(doc_freq_Cust_Tok)=="docfreq(dtm_Cust_Tok, use.names = TRUE)"] <- "f"

#Alternate Approach for Context window
Tok_data_kw <- as.data.frame(kwic(Tok_data,context_words))

# Ngrams

#Positional Analysis - ngrams
ngram <- tokens_ngrams(Tok_data, n = 2:3)
dtm_ngram <- dfm(ngram)

doc_freq_ngram <- as.data.frame(docfreq(dtm_ngram,use.names=TRUE))
names(doc_freq_ngram)[names(doc_freq_ngram)=="docfreq(doc_freq_ngram, use.names = TRUE)"] <- "f"

textplot_wordcloud(dtm_ngram,random_order = FALSE,rotation=0.20, random_color = FALSE,
                   min_count = 2)



#ngrams using custom compound tokens

Custom_bigram <- tokens_compound(Tok_data, phrase('work *'))
Custom_bigram <- tokens_select(Custom_bigram, phrase('work_*'))
dtm_Custom_bigram <- dfm(Custom_bigram)

textplot_wordcloud(dtm_Custom_bigram,random_order = FALSE,rotation=0.20, random_color = FALSE,
                   min_count = 1)

doc_freq_Custom_bigram <- as.data.frame(docfreq(dtm_Custom_bigram,use.names=TRUE))
names(doc_freq_Custom_bigram)[names(doc_freq_Custom_bigram)=="docfreq(dtm_Custom_bigram, use.names = TRUE)"] <- "f"
