
dtm.matrix <- as.matrix(dtm)

#TF IDF using custom Function

term.frequency <- function(row) {
  
  row / sum(row)
  
}

#function for calculating inverse document frequency (IDF)

inverse.doc.freq <- function(col) {
  
  corpus.size <- length(col)
  
  doc.count <- length(which(col > 0))
  
  
  
  log10(corpus.size / doc.count)
  
}

# Our function for calculating TF-IDF.

tf.idf <- function(x, idf) {
  
  x * idf
  
}



# Step 1- normalize all documents via TF.

tokens.df <- apply(dtm.matrix, 1, term.frequency)

dim(tokens.df)
View(tokens.df[1:20, 1:100])

#Test that normalized term frequencies add up to 1
chk.termfreq = t(tokens.df)
Sum.termfreq <- as.data.frame(rowSums(chk.termfreq))




# Step 2 - calculate the IDF vector that we will use - both


tokens.idf <- apply(dtm.matrix, 2, inverse.doc.freq)

str(tokens.idf)





# Lastly, calculate TF-IDF for our training corpus.

tokens.tfidf <-  apply(tokens.df, 2, tf.idf, idf = tokens.idf)

dim(tokens.tfidf)

View(tokens.tfidf[1:25, 1:25])

# Transpose the matrix

tokens.tfidf <- t(tokens.tfidf)

dim(tokens.tfidf)

View(tokens.tfidf[1:25, 1:25])

#TF IDF using Quanteda - the Easy way

tfidf <- as.data.frame(dfm_tfidf(dtm), scheme_tf = "prop")