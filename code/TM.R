require(wordcloud)

# Run one-off analysis? ---------------------------------------------------
run <- F

# Support Functions -------------------------------------------------------
rmHTML <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Train -------------------------------------------------------------------
# # Create a single vector (q1 + q2)
# train.q    <- cbind(train$question1, train$question2)
# train.q.dt <- data.table(question1=train.q[,1],
#                          question2=train.q[,2])
# saveRDS(train.q.dt, "train.q.dt.RDS")

# TM map cleaning
ds <- Corpus(VectorSource(train.q.dt$question1))
ds <- tm_map(ds, content_transformer(tolower))
ds <- tm_map(ds, stripWhitespace)
ds <- tm_map(ds, removePunctuation)
ds <- tm_map(ds, stemDocument)
ds <- tm_map(ds, removeWords, c("the", stopwords("english")))

saveRDS(ds, "train.ds.q1.RDS")

ds2 <- Corpus(VectorSource(train.q.dt$question2))
ds2 <- tm_map(ds2, content_transformer(tolower))
ds2 <- tm_map(ds2, stripWhitespace)
ds2 <- tm_map(ds2, removePunctuation)
ds2 <- tm_map(ds2, stemDocument)
ds2 <- tm_map(ds2, removeWords, c("the", stopwords("english")))

saveRDS(ds2, "train.ds.q2.RDS")

# Append clean text to data set
tmp            <- data.table(text=unlist(sapply(ds, `[`, "content")),
                             stringsAsFactors=F)
row.names(tmp) <- NULL
tmp1           <- tmp[,.(text)]
train$q1_clean <- trim(tmp1$text)

tmp            <- data.table(text=unlist(sapply(ds2, `[`, "content")),
                             stringsAsFactors=F)
tmp2           <- tmp[,.(text)]
train$q2_clean <- trim(tmp2$text)
saveRDS(train, "train.RDS"); rm(ds, ds2, tmp, tmp1, tmp2); gc()



# Test -------------------------------------------------------------------
# TM map cleaning
ds <- Corpus(VectorSource(test$question1))
ds <- tm_map(ds, content_transformer(tolower))
ds <- tm_map(ds, stripWhitespace)
ds <- tm_map(ds, removePunctuation)
ds <- tm_map(ds, stemDocument)
ds <- tm_map(ds, removeWords, c("the", stopwords("english")))

saveRDS(ds, "test.ds.q1.RDS")

ds2 <- Corpus(VectorSource(test$question2))
ds2 <- tm_map(ds2, content_transformer(tolower))
ds2 <- tm_map(ds2, stripWhitespace)
ds2 <- tm_map(ds2, removePunctuation)
ds2 <- tm_map(ds2, stemDocument)
ds2 <- tm_map(ds2, removeWords, c("the", stopwords("english")))

saveRDS(ds2, "test.ds.q2.RDS")

# Append clean text to data set
tmp            <- data.table(text=unlist(sapply(ds, `[`, "content")),
                             stringsAsFactors=F)
row.names(tmp) <- NULL
tmp1           <- tmp[,.(text)]
test$q1_clean  <- trim(tmp1$text)

tmp            <- data.table(text=unlist(sapply(ds2, `[`, "content")),
                             stringsAsFactors=F)
tmp2           <- tmp[,.(text)]
test$q2_clean  <- trim(tmp2$text)
saveRDS(test, "test.RDS"); rm(ds, ds2, tmp, tmp1, tmp2); gc()

# 
# # TDM / DTM analysis
# if(run){
#   tdm.1g <- TermDocumentMatrix(ds)
#   saveRDS(tdm.1g, "train.tdm.1g.RDS")
#   
#   findFreqTerms(tdm.1g, 10000)
# #   [1] "ani"      "becom"    "best"     "book"     "can"      "day"      "differ"   "doe"      "engin"    "ever"     "find"     "get"      "good"    
# #   [14] "happen"   "india"    "indian"   "job"      "know"     "learn"    "life"     "like"     "make"     "mani"     "mean"     "money"    "movi"    
# #   [27] "much"     "new"      "one"      "peopl"    "question" "quora"    "someon"   "start"    "thing"    "think"    "time"     "trump"    "use"     
# #   [40] "want"     "way"      "whi"      "will"     "without"  "work"     "year"  
#   findFreqTerms(tdm.1g, 20000)
#   # [1] "best"   "can"    "differ" "doe"    "get"    "good"   "india"  "like"   "make"   "one"    "peopl"  "use"    "way"    "whi"    "will"  
#   
#   
#   findAssocs(tdm.1g, "india", .75)
#   
#   tdm2.1g <- removeSparseTerms(tdm.1g, 0.98)
#   
#   # Creates a Boolean matrix (counts # docs w/terms, not raw # terms)
#   tdm3.1g <- inspect(tdm2.1g)
#   tdm3.1g[tdm3.1g>=1] <- 1
#   
#   # Transform into a term-term adjacency matrix
#   termMatrix.1gram <- tdm3.1g %*% t(tdm3.1g)
#   
#   # inspect terms numbered 5 to 10
#   termMatrix.1gram[5:10,5:10]
#   termMatrix.1gram[1:10,1:10]
#   
#   # Create a WordCloud to Visualize the Text Data
#   notsparse <- tdm2.1g
#   m = as.matrix(notsparse)
#   v = sort(rowSums(m),decreasing=TRUE)
#   d = data.frame(word = names(v),freq=v)
#   
#   # Create the word cloud
#   pal = brewer.pal(9,"BuPu")
#   wordcloud(words = d$word,
#             freq = d$freq,
#             scale = c(3,.8),
#             random.order = F,
#             colors = pal,
#             min.freq=1)
#   
#   
# }
# 
# 
# # Test --------------------------------------------------------------------
# # Create a single vector (q1 + q2)
# test.q    <- cbind(test$question1, test$question2)
# test.q.dt <- data.table(question1=test.q[,1],
#                          question2=test.q[,2])
# saveRDS(test.q.dt, "test.q.dt.RDS")
# 
# # TM map cleaning
# ds <- Corpus(VectorSource(test.q))
# ds <- tm_map(ds, content_transformer(tolower))
# ds <- tm_map(ds, stripWhitespace)
# ds <- tm_map(ds, removePunctuation)
# ds <- tm_map(ds, stemDocument)
# ds <- tm_map(ds, removeWords, c("the", stopwords("english")))
# 
# saveRDS(ds, "test.ds.RDS")
# 
# # Append clean text to data set
# tmp            <- data.table(text=unlist(sapply(ds, `[`, "content")),
#                              stringsAsFactors=F)
# row.names(tmp) <- NULL
# tmp1           <- tmp[,.(text)]
# tmp2           <- tmp1[1:2345796]
# tmp3           <- tmp1[2345797:4691592]
# test$q1_clean <- tmp2
# test$q2_clean <- tmp3
# saveRDS(test, "test.RDS")
# 
# # TDM / DTM analysis
# if(run){
#   tdm.1g <- TermDocumentMatrix(ds)
#   saveRDS(tdm.1g, "test.tdm.1g.RDS")
#   
#   findFreqTerms(tdm.1g, 10000)
#   findFreqTerms(tdm.1g, 20000)
# 
#   
#   findAssocs(tdm.1g, "india", .75)
#   
#   tdm2.1g <- removeSparseTerms(tdm.1g, 0.98)
#   
#   # Creates a Boolean matrix (counts # docs w/terms, not raw # terms)
#   tdm3.1g <- inspect(tdm2.1g)
#   tdm3.1g[tdm3.1g>=1] <- 1
#   
#   # Transform into a term-term adjacency matrix
#   termMatrix.1gram <- tdm3.1g %*% t(tdm3.1g)
#   
#   # inspect terms numbered 5 to 10
#   termMatrix.1gram[5:10,5:10]
#   termMatrix.1gram[1:10,1:10]
#   
#   # Create a WordCloud to Visualize the Text Data
#   notsparse <- tdm2.1g
#   m = as.matrix(notsparse)
#   v = sort(rowSums(m),decreasing=TRUE)
#   d = data.frame(word = names(v),freq=v)
#   
#   # Create the word cloud
#   pal = brewer.pal(9,"BuPu")
#   wordcloud(words = d$word,
#             freq = d$freq,
#             scale = c(3,.8),
#             random.order = F,
#             colors = pal,
#             min.freq=1)
#   
#   
# }
# 
# 


# 2-grams -----------------------------------------------------------------
all   <- data.frame(qs = c(train$q1_clean, train$q2_clean))
all$n <- 1:nrow(all)
samp  <- sample_n(all, 250000)
vc    <- as.VCorpus(Corpus(VectorSource(samp$qs)) )

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.2g          <- TermDocumentMatrix(vc, control = list(tokenize = BigramTokenizer))

findFreqTerms(tdm.2g, lowfreq = 100)

tdm2.2g <- removeSparseTerms(tdm.2g, 0.995)

# Creates a Boolean matrix (counts # docs w/terms, not raw # terms)
tdm3.2g <- inspect(tdm2.2g)
tdm3.2g[tdm3.2g>=1] <- 1

# Transform into a term-term adjacency matrix
termMatrix.2gram <- tdm3.2g %*% t(tdm3.2g)
termMatrix.2gram

saveRDS(tdm.2g, "tdm.2g.RDS")
saveRDS(termMatrix.2gram, "termMatrix.2gram")
