
# Libraries + Options -----------------------------------------------------
library(dplyr)
pacman::p_load(janeaustenr)
library(tidytext)

# Functions ---------------------------------------------------------------
word_count <- function(x) sapply(gregexpr("[A-z]\\W+", x), length) + 1L

get_weight <- function(count, eps=10000, min_count=2){
  # If a word appears only once, we ignore it completely (likely a typo)
  # Epsilon defines a smoothing constant, which makes the effect of extremely rare words smaller
  ifelse(count < min_count,0,1 / (count + eps))
}

word_shares <- function(q1_clean,q2_clean,question1, question2){ #, i
  
  q1words <- unlist(str_split(pattern = " ",str_replace_all(q1_clean, "[[:punct:]]", " ")))
  q1words <- unique(q1words[!q1words==""])
  q2words <- unlist(str_split(pattern = " ",str_replace_all(q2_clean, "[[:punct:]]", " ")))
  q2words <- unique(q2words[!q2words==""])
  
  q1words  <- train_qs[train_qs$word %in% q1words,]
  q2words  <- train_qs[train_qs$word %in% q2words,]
  
  shared_words                  <- intersect(q1words$word, q2words$word)
  shared_weights                <- train_qs[train_qs$word %in% shared_words,]
  shared_weights$shared_weights <- get_weight(shared_weights$n,eps=0, min_count=2)
  
  total_weights <- c(get_weight(q1words$n,eps=0, min_count=2),
                     get_weight(q2words$n,eps=0, min_count=2)
  )
  
  R1  = sum(shared_weights$shared_weights) / sum(total_weights)              # tfidf share
  R2  = length(shared_words) / (length(q1words$word) + length(q2words$word)) # count share

  q1words0 <- unlist(str_split(pattern = " ",str_replace_all(question1, "[[:punct:]]", " ")))
  q1words0 <- unique(q1words0[!q1words0==""])
  q2words0 <- unlist(str_split(pattern = " ",str_replace_all(question2, "[[:punct:]]", " ")))
  q2words0 <- unique(q2words0[!q2words0==""])
  
  q1words1 <- unlist(str_split(pattern = " ",str_replace_all(q1_clean, "[[:punct:]]", " ")))
  q1words1 <- unique(q1words1[!q1words1==""])
  q2words1 <- unlist(str_split(pattern = " ",str_replace_all(q2_clean, "[[:punct:]]", " ")))
  q2words1 <- unique(q2words1[!q2words1==""])

  wcq1        <- length(q1words0)
  wcq1a       <- length(q1words1)
  len_q1stops <- wcq1 - wcq1a
  
  wcq2        <- length(q2words0)
  wcq2a       <- length(q2words1)
  len_q2stops <- wcq2 - wcq2a
  
  R31 = len_q1stops / wcq1 # ratio of stops in q1
  R32 = len_q2stops / wcq2 # ratio of stops in q2
  
  x <- data.frame(
    word_match       = R1,
    tfidf_word_match = R2,
    shared_count     = length(shared_words),
    stops1_ratio     = R31,
    stops2_ratio     = R32,
    diff_stops_r     = R31-R32,
    len_q1           = wcq1,
    len_q2           = wcq2,
    diff_len         = wcq1-wcq2,
    exact_dup        = q1_clean==q2_clean
  )
  
  return(x)
}


# TF-IDF ------------------------------------------------------------------
ab_format0 <- tibble(text = train$q1_clean, book = "train Q1")
ab_format1 <- tibble(text = train$q2_clean, book = "train Q2")

ab_format2 <- tibble(text = test$q1_clean, book = "test Q1")
ab_format3 <- tibble(text = test$q2_clean, book = "test Q2")

ab_format <- rbind(ab_format0,ab_format1,ab_format2,ab_format3)
rm(ab_format0,ab_format1,ab_format2,ab_format3);gc()

book_words <- ab_format %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>% group_by(book) %>% summarize(total = sum(n))
book_words  <- left_join(book_words, total_words)


library(ggplot2)
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

saveRDS(book_words, "tf_idf.RDS")

tf_idf <- readRDS("tf_idf.RDS")

tf_idf <- tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

tf_idf1 <- sqldf("select distinct word as word,
                avg(tf) as tf,
                avg(idf) as idf,
                avg(tf_idf) as tf_idf
                from tf_idf 
                group by word")

tf_idf1 <- tf_idf1 %>% arrange(desc(tf_idf))

plot(tf_idf1$tf_idf,xlim=c(0,40))

train$q1_tfidf <- 0
train$q2_tfidf <- 0

test$q1_tfidf <- 0
test$q2_tfidf <- 0
  
for(n in 1:11){
  cat("train q1", n, "\n")
  train$q1_tfidf <- ifelse(grepl(tf_idf1$word[n], train$q1_clean),tf_idf1$tf_idf[n]+train$q1_tfidf, train$q1_tfidf)
  cat("train q2", n, "\n")
  train$q2_tfidf <- ifelse(grepl(tf_idf1$word[n], train$q2_clean),tf_idf1$tf_idf[n]+train$q2_tfidf, train$q2_tfidf)
  
  cat("test q1", n, "\n")
  test$q1_tfidf <- ifelse(grepl(tf_idf1$word[n], test$q1_clean),tf_idf1$tf_idf[n]+test$q1_tfidf, test$q1_tfidf)
  cat("test q2", n, "\n")
  test$q2_tfidf <- ifelse(grepl(tf_idf1$word[n], test$q2_clean),tf_idf1$tf_idf[n]+test$q2_tfidf, test$q2_tfidf)
}

train$tfidf <- train$q1_tfidf + train$q2_tfidf
test$tfidf  <- test$q1_tfidf  + test$q2_tfidf

train$tfidf_delta <- train$q1_tfidf - train$q2_tfidf
test$tfidf_delta  <- test$q1_tfidf  - test$q2_tfidf

train$q1_clean_fw <- gsub("([A-Za-z]+).*", "\\1", train$q1_clean)
train$q2_clean_fw <- gsub("([A-Za-z]+).*", "\\1", train$q2_clean)
test$q1_clean_fw  <- gsub("([A-Za-z]+).*", "\\1", test$q1_clean)
test$q2_clean_fw  <- gsub("([A-Za-z]+).*", "\\1", test$q2_clean)


t  <- sqldf("select a.id, b.tf_idf as if_idf_q1 from train a left join tf_idf1 b on a.q1_clean_fw = b.word")
t1 <- sqldf("select a.id, b.tf_idf as if_idf_q2 from train a left join tf_idf1 b on a.q2_clean_fw = b.word")

t2 <- sqldf("select b.tf_idf as if_idf_q1 from test a left join tf_idf1 b on a.q1_clean_fw = b.word")
t3 <- sqldf("select b.tf_idf as if_idf_q2 from test a left join tf_idf1 b on a.q2_clean_fw = b.word")

train$if_idf_q1_fw <- t$if_idf_q1
train$if_idf_q2_fw <- t1$if_idf_q2

test$if_idf_q1_fw <- t2$if_idf_q1
test$if_idf_q2_fw <- t3$if_idf_q2

saveRDS(train, "train.RDS")
saveRDS(test, "test.RDS")

# WC weights / TF-IDF ------------------------------------------------------
# TRAIN
tf_idf <- readRDS("tf_idf.RDS")

train_qs <- tf_idf[order(tf_idf$n,decreasing=T),]
q1words  <- train_qs[train_qs$book %in% c("train Q1"),]
q2words  <- train_qs[train_qs$book %in% c("train Q2"),]

train_qs <- train_qs[train_qs$book %in% c("train Q1", "train Q2"),]
train_qs <- sqldf("select  sum(n) as n,
                  word
                  from train_qs
                  group by word")
train_qs <- train_qs[order(train_qs$n, decreasing = T),]


temp  <- as.data.table(t(mapply(word_shares,train$q1_clean,train$q2_clean,train$question1,train$question2)))
saveRDS(temp, "train_word_shares.RDS")

require(parallel)
require(doParallel)
cores_2_use <- detectCores() - 4
cl          <- makeCluster(cores_2_use, useXDR = F)
clusterSetRNGStream(cl, 9956)
registerDoParallel(cl, cores_2_use)

clusterExport(cl, list("word_shares", "str_split","str_replace_all",
                       "train_qs","get_weight"))

s<-Sys.time()
temp0  <- as.data.table(t(clusterMap(cl,word_shares,test$q1_clean,
                                                   test$q2_clean,
                                                   test$question1,
                                                   test$question2)))
Sys.time()-s

test.tfidf <- plyr::ldply(temp0, data.frame)
stopCluster(cl)
saveRDS(test.tfidf, "test.tfidf.RDS")
saveRDS(temp0, "test_word_shares.RDS")


test.tfidf$.id <- NULL
test           <- cbind(test, test.tfidf)

train.tfidf <- readRDS("train_word_shares.RDS")
train       <- cbind(train, train.tfidf)

train$word_match       <- unlist(train$word_match)
train$tfidf_word_match <- unlist(train$tfidf_word_match)
train$shared_count     <- unlist(train$shared_count)
train$stops1_ratio     <- unlist(train$stops1_ratio)
train$stops2_ratio     <- unlist(train$stops2_ratio)
train$diff_stops_r     <- unlist(train$diff_stops_r)
train$len_q1           <- unlist(train$len_q1)
train$len_q2           <- unlist(train$len_q2)
train$diff_len         <- unlist(train$diff_len)
train$exact_dup        <- unlist(train$exact_dup)

train$word_match       <- RRF::na.roughfix(train$word_match)
train$tfidf_word_match <- RRF::na.roughfix(train$tfidf_word_match)
train$shared_count     <- RRF::na.roughfix(train$shared_count)
train$stops1_ratio     <- RRF::na.roughfix(train$stops1_ratio)
train$stops2_ratio     <- RRF::na.roughfix(train$stops2_ratio)
train$diff_stops_r     <- RRF::na.roughfix(train$diff_stops_r)
train$len_q1           <- RRF::na.roughfix(train$len_q1)
train$len_q2           <- RRF::na.roughfix(train$len_q2)
train$diff_len         <- RRF::na.roughfix(train$diff_len)
train$exact_dup        <- RRF::na.roughfix(train$exact_dup)

test$word_match       <- RRF::na.roughfix(test$word_match)
test$tfidf_word_match <- RRF::na.roughfix(test$tfidf_word_match)
test$shared_count     <- RRF::na.roughfix(test$shared_count)
test$stops1_ratio     <- RRF::na.roughfix(test$stops1_ratio)
test$stops2_ratio     <- RRF::na.roughfix(test$stops2_ratio)
test$diff_stops_r     <- RRF::na.roughfix(test$diff_stops_r)
test$len_q1           <- RRF::na.roughfix(test$len_q1)
test$len_q2           <- RRF::na.roughfix(test$len_q2)
test$diff_len         <- RRF::na.roughfix(test$diff_len)
test$exact_dup        <- RRF::na.roughfix(test$exact_dup)

saveRDS(train, "train.RDS")
saveRDS(test, "test.RDS")



