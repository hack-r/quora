word_count <- function(x) sapply(gregexpr("[A-z]\\W+", x), length) + 1L

# Train -------------------------------------------------------------------
## TM cleaned FE ####
# String Distances
train$clean_osa  <- stringsim(train$q1_clean, train$q2_clean, method = "osa")
train$clean_lv   <- stringsim(train$q1_clean, train$q2_clean, method = "lv") # looks identical to osa
train$clean_dl   <- stringsim(train$q1_clean, train$q2_clean, method = "dl") # looks identical to osa
train$clean_lcs  <- stringsim(train$q1_clean, train$q2_clean, method = "lcs")

# Bi-gram cleaned flags
train$q1_flag_make_money   <- ifelse(grepl('make money',train$q1_clean),1,0)
train$q1_flag_lose_weight  <- ifelse(grepl('lose weight',train$q1_clean),1,0)
train$q1_flag_500_1000     <- ifelse(grepl('500 1000',train$q1_clean),1,0)
train$q1_flag_best_way     <- ifelse(grepl('best way',train$q1_clean),1,0)
train$q1_flag_can_find     <- ifelse(grepl('can find',train$q1_clean),1,0)
train$q1_flag_can_get      <- ifelse(grepl('can get',train$q1_clean),1,0)
train$q1_flag_can_make     <- ifelse(grepl('can make',train$q1_clean),1,0)
train$q1_flag_donald_trump <- ifelse(grepl('donald trump',train$q1_clean),1,0)
train$q1_flag_whi_doe      <- ifelse(grepl('whi doe',train$q1_clean),1,0)
train$q1_flag_whi_peopl    <- ifelse(grepl('whi peopl',train$q1_clean),1,0)

train$q2_flag_make_money   <- ifelse(grepl('make money',train$q2_clean),1,0)
train$q2_flag_lose_weight  <- ifelse(grepl('lose weight',train$q2_clean),1,0)
train$q2_flag_500_1000     <- ifelse(grepl('500 1000',train$q2_clean),1,0)
train$q2_flag_best_way     <- ifelse(grepl('best way',train$q2_clean),1,0)
train$q2_flag_can_find     <- ifelse(grepl('can find',train$q2_clean),1,0)
train$q2_flag_can_get      <- ifelse(grepl('can get',train$q2_clean),1,0)
train$q2_flag_can_make     <- ifelse(grepl('can make',train$q2_clean),1,0)
train$q2_flag_donald_trump <- ifelse(grepl('donald trump',train$q2_clean),1,0)
train$q2_flag_whi_doe      <- ifelse(grepl('whi doe',train$q2_clean),1,0)
train$q2_flag_whi_peopl    <- ifelse(grepl('whi peopl',train$q2_clean),1,0)

train$flag_make_money   <- ifelse(train$q1_flag_make_money==1 & train$q2_flag_make_money == 1,1,0)
train$flag_lose_weight  <- ifelse(train$q1_flag_lose_weight==1 & train$q2_flag_lose_weight == 1,1,0)
train$flag_500_1000     <- ifelse(train$q1_flag_500_1000==1 & train$q2_flag_500_1000 == 1,1,0)
train$flag_best_way     <- ifelse(train$q1_flag_best_way==1 & train$q2_flag_best_way == 1,1,0)
train$flag_can_find     <- ifelse(train$q1_flag_can_find==1 & train$q2_flag_can_find == 1,1,0)
train$flag_can_get      <- ifelse(train$q1_flag_can_get==1 & train$q2_flag_can_get == 1,1,0)
train$flag_can_make     <- ifelse(train$q1_flag_can_make==1 & train$q2_flag_can_make == 1,1,0)
train$flag_donald_trump <- ifelse(train$q1_flag_donald_trump==1 & train$q2_flag_donald_trump == 1,1,0)
train$flag_whi_doe      <- ifelse(train$q1_flag_whi_doe==1 & train$q2_flag_whi_doe == 1,1,0)
train$flag_whi_peopl    <- ifelse(train$q1_flag_whi_peopl==1 & train$q2_flag_whi_peopl == 1,1,0)

# Interactions / transformations
train$clean_sd_sum  <- train$clean_osa + train$clean_lv  + train$clean_dl + train$clean_lcs
train$clean_sd_mul  <- train$clean_osa * train$clean_lv  * train$clean_dl * train$clean_lcs
train$ln_clean_lcs  <- log(train$clean_lcs)
train$ln_word_match <- log(train$word_match)
  
## Raw Data FE ####
# String Distances (truly raw)
train$raw_osa  <- stringsim(train$question1, train$question2, method = "osa")
train$raw_lv   <- stringsim(train$question1, train$question2, method = "lv") # looks identical to osa
train$raw_dl   <- stringsim(train$question1, train$question2, method = "dl") # looks identical to osa
train$raw_lcs  <- stringsim(train$question1, train$question2, method = "lcs")

# Lower Case from here on out
train$question1 <- tolower(train$question1)
train$question2 <- tolower(train$question2)

# String Distances (lower case version)
train$raw_lower_osa  <- stringsim(train$question1, train$question2, method = "osa")
train$raw_lower_lv   <- stringsim(train$question1, train$question2, method = "lv") # looks identical to osa
train$raw_lower_dl   <- stringsim(train$question1, train$question2, method = "dl") # looks identical to osa
train$raw_lower_lcs  <- stringsim(train$question1, train$question2, method = "lcs")

# Flags  
train$q1_flag_who   <- ifelse(grepl('who',train$question1),1,0)
train$q1_flag_what  <- ifelse(grepl('what',train$question1),1,0)
train$q1_flag_when  <- ifelse(grepl('when',train$question1),1,0)
train$q1_flag_where <- ifelse(grepl('where',train$question1),1,0) 
train$q1_flag_why   <- ifelse(grepl('why',train$question1),1,0) 
train$q1_flag_which <- ifelse(grepl('which',train$question1),1,0) 
train$q1_flag_how   <- ifelse(grepl('how',train$question1),1,0)
train$q1_flag_get   <- ifelse(grepl('get',train$question1),1,0)
train$q1_flag_good  <- ifelse(grepl('good',train$question1),1,0)
train$q1_flag_india <- ifelse(grepl('india',train$question1),1,0)
train$q1_flag_math  <- ifelse(grepl('math',train$question1),1,0)
train$q1_flag_trump <- ifelse(grepl('trump',train$question1),1,0)
train$q1_flag_book  <- ifelse(grepl('book',train$question1),1,0)
train$q1_flag_can   <- ifelse(grepl('can',train$question1),1,0)
train$q1_flag_day   <- ifelse(grepl('day',train$question1),1,0)
train$q1_flag_ever  <- ifelse(grepl('ever',train$question1),1,0)
train$q1_flag_new   <- ifelse(grepl('new',train$question1),1,0)
train$q1_flag_life  <- ifelse(grepl('life',train$question1),1,0)

train$q2_flag_who   <- ifelse(grepl('who',train$question2),1,0)
train$q2_flag_what  <- ifelse(grepl('what',train$question2),1,0)
train$q2_flag_when  <- ifelse(grepl('when',train$question2),1,0)
train$q2_flag_where <- ifelse(grepl('where',train$question2),1,0) 
train$q2_flag_why   <- ifelse(grepl('why',train$question2),1,0) 
train$q2_flag_which <- ifelse(grepl('which',train$question2),1,0) 
train$q2_flag_how   <- ifelse(grepl('how',train$question2),1,0)
train$q2_flag_get   <- ifelse(grepl('get',train$question2),1,0)
train$q2_flag_good  <- ifelse(grepl('good',train$question2),1,0)
train$q2_flag_india <- ifelse(grepl('india',train$question2),1,0)
train$q2_flag_math  <- ifelse(grepl('math',train$question2),1,0)
train$q2_flag_trump <- ifelse(grepl('trump',train$question2),1,0)
train$q2_flag_book  <- ifelse(grepl('book',train$question2),1,0)
train$q2_flag_can   <- ifelse(grepl('can',train$question2),1,0)
train$q2_flag_day   <- ifelse(grepl('day',train$question2),1,0)
train$q2_flag_ever  <- ifelse(grepl('ever',train$question2),1,0)
train$q2_flag_new   <- ifelse(grepl('new',train$question2),1,0)
train$q2_flag_life  <- ifelse(grepl('life',train$question2),1,0)

train$sum_flag_who		<-	train$q2_flag_who	  +	train$q1_flag_who	
train$sum_flag_what		<-	train$q2_flag_what	+	train$q1_flag_what	
train$sum_flag_when		<-	train$q2_flag_when	+	train$q1_flag_when	
train$sum_flag_where	<-	train$q2_flag_where	+	train$q1_flag_where	
train$sum_flag_why		<-	train$q2_flag_why	  +	train$q1_flag_why	
train$sum_flag_which	<-	train$q2_flag_which	+	train$q1_flag_which	
train$sum_flag_how		<-	train$q2_flag_how  	+	train$q1_flag_how	
train$sum_flag_get		<-	train$q2_flag_get 	+	train$q1_flag_get	
train$sum_flag_good		<-	train$q2_flag_good	+	train$q1_flag_good	
train$sum_flag_india	<-	train$q2_flag_india	+	train$q1_flag_india	
train$sum_flag_math		<-	train$q2_flag_math	+	train$q1_flag_math	
train$sum_flag_trump	<-	train$q2_flag_trump	+	train$q1_flag_trump	
train$sum_flag_book		<-	train$q2_flag_book	+	train$q1_flag_book	
train$sum_flag_can		<-	train$q2_flag_can	  +	train$q1_flag_can	
train$sum_flag_day		<-	train$q2_flag_day 	+	train$q1_flag_day	
train$sum_flag_ever		<-	train$q2_flag_ever	+	train$q1_flag_ever	
train$sum_flag_new		<-	train$q2_flag_new	  +	train$q1_flag_new	
train$sum_flag_life		<-	train$q2_flag_life	+	train$q1_flag_life	

# Counts
train$q1_nchar    <- nchar(train$question1)
train$q2_nchar    <- nchar(train$question2)
train$nchar_delta <- train$q1_nchar - train$q2_nchar

train$q1_word_count  <- word_count(train$question1)
train$q2_word_count  <- word_count(train$question2)

train$same_word_count <- as.factor(ifelse(train$q1_word_count == train$q2_word_count,1,0))
train$wc_delta        <- train$q1_word_count - train$q2_word_count


# [1] "ani"      "becom"    "best"     "book"     "can"      "day"      "differ"   "doe"      "engin"    "ever"     "find"     "get"      "good"    
# [14] "happen"   "india"    "indian"   "job"      "know"     "learn"    "life"     "like"     "make"     "mani"     "mean"     "money"    "movi"    
# [27] "much"     "new"      "one"      "peopl"    "question" "quora"    "someon"   "start"    "thing"    "think"    "time"     "trump"    "use"     
# [40] "want"     "way"      "whi"      "will"     "without"  "work"     "year"  

# Test --------------------------------------------------------------------
## TM cleaned FE ####
# String Distances
test$clean_osa  <- stringsim(test$q1_clean, test$q2_clean, method = "osa")
test$clean_lv   <- stringsim(test$q1_clean, test$q2_clean, method = "lv") # looks identical to osa
test$clean_dl   <- stringsim(test$q1_clean, test$q2_clean, method = "dl") # looks identical to osa
test$clean_lcs  <- stringsim(test$q1_clean, test$q2_clean, method = "lcs")

# Bi-gram cleaned flags
test$q1_flag_make_money   <- ifelse(grepl('make money',test$q1_clean),1,0)
test$q1_flag_lose_weight  <- ifelse(grepl('lose weight',test$q1_clean),1,0)
test$q1_flag_500_1000     <- ifelse(grepl('500 1000',test$q1_clean),1,0)
test$q1_flag_best_way     <- ifelse(grepl('best way',test$q1_clean),1,0)
test$q1_flag_can_find     <- ifelse(grepl('can find',test$q1_clean),1,0)
test$q1_flag_can_get      <- ifelse(grepl('can get',test$q1_clean),1,0)
test$q1_flag_can_make     <- ifelse(grepl('can make',test$q1_clean),1,0)
test$q1_flag_donald_trump <- ifelse(grepl('donald trump',test$q1_clean),1,0)
test$q1_flag_whi_doe      <- ifelse(grepl('whi doe',test$q1_clean),1,0)
test$q1_flag_whi_peopl    <- ifelse(grepl('whi peopl',test$q1_clean),1,0)

test$q2_flag_make_money   <- ifelse(grepl('make money',test$q2_clean),1,0)
test$q2_flag_lose_weight  <- ifelse(grepl('lose weight',test$q2_clean),1,0)
test$q2_flag_500_1000     <- ifelse(grepl('500 1000',test$q2_clean),1,0)
test$q2_flag_best_way     <- ifelse(grepl('best way',test$q2_clean),1,0)
test$q2_flag_can_find     <- ifelse(grepl('can find',test$q2_clean),1,0)
test$q2_flag_can_get      <- ifelse(grepl('can get',test$q2_clean),1,0)
test$q2_flag_can_make     <- ifelse(grepl('can make',test$q2_clean),1,0)
test$q2_flag_donald_trump <- ifelse(grepl('donald trump',test$q2_clean),1,0)
test$q2_flag_whi_doe      <- ifelse(grepl('whi doe',test$q2_clean),1,0)
test$q2_flag_whi_peopl    <- ifelse(grepl('whi peopl',test$q2_clean),1,0)

test$flag_make_money   <- ifelse(test$q1_flag_make_money==1 & test$q2_flag_make_money == 1,1,0)
test$flag_lose_weight  <- ifelse(test$q1_flag_lose_weight==1 & test$q2_flag_lose_weight == 1,1,0)
test$flag_500_1000     <- ifelse(test$q1_flag_500_1000==1 & test$q2_flag_500_1000 == 1,1,0)
test$flag_best_way     <- ifelse(test$q1_flag_best_way==1 & test$q2_flag_best_way == 1,1,0)
test$flag_can_find     <- ifelse(test$q1_flag_can_find==1 & test$q2_flag_can_find == 1,1,0)
test$flag_can_get      <- ifelse(test$q1_flag_can_get==1 & test$q2_flag_can_get == 1,1,0)
test$flag_can_make     <- ifelse(test$q1_flag_can_make==1 & test$q2_flag_can_make == 1,1,0)
test$flag_donald_trump <- ifelse(test$q1_flag_donald_trump==1 & test$q2_flag_donald_trump == 1,1,0)
test$flag_whi_doe      <- ifelse(test$q1_flag_whi_doe==1 & test$q2_flag_whi_doe == 1,1,0)
test$flag_whi_peopl    <- ifelse(test$q1_flag_whi_peopl==1 & test$q2_flag_whi_peopl == 1,1,0)

# Interactions / transformations
test$clean_sd_sum  <- test$clean_osa + test$clean_lv  + test$clean_dl + test$clean_lcs
test$clean_sd_mul  <- test$clean_osa * test$clean_lv  * test$clean_dl * test$clean_lcs
test$ln_clean_lcs  <- log(test$clean_lcs)
test$ln_word_match <- log(test$word_match)

## Raw Data FE ####
# String Distances (truly raw)
test$raw_osa  <- stringsim(test$question1, test$question2, method = "osa")
test$raw_lv   <- stringsim(test$question1, test$question2, method = "lv") # looks identical to osa
test$raw_dl   <- stringsim(test$question1, test$question2, method = "dl") # looks identical to osa
test$raw_lcs  <- stringsim(test$question1, test$question2, method = "lcs")

# Lower Case from here on out
test$question1 <- tolower(test$question1)
test$question2 <- tolower(test$question2)

# String Distances (lower case version)
test$raw_lower_osa  <- stringsim(test$question1, test$question2, method = "osa")
test$raw_lower_lv   <- stringsim(test$question1, test$question2, method = "lv") # looks identical to osa
test$raw_lower_dl   <- stringsim(test$question1, test$question2, method = "dl") # looks identical to osa
test$raw_lower_lcs  <- stringsim(test$question1, test$question2, method = "lcs")

# Flags  
test$q1_flag_who   <- ifelse(grepl('who',test$question1),1,0)
test$q1_flag_what  <- ifelse(grepl('what',test$question1),1,0)
test$q1_flag_when  <- ifelse(grepl('when',test$question1),1,0)
test$q1_flag_where <- ifelse(grepl('where',test$question1),1,0) 
test$q1_flag_why   <- ifelse(grepl('why',test$question1),1,0) 
test$q1_flag_which <- ifelse(grepl('which',test$question1),1,0) 
test$q1_flag_how   <- ifelse(grepl('how',test$question1),1,0)
test$q1_flag_get   <- ifelse(grepl('get',test$question1),1,0)
test$q1_flag_good  <- ifelse(grepl('good',test$question1),1,0)
test$q1_flag_india <- ifelse(grepl('india',test$question1),1,0)
test$q1_flag_math  <- ifelse(grepl('math',test$question1),1,0)
test$q1_flag_trump <- ifelse(grepl('trump',test$question1),1,0)
test$q1_flag_book  <- ifelse(grepl('book',test$question1),1,0)
test$q1_flag_can   <- ifelse(grepl('can',test$question1),1,0)
test$q1_flag_day   <- ifelse(grepl('day',test$question1),1,0)
test$q1_flag_ever  <- ifelse(grepl('ever',test$question1),1,0)
test$q1_flag_new   <- ifelse(grepl('new',test$question1),1,0)
test$q1_flag_life  <- ifelse(grepl('life',test$question1),1,0)

test$q2_flag_who   <- ifelse(grepl('who',test$question2),1,0)
test$q2_flag_what  <- ifelse(grepl('what',test$question2),1,0)
test$q2_flag_when  <- ifelse(grepl('when',test$question2),1,0)
test$q2_flag_where <- ifelse(grepl('where',test$question2),1,0) 
test$q2_flag_why   <- ifelse(grepl('why',test$question2),1,0) 
test$q2_flag_which <- ifelse(grepl('which',test$question2),1,0) 
test$q2_flag_how   <- ifelse(grepl('how',test$question2),1,0)
test$q2_flag_get   <- ifelse(grepl('get',test$question2),1,0)
test$q2_flag_good  <- ifelse(grepl('good',test$question2),1,0)
test$q2_flag_india <- ifelse(grepl('india',test$question2),1,0)
test$q2_flag_math  <- ifelse(grepl('math',test$question2),1,0)
test$q2_flag_trump <- ifelse(grepl('trump',test$question2),1,0)
test$q2_flag_book  <- ifelse(grepl('book',test$question2),1,0)
test$q2_flag_can   <- ifelse(grepl('can',test$question2),1,0)
test$q2_flag_day   <- ifelse(grepl('day',test$question2),1,0)
test$q2_flag_ever  <- ifelse(grepl('ever',test$question2),1,0)
test$q2_flag_new   <- ifelse(grepl('new',test$question2),1,0)
test$q2_flag_life  <- ifelse(grepl('life',test$question2),1,0)

test$sum_flag_who		<-	test$q2_flag_who	  +	test$q1_flag_who	
test$sum_flag_what	<-	test$q2_flag_what	+	test$q1_flag_what	
test$sum_flag_when	<-	test$q2_flag_when	+	test$q1_flag_when	
test$sum_flag_where	<-	test$q2_flag_where	+	test$q1_flag_where	
test$sum_flag_why		<-	test$q2_flag_why	  +	test$q1_flag_why	
test$sum_flag_which	<-	test$q2_flag_which	+	test$q1_flag_which	
test$sum_flag_how		<-	test$q2_flag_how  	+	test$q1_flag_how	
test$sum_flag_get		<-	test$q2_flag_get 	+	test$q1_flag_get	
test$sum_flag_good	<-	test$q2_flag_good	+	test$q1_flag_good	
test$sum_flag_india	<-	test$q2_flag_india	+	test$q1_flag_india	
test$sum_flag_math	<-	test$q2_flag_math	+	test$q1_flag_math	
test$sum_flag_trump	<-	test$q2_flag_trump	+	test$q1_flag_trump	
test$sum_flag_book	<-	test$q2_flag_book	+	test$q1_flag_book	
test$sum_flag_can		<-	test$q2_flag_can	  +	test$q1_flag_can	
test$sum_flag_day		<-	test$q2_flag_day 	+	test$q1_flag_day	
test$sum_flag_ever	<-	test$q2_flag_ever	+	test$q1_flag_ever	
test$sum_flag_new		<-	test$q2_flag_new	  +	test$q1_flag_new	
test$sum_flag_life	<-	test$q2_flag_life	+	test$q1_flag_life	

# Counts
test$q1_nchar    <- nchar(test$question1)
test$q2_nchar    <- nchar(test$question2)
test$nchar_delta <- test$q1_nchar - test$q2_nchar

test$q1_word_count  <- word_count(test$question1)
test$q2_word_count  <- word_count(test$question2)

test$same_word_count <- as.factor(ifelse(test$q1_word_count == test$q2_word_count,1,0))
test$wc_delta        <- test$q1_word_count - test$q2_word_count

#get_nrc_sentiment


# Add Imported Features ---------------------------------------------------
train_feats <- fread("train_features.csv")
test_feats  <- fread("test_features.csv")

train <- cbind(train, train_feats[,3:ncol(train_feats)])
test  <- cbind(test, test_feats[,3:ncol(test_feats)])

# Save --------------------------------------------------------------------
saveRDS(train, "train.RDS")
saveRDS(test, "test.RDS")