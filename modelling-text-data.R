# PART 1-------------------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(text2vec)
library(tidytext)
library(ggplot2)
library(SnowballC)
reviews_df<-read.csv("employee.csv" , encoding = "UTF-8")
reviews_df <- reviews_df %>% 
  rename(
    Description = CompleteReview
  )
reviews_df <- reviews_df[1:100000, ]
reviews_df <- reviews_df[, c(1,2,4)]
reviews_df$Rating <- as.numeric(reviews_df$Rating)
reviews_df$Description <- as.character(reviews_df$Description) %>%
  tolower() %>%
  {gsub(":( |-|o)*\\("," SADSMILE ", .)} %>%       # Find :( or :-( or : ( or :o(
  {gsub(":( |-|o)*\\)"," HAPPYSMILE ", .)} %>%     # Find :) or :-) or : ) or :o)
  {gsub("(\"| |\\$)-+\\.-+"," NUMBER", .)} %>%     # Find numbers
  {gsub("(-+:)*-+ *am"," TIME_AM", .)} %>%         # Find time AM
  {gsub("(-+:)*-+ *pm"," TIME_PM", .)} %>%         # Find time PM
  {gsub("-+:-+","TIME", .)} %>%                    # Find general time
  {gsub("[[:punct:]]", " ",.)} %>% 
  {gsub("([0-9])+"," NUMBER", .)} %>%            # Find numbers
  {gsub(" +"," ", .)}                          # Remove excess spaces
saved <- reviews_df

# Stemming. See which one is better, remove stop words or not.-----------------------------------------------------------------------------------
for (j in 1:nrow(reviews_df)) {
  #  stemmed_description<-  anti_join((reviews_df[j,] %>% unnest_tokens(word,Description, drop=FALSE,to_lower=TRUE) ),stop_words)
  
  stemmed_description<-  ((reviews_df[j,] %>% unnest_tokens(word,Description, drop=FALSE,to_lower=TRUE) ))
  
  stemmed_description<-(wordStem(stemmed_description[,"word"], language = "porter"))
  
  reviews_df[j, "stemmed_reviewtext"] <- paste(stemmed_description, collapse = " ")
  
}
print("done")
stemmed_reviews_df_with_stop <- reviews_df
save(reviews_df,file="employee_stemmed_with_stop.Rdata") 
# Stemming finished-------------------------------------------------------------------------------------------------------------------------------

# Pre-process and create Term Co-occurance Matrix (TCM)-------------------------------------------------------------------------------------------
load(file = "employee_stemmed_with_stop.Rdata")
# create iterator over list of text items
it = itoken(reviews_df$stemmed_reviewtext)

#create the vocabulary and remove infrequent words
vocabulary = create_vocabulary(it)
vocabulary = prune_vocabulary(vocabulary, term_count_min = 400 )
# Note skip_grams_window = 10L -> window size=10
v_vect = vocab_vectorizer(vocabulary)
# create term co-occurrence matrix
tcm = create_tcm(it, v_vect, skip_grams_window = 10L, skip_grams_window_context = "symmetric", weights = rep(1,10) )

glimpse(tcm) # we get a term matrix of size 848*848
# Store half of the symmetric TCM 
printable_tcm <- as.matrix(tcm) + t(as.matrix(tcm))
diag(printable_tcm) <- diag(printable_tcm) - diag(as.matrix(tcm)) #avoid double counting diagonal
printable_tcm <- cbind(printable_tcm , term_count = vocabulary$term_count) # include term counts on diagonal
# TCM finished--------------------------------------------------------------------------------------------------------------------------
# create the GLOvec model
glove_model = GlobalVectors$new( x_max = 1000 , rank = 50) # rank=50 means 50 dimensions

# fit model and obtain results.
word_vectors = glove_model$fit_transform(tcm, n_iter = 200)
word_vec_transpose <- t(word_vectors)

# look at the similarity of different words. It's also about type of words, is this an adjective or is this a noun or food dish
similarity_matrix = sim2(word_vectors)
nwords=nrow(similarity_matrix)
top_bottom=c(1:10,(nwords-9):nwords)
cc=sort(similarity_matrix[,"googl"], decreasing=TRUE)
cc[top_bottom]
cc=sort(similarity_matrix[,"environ"], decreasing=TRUE)
cc[top_bottom]
cc=sort(similarity_matrix[,"opportun"], decreasing=TRUE)
cc[top_bottom]

# An analysis of one single element (the 33rd axis)-----------------------------------------------------------------------------------
one_single_element <- read.csv(file = "single_element.csv")
head(one_single_element,10)
tail(one_single_element,10)

# word arithmetic---------------------------------------------------------------------------------------------------------------------
comparator = word_vectors["experienc",]+word_vectors["microsoft",] - word_vectors["kpmg",]
similarities = sim2(word_vectors,t(comparator))
ranking = similarities %>% order(decreasing=TRUE)
print(as.data.frame(similarities[ranking[top_bottom]], row.names = vocabulary$term[ranking[top_bottom]]))
# Part 1 finished---------------------------------------------------------------------------------------------------------------------
```


# PART 2-------------------------------------------------------------------------------------------------------------
library(plyr)
library(syuzhet)
library(textclean)


reviews_df$User_ID = 1:nrow(reviews_df)
reviews_df$Rating = ifelse(reviews_df$Rating>=4, "happy", "unhappy")
reviews_df$Rating = as.factor(reviews_df$Rating)

### Remove stop words (with/without "no", "not", "never") and stem ###
ignorelist = stop_words %>% filter(!word %in% c("no", "not", "never"))

for (j in 1:nrow(reviews_df)) {
  
  words <- reviews_df[j,] %>% 
    unnest_tokens(word, Description) %>% 
    anti_join(ignorelist, by="word")
  
  stemmed <- wordStem(words[ , "word"], language = "porter")
  reviews_df[j, "stemmed_Description_with_no"] <- paste(stemmed, collapse = " ")
  
  # Again, but with ignoring all stopwords
  nostopwords <- reviews_df[j,] %>% unnest_tokens(word, Description) %>%
    anti_join( stop_words, by = "word")
  stemmed <- wordStem(nostopwords[ , "word"], language = "porter")
  
  # Add variables to data
  reviews_df[j, "stemmed_Description"] <- paste(stemmed, collapse = " ")
  reviews_df[j, "Description_not_stemmed"] <- paste((nostopwords$word), collapse = " ")
  reviews_df[j, "Nr_of_words"]<- nrow(nostopwords)
}

### construct bigrams###
all_bigrams <- reviews_df[,c("User_ID", "stemmed_Description_with_no")] %>% 
  unnest_tokens(bigram, stemmed_Description_with_no, token = "ngrams", n = 2 )


all_bigrams <- all_bigrams %>%  dplyr::count(bigram, sort = TRUE)

sel_bigrams <- all_bigrams %>% filter(n>85)


bigrams_sep <-  separate(all_bigrams, bigram, c("word1", "word2"), sep = " ")

# Get word frequency after stemming
frequency  <- reviews_df %>% unnest_tokens(word, stemmed_Description) %>% dplyr::count(word, sort=TRUE)

# Select very frequent or infrequent words
infrequent <- frequency %>% filter(n < 0.01*nrow(reviews_df))
frequent   <- frequency %>% filter(word %in% c("manag", "compani")) # you can extend this list with word you want to remove
toremove   <- full_join(frequent, infrequent, by = "word")      # combining these word lists

### Remove common words from stemmed Description
for (j in 1:nrow(reviews_df)) 
{
  tmp <-  anti_join( (reviews_df[j,] %>% unnest_tokens(word, stemmed_Description) ), toremove, by = "word") 
  
  reviews_df[j,"stemmed_Description"] <- paste(tmp[, "word"], collapse = " ")
}

#Get document term matrix uni-grams
reviews_df$User_ID <- as.character(reviews_df$User_ID) %>% as.factor() # the factor may have more values than are actually present. These are removed here, as this causes an error in prcomp

review_dtm <- reviews_df %>% 
  unnest_tokens(word, stemmed_Description) %>% 
  dplyr::count(User_ID, word, sort=TRUE) %>% 
  ungroup() %>%
  cast_dtm(User_ID,word,n)

#Get document term matrix for bi-grams
review_dtm_bi <- reviews_df %>% 
  unnest_tokens(bigram, stemmed_Description_with_no, token = "ngrams", n = 2) %>% 
  filter(bigram %in% sel_bigrams$bigram) %>%
  dplyr::count(User_ID, bigram, sort=TRUE)
review_dtm_bi$User_ID = as.character(review_dtm_bi$User_ID)

review_dtm_bi <- review_dtm_bi %>% 
  ungroup() %>%
  cast_dtm(User_ID, bigram, n)

### add indicators for 50 most common words ###
counts <- colSums(as.matrix(review_dtm)) %>% sort(decreasing=TRUE)

lastcol        <- ncol(reviews_df)
N_words_stored <- 50
word_labels    <- (names(counts)[1:N_words_stored])
reviews_df     <- data.frame(reviews_df, words = as.matrix(review_dtm[,word_labels]))
names(reviews_df)[(lastcol+1):(lastcol+N_words_stored)] <- word_labels

### add bigrams ###
review_dtm_bi <- as.matrix(review_dtm_bi)
reviews_df <- cbind(reviews_df, review_dtm_bi[match(reviews_df$User_ID, rownames(review_dtm_bi)),])

reviews_df[is.na(reviews_df)] <- 0


### add inferred emotions ###
nrc_emotions  <- get_nrc_sentiment(reviews_df$Description)
lastcol <- ncol(reviews_df)
reviews_df <- data.frame(reviews_df, nrc_emotions)

### add LDA topics ###
library(textmineR)

dtm <- CreateDtm(doc_vec = reviews_df[, "stemmed_Description"], # character vector of documents
                 doc_names = reviews_df[, "User_ID"]) # document names

alpha = .1
n_topics = 20
lda_results <- FitLdaModel(dtm = dtm,
                           k = n_topics, # number of topic
                           burnin = 200 + 10*n_topics,
                           iterations = 700 + 10*n_topics,
                           alpha = alpha, beta = 0.05,
                           optimize_alpha = T,
                           calc_likelihood = T,
                           calc_coherence = T)

reviews_df <- data.frame(reviews_df, Topics = lda_results$theta)

phi <- lda_results$phi
top_term_table <- NULL
for (j in 1:n_topics) {
  words <- t(phi[j,]) %>% order(decreasing=TRUE) %>% head(10) %>% phi[j,.]
  top_term_table <- rbind(top_term_table, data.frame(topic=j,probability=words , term = labels(words)) )
}

text_top_terms <- top_term_table %>%
  group_by(topic) %>%
  top_n(10, probability) %>%
  ungroup() %>%
  arrange(topic, -probability)

perplot <- 10
set <-c(1:4,6,7,13,16,17,19) #significant ones
set <-c(2,3,8,17,18,20) #non-significant ones
for (i in 1:1)
{
  p <- text_top_terms %>%
    filter(topic %in% set) %>%
    mutate(term = reorder_within(term, probability, topic)) %>%
    ggplot(aes(term, probability, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()+
    scale_x_reordered()
  show(p)
}

N_words_stored <- 50
N_bigrams_stored <- 38
N_emotions <- 10 # includes pos/neg
N_topics <- 20

### get feature names and split samples ###
index <- 9
wordnames    <- colnames(reviews_df)[index:(index+N_words_stored-1)]
index <- index + N_words_stored
bigramnames <- colnames(reviews_df)[index:(index+N_bigrams_stored-1)]
index <- index + N_bigrams_stored
emotionnames <- colnames(reviews_df)[index:(index+N_emotions-1)]
index <- index + N_emotions
topicnames <- colnames(reviews_df)[index:(index+N_topics-1)]
index <- index + N_topics

### split the data set ###
set.seed(1234)    # fix seed to allow for results to be reproducible
estimation_sample <- sort(sample(1:nrow(reviews_df), size = round(0.7*nrow(reviews_df))))
test_sample <- (1:nrow(reviews_df))[-estimation_sample]

### Prepare some strings for use in models ###
allEmotions <- paste("(", paste(emotionnames,collapse=" + "), ")")
allWords <- paste("(", paste(wordnames,collapse=" + "), ")")
allBigrams <- paste("(", paste(bigramnames,collapse=" + "), ")")
allWordsAndBigrams <- paste("(", paste(c(wordnames, bigramnames),collapse=" + "), ")")
allTopics <- paste("(", paste(topicnames,collapse=" + "), ")")

# logit model (GLM)
set.seed(1234)
f <- paste("(Rating) ~  Nr_of_words + ", allWords , " + ", allBigrams )
glm.allwords <- glm(f, data=reviews_df[estimation_sample,], family = "binomial" )
summary(glm.allwords)

set.seed(1234)
f <- paste("(Rating) ~  Nr_of_words + ", allEmotions)
glm.allemotions <- glm(f, data=reviews_df[estimation_sample,], family = "binomial")
summary(glm.allemotions)

set.seed(1234)
f <- paste("(Rating) ~  Nr_of_words + ",  allTopics )
glm.alltopics <- glm(f, data=reviews_df[estimation_sample,], family = "binomial" )
summary(glm.alltopics)

set.seed(1234)
f <- paste("(Rating) ~  Nr_of_words + ", allEmotions, " + ", allWords , " + ", allBigrams, "+ ", allTopics )
glm.all <- glm(f, data=reviews_df[estimation_sample,], family = "binomial" )
summary(glm.all)


### AIC ###
set.seed(1234)
stats_df <- AIC(glm.all, glm.allwords, glm.allemotions, glm.alltopics)

### model performance based on accuracy and log-likelihood ###
library(caret)

model_list = NULL
model_list[["all"]] <- glm.all
model_list[["allwords"]] <- glm.allwords
model_list[["allemotions"]] <- glm.allemotions
model_list[["alltopics"]] <- glm.alltopics


outcomes <- NULL

j <- "allwords"

model_list_names <- c("all","allwords","allemotions","alltopics")
set.seed(1234)
for (j in 1:length(model_list_names)) {
  
  outputs <- model_list_names[j]
  predictions <- predict(model_list[[model_list_names[j]]], type="response")
  accuracy <- confusionMatrix(data = as.factor( predictions > .5),  
                              reference = as.factor(reviews_df[estimation_sample,"Rating"]=="happy"))$overall[1]
  loglik <- sum(log(predictions[reviews_df[estimation_sample,"Rating"]=="happy"]))
  loglik <- loglik +  sum(log(1-predictions[reviews_df[estimation_sample,"Rating"]=="unhappy"]))
  
  
  outputs <- c(outputs, accuracy, loglik)
  
  predictions <- predict(model_list[[model_list_names[j]]],reviews_df[test_sample,], type="response")
  accuracy <- confusionMatrix(data = as.factor( predictions > .5),  
                              reference = as.factor(reviews_df[test_sample,"Rating"]=="happy"))$overall[1]
  loglik <- sum(log(predictions[reviews_df[test_sample,"Rating"]=="happy"]))
  loglik <- loglik +  sum(log(1-predictions[reviews_df[test_sample,"Rating"]=="unhappy"]))
  
  outputs <- c(outputs, accuracy, loglik)
  
  outcomes <- rbind(outcomes, outputs)
  
}
outcomes

# on t-values
vi <- varImp(glm.all)
vi$Variable <- rownames(vi)
vi <- vi[order(-vi$Overall),]
vi$Variable <- factor(vi$Variable, levels = rev(vi$Variable))

ggplot(vi[1:25, ], aes(Variable,Overall)) + geom_bar(stat = "identity") + coord_flip() + ylab("Variable importance (t-value based)")

ggplot(vi[(nrow(vi)-24):nrow(vi), ], aes(Variable,Overall)) + geom_bar(stat = "identity") + coord_flip() + ylab("Variable importance (t-value based)")

### LASSO ###
library(glmnet)
library(plotmo) # for plot_glmnet


f <- paste("(Rating) ~  Nr_of_words + ", allEmotions, " + ", allWords , " + ", allBigrams, "+ ", allTopics )

LargeX <- model.matrix(formula(f), data=reviews_df)
LargeX = LargeX[,-1]
set.seed(1234)
y <- as.factor(reviews_df[estimation_sample, "Rating"] == "happy")
lasso.glm <- glmnet(LargeX[estimation_sample,], y, family="binomial", alpha = 1)

plot_glmnet(lasso.glm) 
plot(lasso.glm)

cvfit.glm <- cv.glmnet(LargeX[estimation_sample,], (y), family="binomial", alpha = 1)
plot(cvfit.glm)

coef(lasso.glm, cvfit.glm$lambda.1se)

par <- predict(lasso.glm, s = cvfit.glm$lambda.1se, type='coefficients')
nnzero(par)
length(par)

lasso.glm.pred <- predict(lasso.glm, s = cvfit.glm$lambda.1se, type="response", newx = LargeX[estimation_sample,])
lasso.glm.pred.test <- predict(lasso.glm, s = cvfit.glm$lambda.1se,type="response", newx = LargeX[test_sample,])



model_list[["Lasso_all"]] <- lasso.glm


model_list_names <- c("Lasso_all")
set.seed(1234)
for (j in 1:length(model_list_names)) {
  
  outputs <- model_list_names[j]
  predictions <- predict(lasso.glm, s = cvfit.glm$lambda.1se, type="response", newx = LargeX[estimation_sample,])
  
  accuracy <- confusionMatrix(data = as.factor( predictions > .5),  
                              reference = as.factor(reviews_df[estimation_sample,"Rating"]=="happy"))$overall[1]
  loglik <- sum(log(predictions[reviews_df[estimation_sample,"Rating"]=="happy"]))
  loglik <- loglik +  sum(log(1-predictions[reviews_df[estimation_sample,"Rating"]=="unhappy"]))
  
  
  outputs <- c(outputs, accuracy, loglik)
  
  predictions <- predict(lasso.glm, s = cvfit.glm$lambda.1se,type="response", newx = LargeX[test_sample,])
  
  accuracy <- confusionMatrix(data = as.factor( predictions > .5),  
                              reference = as.factor(reviews_df[test_sample,"Rating"]=="happy"))$overall[1]
  loglik <- sum(log(predictions[reviews_df[test_sample,"Rating"]=="happy"]))
  loglik <- loglik +  sum(log(1-predictions[reviews_df[test_sample,"Rating"]=="unhappy"]))
  
  outputs <- c(outputs, accuracy, loglik)
  
  outcomes <- rbind(outcomes, outputs)
  
}

outcomes

### Random Forest ###
library("randomForest")

f <- paste("(Rating) ~  Nr_of_words + ", allEmotions, " + ", allWords , " + ", allBigrams, "+ ", allTopics )

set.seed(1234)
rf = randomForest(formula(f),  
                  ntree = 50,
                  data = reviews_df[estimation_sample,])
plot(rf)
print(rf)

varImpPlot(rf,  
           sort = T,
           n.var = 20,
           main = "Top 20 - Variable Importance")

library(caret)


model_list[["RF_all"]] <- rf


model_list_names <- c("RF_all")
set.seed(123)
for (j in 1:length(model_list_names)) {
  
  outputs <- model_list_names[j]
  predictions <-  predict(rf, type = "prob")[,2]
  
  accuracy <- confusionMatrix(data = as.factor( predictions > .5),  
                              reference = as.factor(reviews_df[estimation_sample,"Rating"]=="happy"))$overall[1]
  loglik <- sum(log(predictions[reviews_df[estimation_sample,"Rating"]=="happy"]))
  loglik <- loglik +  sum(log(1-predictions[reviews_df[estimation_sample,"Rating"]=="unhappy"]))
  
  
  outputs <- c(outputs, accuracy, loglik)
  
  predictions <- predict(rf, reviews_df[test_sample,], type = "prob")[,2]
  
  accuracy <- confusionMatrix(data = as.factor( predictions > .5),  
                              reference = as.factor(reviews_df[test_sample,"Rating"]=="happy"))$overall[1]
  loglik <- sum(log(predictions[reviews_df[test_sample,"Rating"]=="happy"]))
  loglik <- loglik +  sum(log(1-predictions[reviews_df[test_sample,"Rating"]=="unhappy"]))
  
  outputs <- c(outputs, accuracy, loglik)
  
  outcomes <- rbind(outcomes, outputs)
  
}

outcomes
