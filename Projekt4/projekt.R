#WYMAGANE PACZKI
install.packages("ggthemes")
install.packages("qdap")
install.packages("dplyr")
install.packages("tm")
install.packages("wordcloud")
install.packages("plotrix")
install.packages("dendextend")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("RWeka")
install.packages("reshape2")
install.packages("quanteda")
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)

#LADOWANIE DANYCH
library(readr)
review=read.csv("Womens Clothing E-Commerce Reviews.csv", stringsAsFactors = FALSE)
names(review)

#PREPROCESSING
corpus_review=Corpus(VectorSource(review$Review.Text))
corpus_review=tm_map(corpus_review, tolower)
corpus_review=tm_map(corpus_review, removePunctuation)
corpus_review=tm_map(corpus_review, removeWords, stopwords("english"))
corpus_review=tm_map(corpus_review, removeWords,c("also", "get","like", 
                                                  "company", "made", "can", "im",
                                                  "dress", "just", "i"))

corpus_review=tm_map(corpus_review, stemDocument)
corpus_review[[8]][1]

term_count <- freq_terms(corpus_review, 20)
plot(term_count)

## tworzenie macierzy DTM
review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)
review_term_freq <- rowSums(review_m)
review_term_freq <- sort(review_term_freq, decreasing = T)
review_term_freq[1:10]

barplot(review_term_freq[1:20], col = "steel blue", las = 2)
review_word_freq <- data.frame(term = names(review_term_freq),
                               num = review_term_freq)
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = "red")
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = c("aquamarine","darkgoldenrod","tomato"))

#zabawa z tekstem
corpus_review_all=Corpus(VectorSource(review$Review.Text)) 

corpus_review_all=tm_map(corpus_review_all, tolower)
corpus_review_all=tm_map(corpus_review_all, removePunctuation)
corpus_review_all=tm_map(corpus_review_all, removeWords, stopwords("english"))
corpus_review_all=tm_map(corpus_review_all, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress","just","i"))

corpus_review_all=tm_map(corpus_review_all, stemDocument)
review_tdm_all <- TermDocumentMatrix(corpus_review_all)
all_m=as.matrix(review_tdm_all)
colnames(all_m)=c("Yes","No")
review_term_freq_all <- rowSums(all_m)
review_word_freq_all <- data.frame(term=names(review_term_freq_all), num = review_term_freq_all)
comparison.cloud(all_m,
                 colors = c("green", "red"),
                 max.words = 50)


#Ilosc wystapien w neg/poz tego samego słowa
common_words <- subset(all_m, all_m[, 1] > 0 & all_m[, 2] > 0)
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = T), ]
head(common_words)

# piramida
top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, 
             main = "Words in Common",
             gap = 2000,
             laxlab = NULL,
             raxlab = NULL, 
             unit = NULL,
             top.labels = c("Yes",
                            "Words",
                            "No")
)

#klastrowanie
review_tdm2 <- removeSparseTerms(review_tdm, sparse = 0.9)
hc <- hclust(d = dist(review_tdm2, method = "euclidean"), method = "complete")
plot(hc)

#3-gramy
review_trigram <- tokens(review$Review.Text) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_ngrams(n = 3) %>%
  dfm()
topfeatures(review_trigram)

#SENTIMENT ANALYSIS
# Tokenizacja
reviewtokens=tokens(review$Review.Text,what="word",
                    remove_numbers=TRUE,remove_punct=TRUE, remove_symbols=TRUE, remove_hyphens=TRUE)

reviewtokens=tokens_tolower(reviewtokens)

rmwords <- c("dress", "etc", "also", "xxs", "xs", "s")
reviewtokens=tokens_select(reviewtokens, stopwords(),selection = "remove")
reviewtokens=tokens_remove(reviewtokens,rmwords)

reviewtokens=tokens_wordstem(reviewtokens,language = "english")
reviewtokens=tokens_ngrams(reviewtokens,n=1:2)

#zmiana w liste słów
reviewtokensdfm=dfm(reviewtokens,tolower = FALSE)

reviewSparse <- convert(reviewtokensdfm, "tm")
tm::removeSparseTerms(reviewSparse, 0.7)

dfm_trim(reviewtokensdfm, min_docfreq = 0.3)
x=dfm_trim(reviewtokensdfm, sparsity = 0.98)

#konwersja dfm na dataframe
df=convert(x,to="data.frame")
##dodanie zmiennej klasowej do kolumn
reviewtokensdf=cbind(review$Recommended.IND,df)
head(reviewtokensdf)
## zgodnosc nazw
names(reviewtokensdf)[names(reviewtokensdf) == "review.Recommended.IND"] <- "recommend"
names(reviewtokensdf)=make.names(names(reviewtokensdf))
head(reviewtokensdf)
## usuniecie starej kolumny z ocena
reviewtokensdf=reviewtokensdf[,-c(2)]
head(reviewtokensdf)
reviewtokensdf$recommend=factor(reviewtokensdf$recommend)

#CART algorytm
tree=rpart(formula = recommend ~ ., data = reviewtokensdf, method="class",
           control = rpart.control(minsplit = 200,  minbucket = 30, cp = 0.0001))
printcp(tree)
plotcp(tree)
bestcp=tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
bestcp
ptree=prune(tree,cp=bestcp)
rpart.plot(ptree,cex = 0.6)
prp(ptree, faclen = 0, cex = 0.5, extra = 2)

#RandomForest
library(randomForest)
reviewRF=randomForest(recommend~., data=reviewtokensdf)
varImpPlot(reviewRF, cex=.7)

library(tidyverse)
library(tidytext)
get_sentiments("bing")