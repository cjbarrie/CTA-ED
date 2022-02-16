library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidytext)
library(stringdist)
library(corrplot)
library(janeaustenr)

thought1 <- "We are all very happy to be at a lecture at 10AM"
thought2 <- "We are all even happier that we don’t have a lecture next week"

# character level

## longest common substring distance
stringdist(thought1, thought2,
           method = "lcs")

## levenshtein distance
stringdist(thought1, thought2,
           method = "lv")

## jaro distance
stringdist(thought1, thought2,
           method = "jw", p =0)

# word level

## similarity and distance example

text <- janeaustenr::prideprejudice

sentences <- text[10:11]

sentence1 <- paste(sentences[1], sentences[2], sep = " ")

sentence2 <- "Everyone knows that a rich man without wife wants a wife"

sentence3 <- "He's loaded so he wants to get married. Everyone knows that's what happens."

dfmat <- dfm(tokens(c(sentence1,
                      sentence2,
                      sentence3)),
             remove_punct = TRUE, remove = stopwords("english"))

## correlation
textstat_simil(dfmat, margin = "documents", method = "correlation")

## show in tidy format
test <- tidy(dfmat)
test <- test %>%
  cast_dfm(term, document, count)
test <- as.data.frame(test)

## Add regression plane plot
scatterplot3d(test[,2:3], pch = 16, type="h", color = "red")

## Add correlation plot
textstat_simil(dfmat, margin = "documents", method = "correlation")
## is same as:
res <- cor(test[,2:4])
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

## euclidean
euclidean <- function(a,b) sqrt(sum((a - b)^2))
euclidean(test$text1, test$text2)
## is same as 
textstat_dist(dfmat, margin = "documents", method = "euclidean")

## manhattan
manhattan <- function(a, b){
  dist <- abs(a - b)
  dist <- sum(dist)
  return(dist)
}

manhattan(test$text1, test$text2)
## is same as:
textstat_dist(dfmat, margin = "documents", method = "manhattan")

## cosine
cos.sim <- function(a, b) 
{
  return(sum(a*b)/sqrt(sum(a^2)*sum(b^2)) )
}  

cos.sim(test$text1, test$text2)
## is same as
textstat_simil(dfmat, margin = "documents", method = "cosine")

## jaccard
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

jaccard(test$text1, test$text2)

## is same as
textstat_simil(dfmat, margin = "documents", method = "jaccard")

textstat_simil(dfmat, margin = "documents", method = "cosine")
textstat_simil(dfmat, margin = "documents", method = "jaccard")

textstat_dist(dfmat, margin = "documents", method = "euclidean")
textstat_dist(dfmat, margin = "documents", method = "manhattan")

plot(hclust(as.dist(tstat)))


## correlation example

sentence4 <- "Into the face of the young man who sat on the terrace of the Hotel Magnifique at Cannes there had crept a look of furtive shame, the shifty, hangdog look which announces that an Englishman is about to talk French."

austen <- sentence1
wodehouse <- sentence4

dfmat <- dfm(tokens(c(austen,
                      wodehouse)),
             remove_punct = TRUE, remove = stopwords("english"))

textstat_simil(dfmat, margin = "documents", method = "correlation")

test <- tidy(dfmat)
test <- test %>%
  cast_dfm(term, document, count)
test <- as.data.frame(test)
cor(test$text1, test$text2)
