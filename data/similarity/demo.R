library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidytext)
library(stringdist)
library(corrplot)
library(janeaustenr)
library(dplyr)
library(tibble)
library(ggplot2)

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


## Comparing tweets


load("/Users/cbarrie6/Dropbox/pd_projects/MP_enviro/data/analysis/MPtweetsv2.Rdata")

mps <- c("theresa_may", "damiangreen", "philiphammonduk", "amberrudduk", 
         "borisjohnson", "daviddavismp","gavinwilliamson","jeremy_hunt",
         "dlidington", "justinegreening", "liamfox", "gregclarkmp",
         "michaelgove", "chrisgraylingmp", "sajidjavid", "davidmundelldct",
         "aluncairns", "jbrokenshire", "pennymordaunt", "davidgauke",
         "andrealeadsom", "trussliz", "juliansmithuk", "brandonlewis", 
         "damianhinds")

tweets <- MPtweets %>%
  filter(username %in% mps)

tweets <- tweets %>%
  filter(date <= "2018-01-08") %>%
  select(username, tweet, date)

saveRDS(tweets, "data/comparison-complexity/cabinet_tweets.rds")

tweets_corpus <- corpus(tweets, text_field = "tweet")

docvars(tweets_corpus, "username") <- tweets$username

dfmat <- dfm(tokens(tweets_corpus),
             remove_punct = TRUE, 
             remove = stopwords("english"))


methods <- c("correlation", "cosine", "jaccard", 
  "ejaccard", "dice", "edice", "hamann", "simple matching")

methods <- c("correlation", "cosine", "dice", "edice")

testdf_all <- data.frame()

for (i in seq_along(methods)) {
  
  sim_method <- methods[[i]]
  
  test <- dfmat %>%
    dfm_group(groups = username) %>%
    textstat_simil(margin = "documents", method = sim_method)
  
  testm <- as.matrix(test)
  
  testdf <- as.data.frame(testm[23, c(1:22, 24)])
  
  colnames(testdf) <- "corr_may"
  
  testdf <- tibble::rownames_to_column(testdf, "username")
  
  testdf$method <- sim_method

  testdf_all <- rbind(testdf_all, testdf)  
  
}

testdf_all <- testdf_all %>%
  group_by(username) %>%
  mutate(mean_sim = mean(corr_may))

ggplot(testdf_all) +
  geom_point( aes(x=reorder(username, -mean_sim), y= corr_may, color = method)) + 
  coord_flip() +
  xlab("MP username") +
  ylab("Similarity score") + 
  theme_minimal()


# methods <- c("euclidean", "manhattan")
# testdf_all <- data.frame()
# 
# for (i in seq_along(methods)) {
#   
#   dist_method <- methods[[i]]
#   
#   test <- dfmat %>%
#     dfm_group(groups = username) %>%
#     textstat_dist(margin = "documents", method = dist_method)
#   
#   testm <- as.matrix(test)
#   
#   testdf <- as.data.frame(testm[23, c(1:22, 24)])
#   
#   colnames(testdf) <- "corr_may"
#   
#   testdf <- tibble::rownames_to_column(testdf, "username")
#   
#   testdf$method <- dist_method
#   
#   testdf_all <- rbind(testdf_all, testdf)  
#   
# }
# 
# testdf_all <- testdf_all %>%
#   group_by(username) %>%
#   mutate(mean_sim = mean(corr_may))
# 
# ggplot(testdf_all) +
#   geom_point( aes(x=reorder(username, -mean_sim), y= corr_may, color = method)) + 
#   coord_flip() +
#   xlab("MP username") +
#   ylab("Similarity score") + 
#   theme_minimal()

# test <- dfmat %>%
#   dfm_group(groups = username) %>%
#   textstat_simil(margin = "documents", method = "jaccard")
# 
# testm <- as.matrix(test)
# 
# testdf <- as.data.frame(testm[23, c(1:22, 24)])
# 
# colnames(testdf) <- "corr_may"
# 
# testdf <- tibble::rownames_to_column(testdf, "username")
# 
# ggplot(testdf, aes(x=reorder(username, -corr_may), y= corr_may)) +
#   geom_bar(stat="identity") + 
#   coord_flip() +
#   xlab("MP username") +
#   ylab("Mean similarity") + 
#   theme_minimal()

# df <- as.data.frame(testm[8,1:7])
# colnames(df) <- "correlation"
# df <- tibble::rownames_to_column(df, "username")

# # diag(testm)=NA
# 
# dates <- unique(dfmat@docvars$date)
# df_all <- data.frame()
# 
# for (i in seq_along(dates)) {
#   
#   seqdate <- dates[[i]]
#   
#   tweets_sub <- tweets %>%
#     filter(date == seqdate)
#   
#   tweets_corpus_sub <- corpus(tweets_sub, text_field = "tweet")
#   
#   docvars(tweets_corpus_sub, "username") <- tweets_sub$username
#   
#   dfmat <- dfm(tokens(tweets_corpus_sub),
#                remove_punct = TRUE, 
#                remove = stopwords("english"))
#   
#   dfmatg <- dfmat %>%
#     dfm_group(groups = username) %>%
#     textstat_simil(margin = "documents",
#                    method = "cosine")
#   
#   testm <- as.matrix(dfmatg)
#   
#   df <- as.data.frame(testm[8,1:7])
#   colnames(df) <- "correlation"
#   df <- tibble::rownames_to_column(df, "username")
#   
#   df$date <- seqdate
#   
#   df_all <- rbind(df_all, df)
# }

## replicating Schoonvelde

load("/Users/cbarrie6/Dropbox/My Mac (Unknown-VBDGQ05F)/Desktop/dataverse_files/Validation/Data/euspeech_validation.Rdata")

unique(euspeech.corpus$speaker)

persons <- c("G. Brown", "J.L.R. Zapatero", "D. Cameron", "M. Rajoy")
test_corpus <- euspeech.corpus %>%
  filter(speaker %in% persons) %>%
  select(speaker, text)

saveRDS(test_corpus, "data/comparison-complexity/speeches.rds")


test_corpus$flesch.kincaid <- textstat_readability(test_corpus$text, measure = "Flesch.Kincaid")

# returned as quanteda data.frame with document-level information;
# need just the score:
test_corpus$flesch.kincaid <- test_corpus$flesch.kincaid$Flesch.Kincaid

sum_corpus <- test_corpus %>%
  dplyr::group_by(speaker) %>%
  dplyr::summarise(mean = mean(flesch.kincaid, na.rm=TRUE),
                   SD=sd(flesch.kincaid, na.rm=TRUE),
                   N=length(speaker))

sum_corpus$se <- sum_corpus$SD / sqrt(sum_corpus$N)
sum_corpus$min <- sum_corpus$mean - 1.96*sum_corpus$se
sum_corpus$max <- sum_corpus$mean + 1.96*sum_corpus$se

ggplot(sum_corpus, aes(x=speaker, y=mean)) +
  geom_bar(stat="identity") + 
  geom_errorbar(ymin=sum_corpus$min,ymax=sum_corpus$max, width=.2) +
  coord_flip() +
  xlab("") +
  ylab("Mean Complexity") + 
  theme_minimal() + 
  ylim(c(0,20)) + 
  guides(fill=FALSE)
