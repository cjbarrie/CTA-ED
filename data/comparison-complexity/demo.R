library(readr) # more informative and easy way to import data
library(quanteda) # includes functions to implement Lexicoder
library(quanteda.textstats) # for estimating similarity and complexity measures
library(stringdist) # for basic character-based distance measures
library(dplyr) #for wrangling data
library(tibble) #for wrangling data
library(ggplot2) #for visualization

tweets  <- readRDS(gzcon(url("https://github.com/cjbarrie/CTA-ED/blob/main/data/comparison-complexity/cabinet_tweets.rds?raw=true")))


#make corpus object, specifying tweet as text field
tweets_corpus <- corpus(tweets, text_field = "tweet")

#add in username document-level information
docvars(tweets_corpus, "username") <- tweets$username

tweets_corpus

dfmat <- dfm(tokens(tweets_corpus),
             remove_punct = TRUE, 
             remove = stopwords("english"))

corrmat <- dfmat %>%
  dfm_group(groups = username) %>%
  textstat_simil(margin = "documents", method = "correlation")

corrmat[1:5,1:5]

#estimate similarity, grouping by username

cos_sim <- dfmat %>%
  dfm_group(groups = username) %>%
  textstat_simil(margin = "documents", method = "cosine") #specify method here as character object

cosmat <- as.matrix(cos_sim) #convert to a matrix


cosmatdf <- as.data.frame(cosmat[23, c(1:22, 24)])

colnames(cosmatdf) <- "corr_may"

#create column variable from rownames
cosmatdf <- tibble::rownames_to_column(cosmatdf, "username")