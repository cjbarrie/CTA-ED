library(dplyr)
library(quanteda) # includes functions to implement Lexicoder
library(quanteda.textmodels) # for estimating similarity and complexity measures
library(quanteda.textplots) 


tweets  <- readRDS(gzcon(url("https://github.com/cjbarrie/CTA-ED/blob/main/data/sentanalysis/newstweets.rds?raw=true")))


tweets <- tweets %>%
  sample_n(20000)


#make corpus object, specifying tweet as text field
tweets_corpus <- corpus(tweets, text_field = "text")

#add in username document-level information
docvars(tweets_corpus, "newspaper") <- tweets$user_username

dfm_tweets <- dfm(tokens(tweets_corpus),
                  remove_punct = TRUE, 
                  remove = stopwords("english"))

dfm_newstweets <- dfm_group(dfm_tweets, groups = newspaper)
# remove words not used by two or more newspapers
dfm_newstweets <- dfm_trim(dfm_newstweets, 
                           min_docfreq = 2, docfreq_type = "count")

## size of the document-feature matrix
dim(dfm_newstweets)

#### estimate the Wordfish model ####
set.seed(123L)
dfm_newstweets_results <- textmodel_wordfish(dfm_newstweets, 
                                             sparse = TRUE)