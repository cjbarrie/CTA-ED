library(dplyr)
library(stringr)

twts_corpus_sample <- MPtweets %>%
  sample_n(100000) %>%
  select(date, username, party_value, tweet, reftweet_username)

twts_corpus_sample$ment_extract <- str_extract_all(twts_corpus_sample$tweet, "@\\S+")

twts_corpus_sample <- twts_corpus_sample %>%
  mutate(ments = sapply(ment_extract, toString))

twts_corpus_sample <- twts_corpus_sample %>%
  sample_n(100000) %>%
  select(date, username, party_value, tweet, reftweet_username, ments)


saveRDS(twts_corpus_sample,"data/wordembed/twts_corpus_sample.rds")
