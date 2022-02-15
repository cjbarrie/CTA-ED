library(stringi)
library(dplyr)
library(tidytext)
library(ggplot2)
library(scales)
library(kableExtra)

lipsum_text <- data.frame(text = stri_rand_lipsum(1, start_lipsum = TRUE))

head(lipsum_text$text)

tokens <- lipsum_text %>%
  unnest_tokens(word, text)

head(tokens)

## Varying total words example

lipsum_text <- data.frame(text = stri_rand_lipsum(5000, start_lipsum = TRUE))

# make some weeks one to ten
lipsum_text$week <- as.integer(rep(seq.int(1:10), 5000/10))

# simulate increasing words trend

for(i in 1:nrow(lipsum_text)) {
  week <- lipsum_text[i, 2]
  morewords <-
    paste(rep("more lipsum words", times = sample(1:100, 1) * week), collapse = " ")
  lipsum_words <- lipsum_text[i, 1]
  new_lipsum_text <- paste0(morewords, lipsum_words, collapse = " ")
  lipsum_text[i, 1] <- new_lipsum_text
}

lipsum_text %>%
  unnest_tokens(word, text) %>%
  group_by(week) %>%
  count(word) %>%
  select(week, n) %>%
  distinct() %>%
  ggplot() +
  geom_bar(aes(week, n), stat = "identity") +
  labs(x = "Week", y = "n words") +
  scale_x_continuous(breaks= pretty_breaks())

# simulate decreasing words trend

## Varying total words example

lipsum_text <- data.frame(text = stri_rand_lipsum(5000, start_lipsum = TRUE))

# make some weeks one to ten
lipsum_text$week <- as.integer(rep(seq.int(1:10), 5000/10))

for(i in 1:nrow(lipsum_text)) {
  week <- lipsum_text[i,2]
  morewords <- paste(rep("more lipsum words", times = sample(1:100, 1)* 1/week), collapse = " ")
  lipsum_words <- lipsum_text[i,1]
  new_lipsum_text <- paste0(morewords, lipsum_words, collapse = " ")
  lipsum_text[i,1] <- new_lipsum_text
}

lipsum_text %>%
  unnest_tokens(word, text) %>%
  group_by(week) %>%
  count(word) %>%
  select(week, n) %>%
  distinct() %>%
  ggplot() +
  geom_bar(aes(week, n), stat = "identity") +
  labs(x = "Week", y = "n words") +
  scale_x_continuous(breaks= pretty_breaks())

# normalized word frequencies

lipsum_text %>%
  unnest_tokens(word, text) %>%
  count(word, sort = T) %>%
  top_n(5) %>%
  knitr::kable(format="html")%>% 
  kable_styling("striped", full_width = F)

# let's look for "sed"

lipsum_totals <- lipsum_text %>%
  group_by(week) %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  mutate(total = sum(n)) %>%
  distinct(week, total)

lipsum_sed <- lipsum_text %>%
  group_by(week) %>%
  unnest_tokens(word, text) %>%
  filter(word == "sed")  %>%
  count(word) %>%
  mutate(total_sed = sum(n)) %>%
  distinct(week, total_sed)

lipsum_sed %>%
  left_join(lipsum_totals, by = "week") %>%
  mutate(sed_prop = total_sed/total) %>%
  ggplot() +
  geom_line(aes(week, sed_prop)) +
  labs(x = "Week", y = "
       Proportion sed word") +
  scale_x_continuous(breaks= pretty_breaks())


