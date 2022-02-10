library(stringi)
library(dplyr)
library(tidytext)
library(ggplot2)
library(scales)

## Varying total happy word numbers example

lipsum_text <- data.frame(text = stri_rand_lipsum(5000, start_lipsum = TRUE))

lipsum_text$happyn <- as.character("")
lipsum_text$happyu <- as.character("")
lipsum_text$happyd <- as.character("")

lipsum_text$week <- as.integer(rep(seq.int(1:10), 5000/10))
lipsum_text$student <- sample(1:30,5000, replace = T)

for(i in 1:nrow(lipsum_text)) {
  week <- lipsum_text[i,5]
  happyn <- paste(rep("happy", times = sample(1:100, 1)), collapse = " ")
  happyu <- paste(rep("happy", times = sample(1:100, 1)*week), collapse = " ")
  happyd <- paste(rep("happy", times = sample(1:100, 1)*1/week), collapse = " ")
  lipsum_text[i, 2] <- happyn
  lipsum_text[i, 3] <- happyu
  lipsum_text[i, 4] <- happyd
}

lipsum_text$happyn <- paste(lipsum_text$text, lipsum_text$happyn)
lipsum_text$happyu <- paste(lipsum_text$text, lipsum_text$happyu)
lipsum_text$happyd <- paste(lipsum_text$text, lipsum_text$happyd)

happyn <- lipsum_text %>% 
  select(happyn, week, student) %>%
  unnest_tokens(word, happyn) %>%
  group_by(week, student) 
happyu <- lipsum_text %>% 
  select(happyu, week, student) %>%
  unnest_tokens(word, happyu) %>%
  group_by(week, student) 
happyd <- lipsum_text %>% 
  select(happyd, week, student) %>%
  unnest_tokens(word, happyd) %>%
  group_by(week, student)

happyn$happy <- as.integer(grepl("happy", x = happyn$word))
happyu$happy <- as.integer(grepl("happy", x = happyu$word))
happyd$happy <- as.integer(grepl("happy", x = happyd$word))

happys <- list(happyn, happyu, happyd)

plots=list()

for (i in seq_along(happys)) {
  
  happy <- happys[[i]]
  
  plots[[i]] <- happy %>%
    group_by(week, student) %>%
    mutate(index_total = n()) %>%
    filter(happy==1) %>%
    summarise(sum_hap = sum(happy),
              index_total = index_total,
              prop_hap = sum_hap/index_total) %>%
    distinct() %>%
    ggplot() +
    geom_point(aes(week, prop_hap)) +
    geom_smooth(aes(week, prop_hap)) +
    scale_x_continuous(breaks= pretty_breaks())
}

do.call("grid.arrange", c(plots, ncol = 2))


## Varying total word numbers example

lipsum_text$lipsumn <- as.character("")
lipsum_text$lipsumu <- as.character("")
lipsum_text$lipsumd <- as.character("")

for(i in 1:nrow(lipsum_text)) {
  week <- lipsum_text[i,5]
  lipsumn <- paste(rep(stri_rand_lipsum(1, start_lipsum = TRUE), 
                       times = 1), collapse = " ")
  lipsumu <- paste(rep(stri_rand_lipsum(1, start_lipsum = TRUE), 
                       times = sample(1:10,1)*week), collapse = " ")
  lipsumd <- paste(rep(stri_rand_lipsum(1, start_lipsum = TRUE), 
                       times = sample(1:10,1)*1/week), collapse = " ")
  lipsum_text[i, 7] <- lipsumn
  lipsum_text[i, 8] <- lipsumu
  lipsum_text[i, 9] <- lipsumd
}

lipsum_text$happylipsumn <- paste(lipsum_text$lipsumn, lipsum_text$happyn)
lipsum_text$happylipsumu <- paste(lipsum_text$lipsumu, lipsum_text$happyn)
lipsum_text$happylipsumd <- paste(lipsum_text$lipsumd, lipsum_text$happyn)

happylipsumn <- lipsum_text %>% 
  select(happylipsumn, week, student) %>%
  unnest_tokens(word, happylipsumn) %>%
  group_by(week, student) 
happylipsumu <- lipsum_text %>% 
  select(happylipsumu, week, student) %>%
  unnest_tokens(word, happylipsumu) %>%
  group_by(week, student) 
happylipsumd <- lipsum_text %>% 
  select(happylipsumd, week, student) %>%
  unnest_tokens(word, happylipsumd) %>%
  group_by(week, student) 

happylipsumn$happy <- as.integer(grepl("happy", x = happylipsumn$word))
happylipsumu$happy <- as.integer(grepl("happy", x = happylipsumu$word))
happylipsumd$happy <- as.integer(grepl("happy", x = happylipsumd$word))

lipsums <- list(happylipsumn, happylipsumu, happylipsumd)

plots=list()

for (i in seq_along(lipsums)) {
  
  lipsum <- lipsums[[i]]
  
  plots[[i]] <- lipsum %>%
    group_by(week, student) %>%
    mutate(index_total = n()) %>%
    filter(happy==1) %>%
    summarise(sum_hap = sum(happy),
              index_total = index_total,
              prop_hap = sum_hap/index_total) %>%
    distinct() %>%
    ggplot() +
    geom_point(aes(week, prop_hap)) +
    geom_smooth(aes(week, prop_hap)) +
    scale_x_continuous(breaks= pretty_breaks()) +
      theme(axis.text.y=element_text(size=20),
            axis.text.x=element_text(size=20),
            axis.title.x=element_text(size=20, face="bold"),
            axis.title.y=element_text(size=20, face="bold"))
}

do.call("grid.arrange", c(plots, ncol = 2))