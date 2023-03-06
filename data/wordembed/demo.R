library(janeaustenr)
library(dplyr)
library(quanteda)

text <- janeaustenr::prideprejudice
sentences <- text[10:11]
sentences <- paste(sentences[1], sentences[2], sep = " ")
text <- text[10:13030]

toks_austen <- tokens(text)

fcm(toks_austen, context = "window", window = 6, ordered = F)

sentence <- paste(sentences, sentences)

toks_austen <- tokens(sentence)

fcm(toks_austen, context = "window", window = 6, ordered = F, tri =F)

sentence
