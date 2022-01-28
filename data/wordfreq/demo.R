string <- "we are all very happy to be at a lecture at 10AM"

tokens(string)

ntoken(string)

dfm(tokens(string))

stringr::str_count(string, '\\w+')

string2 <- "We are all very happy to be at a lecture at 10AM I am furious to be at a lecture at 10AM why am I awake before the sun where is the sun I need some vitamin D is there a Boots nearby what was he saying just then hooray it's over"

tokens(string2)

ntoken(string2)

dfm(tokens(string2))

