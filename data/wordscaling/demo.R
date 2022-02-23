devtools::install_github("conjugateprior/austin")
library(austin)

# inspect function
classic.wordscores

wordfish

wfm

dd <- sim.wordfish()
wf <- wordfish(dd$Y)
summary(wf)

wfm <- dd$Y

wordfish(
  dd$Y,
  dir = c(1, length(docs(dd$Y))),
  control = list(tol = 1e-06, sigma = 3, startparams = NULL, conv.check = c("ll",
                                                                            "cor")),
  verbose = FALSE
)