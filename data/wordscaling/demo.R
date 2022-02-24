devtools::install_github("conjugateprior/austin")
library(austin)

# inspect function
classic.wordscores

data(lbg)
ref <- getdocs(lbg, 1:5)
ws <- classic.wordscores(ref, scores=seq(-1.5,1.5,by=0.75))

summary(ws)
vir <- getdocs(lbg, 'V1') 
predict(ws, newdata=vir)

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