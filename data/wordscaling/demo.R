devtools::install_github("conjugateprior/austin")
library(austin)

# inspect function
classic.wordscores

data(lbg)
ref <- getdocs(lbg, 1:5)

ref
ws <- classic.wordscores(ref, scores=seq(-1.5,1.5,by=0.75))
summary(ws)

#get "virgin" documents
vir <- getdocs(lbg, 'V1')
vir
predict(ws, newdata=vir)

# inspect function
wordfish

dd <- sim.wordfish()

dd

wf <- wordfish(dd$Y)
summary(wf)

wfm <- dd$Y