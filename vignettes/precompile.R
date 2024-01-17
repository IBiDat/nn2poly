# move to the vignettes directory
old_dir <- setwd("vignettes")

# generate static Rmds
knitr::knit("source/nn2poly-01-introduction.Rmd.orig", "nn2poly-01-introduction.Rmd")
knitr::knit("source/nn2poly-02-supported-DL-frameworks.Rmd.orig", "nn2poly-02-supported-DL-frameworks.Rmd")
knitr::knit("source/nn2poly-03-classification-example.Rmd.orig", "nn2poly-03-classification-example.Rmd")

# restore
setwd("old_dir")
