# move to the vignettes directory
old_dir <- setwd("vignettes")

# generate static Rmds
knitr::knit("source/nn2poly-01-introduction.orig.Rmd", "nn2poly-01-introduction.Rmd")
knitr::knit("source/nn2poly-02-supported-DL-frameworks.orig.Rmd", "nn2poly-02-supported-DL-frameworks.Rmd")
knitr::knit("source/nn2poly-03-classification-example.orig.Rmd", "nn2poly-03-classification-example.Rmd")

# restore
setwd(old_dir)
