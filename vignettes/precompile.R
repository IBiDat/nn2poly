# move to the vignettes directory
old_dir <- setwd("vignettes")

# generate static Rmds
knitr::knit("source/_nn2poly-01-introduction.Rmd", "nn2poly-01-introduction.Rmd")
knitr::knit("source/_nn2poly-02-supported-DL-frameworks.Rmd", "nn2poly-02-supported-DL-frameworks.Rmd")
knitr::knit("source/_nn2poly-03-classification-example.Rmd", "nn2poly-03-classification-example.Rmd")

# restore
setwd(old_dir)
