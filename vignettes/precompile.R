# generate static Rmds
knitr::knit("vignettes/source/nn2poly-01-introduction.Rmd.orig", "vignettes/nn2poly-01-introduction.Rmd")
knitr::knit("vignettes/source/nn2poly-02-supported-DL-frameworks.Rmd.orig", "vignettes/nn2poly-02-supported-DL-frameworks.Rmd")
knitr::knit("vignettes/source/nn2poly-03-classification-example.Rmd.orig", "vignettes/nn2poly-03-classification-example.Rmd")

# move figures under the vignettes dir
file.copy("figure", "vignettes", recursive=TRUE)
unlink("figure", recursive=TRUE, force=TRUE)

