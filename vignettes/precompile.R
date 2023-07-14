# generate static Rmds
knitr::knit("vignettes/source/nn2poly-01-introduction.Rmd.orig", "vignettes/nn2poly-01-introduction.Rmd")
knitr::knit("vignettes/source/nn2poly-02-tensorflow-regression.Rmd.orig", "vignettes/nn2poly-02-tensorflow-regression.Rmd")
knitr::knit("vignettes/source/nn2poly-03-tensorflow-classification.Rmd.orig", "vignettes/nn2poly-03-tensorflow-classification.Rmd")
# knitr::knit("vignettes/source/nn2poly-03-data.Rmd.orig", "vignettes/nn2poly-03-data.Rmd")
# knitr::knit("vignettes/source/nn2poly-04-tests.Rmd.orig", "vignettes/nn2poly-04-tests.Rmd")

# move figures under the vignettes dir
file.copy("figure", "vignettes", recursive=TRUE)
unlink("figure", recursive=TRUE, force=TRUE)

