# generate static Rmds
knitr::knit("vignettes/source/nn2poly-01-introduction.Rmd", "vignettes/nn2poly-01-introduction.Rmd")
knitr::knit("vignettes/source/nn2poly-02-constraints.Rmd", "vignettes/nn2poly-02-constraints.Rmd")
knitr::knit("vignettes/source/nn2poly-03-data.Rmd", "vignettes/nn2poly-03-data.Rmd")
knitr::knit("vignettes/source/nn2poly-04-tests.Rmd", "vignettes/nn2poly-04-tests.Rmd")

# move figures under the vignettes dir
file.rename("figure", "vignettes/figure")
