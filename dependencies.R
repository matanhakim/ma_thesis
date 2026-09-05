# Packages that the project uses outside of R code, listed here so that renv
# records them in renv.lock: Quarto renders the document with knitr and
# rmarkdown, figures are drawn with the ragg device (set in _quarto.yml), and
# the targets pipeline renders the document through the quarto package.
library(knitr)
library(rmarkdown)
library(ragg)
library(quarto)
