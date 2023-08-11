
<!-- README.md is generated from README.Rmd. Please edit that file -->

# randomfields

<!-- badges: start -->

[![.github/workflows/R-CMD-check.yaml](https://github.com/C-Juliette/randomfields/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/C-Juliette/randomfields/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/C-Juliette/randomfields/branch/main/graph/badge.svg)](https://codecov.io/gh/C-Juliette/randomfields?branch=main)
[![test-coverage](https://github.com/C-Juliette/randomfields/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/C-Juliette/randomfields/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

– *English version below* –

Le principe est de générer des champs aléatoires en 2D, puis de calculer
leurs corrélations.  
Un champ en 2D est un ensemble de valeurs sur une grille (x ,y). Ces
valeurs sont, dans notre cas, des réalisations de variables
aléatoires.  
Le but de ce package est de donner des outils pour calculer la
corrélation entre deux variables aléatoires : une située en un point
(x<sub>1</sub>, y<sub>1</sub>) et une autre située en un point
(x<sub>2</sub>, y<sub>2</sub>).  
On peut montrer qu’en appliquant des moyennes glissantes à des champs
générés par des variables aléatoires indépendantes, on obtient des
champs structurés et des variables aléatoires dépendantes.

– *English version* –

This package considers randomly generated matrix as maps. It plots the
maps and indicates the variance and the distribution of a realization.
It does moving average to generate new maps which are spatially
structured. It calculates the covariance between points on spatial
structured maps. And it subsamples maps, with or without an average of
the neighbored points.

More precisely :

The principle is to generate random fields in 2D and then calculate
their correlations.  
A 2D field is a set of values on a grid (x, y). In our case, these
values are realizations of random variables.  
The purpose of this package is to provide tools for calculating the
correlation between two random variables: one located at a point
(x<sub>1</sub>, y<sub>1</sub>) and another located at a point
(x<sub>2</sub>, y<sub>2</sub>).  
It can be shown that by applying moving averages to fields generated by
independent random variables, structured fields and dependent random
variables are obtained.

## Getting started

You can install the package randomfields from github :

``` r
devtools::install_github("https://github.com/C-Juliette/randomfields")
```
