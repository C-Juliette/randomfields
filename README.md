
<!-- README.md is generated from README.Rmd. Please edit that file -->

# randomfields

<!-- badges: start -->

[![R-CMD-check](https://github.com/C-Juliette/randomfields/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/C-Juliette/randomfields/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/C-Juliette/randomfields/branch/main/graph/badge.svg)](https://codecov.io/gh/C-Juliette/randomfields?branch=main)
[![test-coverage](https://github.com/C-Juliette/randomfields/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/C-Juliette/randomfields/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

Le principe est de générer des champs aléatoires en 2D, puis de calculer
leurs corrélations.

Un champ en 2D est un ensemble de valeurs sur une grille (x ,y). Ces
valeurs sont, dans notre cas, des réalisations de variables aléatories.

Le but de ce package est de donner des outils pour calculer la
corrélation entre deux variables aléatoires situées en un point (x_1,
y_1) et une autre en un point (x_2, y_2).

On peut montrer qu’en appliquant des moyennes glissantes à des champs
générés par des variables aléatoires indépendantes, on obtient des
champs structurés et des variables aléatoires dépendantes.

This package considers randomly generated matrix as maps. It plots the
maps and indicates the variance and the distribution of a realization.
It does moving average to generate new maps which are spatially
structured. It calculates the covariance between points on spatial
structured maps. And it subsamples maps, with or without an average of
the neighbored points.

## Getting started

You can install the package randomfields from github :

``` r
devtools::install_github("")
```
