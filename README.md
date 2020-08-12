<p align=
"right">
  <a href="https://github.com/anthonynorth/snapbox/releases/latest">
    <img src="https://img.shields.io/github/v/release/anthonynorth/snapbox?sort=semver&style=flat-square" alt="release">
  </a>
  <a href="https://www.tidyverse.org/lifecycle/#experimental">
    <img src="https://img.shields.io/badge/lifecycle-experimental-orange?style=flat-square" alt="lifecycle" />
  </a>
  <a href="https://travis-ci.com/anthonynorth/snapbox">
    <img src="https://img.shields.io/travis/com/anthonynorth/snapbox?style=flat-square" alt="build">
  </a>
</p>

<h1 align="center">snapbox</h1>

<p align="center">
  Static mapbox basemap for ggplot2.
</p>

## Installation

```r
remotes::install_github("anthonynorth/snapbox")
```

## Usage
```r
library(sf)
library(ggplot2)
library(snapbox)

area <- st_bbox(
  c(xmin = 147, ymin = -43, xmax = 147.7, ymax = -42.65),
  crs = 4326
)

ggplot() +
  layer_mapbox(area, scale_ratio = 0.5)
```
