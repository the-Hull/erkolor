% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/erkpalettes.R
\name{scale_colour_erk_c}
\alias{scale_colour_erk_c}
\title{erkolor palettes for plotting with ggplot2}
\usage{
scale_colour_erk_c(palette_name, direction = 1, ...)
}
\arguments{
\item{palette_name}{Name of Palette. Choices are:
\code{erk}, \code{erksemi}, \code{erklight}.}

\item{direction}{Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed}

\item{...}{Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}}
}
\description{
Function for using \code{erkolor} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_erk_d}} and \code{\link{scale_fill_erk_d}}
for discrete scales and \code{\link{scale_color_erk_c}} and \code{\link{scale_fill_erk_c}} for continuous scales.
}
\examples{
library(ggplot2)
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Sepal.Length)) +
geom_point() +
scale_colour_erk_c("Isfahan1", direction=-1)
}
