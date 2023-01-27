# List of Color Palettes and the order in which they are printed


#' Complete list of palettes.
#'
#' Use names(erkPalettes) to return all possible palette names. Current choices are:
#' \code{erk}, \code{erksemi}, \code{erklight}.
#' Use \code{\link{erk.brewer}} to construct palettes.
#'
#' @export
erkPalettes <- list(
  erk = list(c("#de3614", "#823d1c", "#964d80", "#667a94", "#575757", "#337d12", "#66c440"), c(4, 3, 7, 1, 5, 2, 6), colorblind=FALSE),
  erksemi = list(c("#DE361480", "#823D1C80", "#964D8080", "#667A9480", "#57575780", "#337D1280", "#66C44080"), c(4, 3, 7, 1, 5, 2, 6), colorblind=FALSE),
  erklight = list(c("#DE361433", "#823D1C33", "#964D8033", "#667A9433", "#57575733", "#337D1233", "#66C44033"), c(4, 3, 7, 1, 5, 2, 6), colorblind=FALSE)
)

# Function for generating palettes

#' ERK Palette Generator
#'
#' Color palettes based on ERK CI Style Guide
#' Use \code{\link{colorblind.friendly}} to check whether palettes are colorblind-friendly.
#'
#' @param palette Name of Palette. Choices are:
#' \code{erk}, \code{erksemi}, \code{erklight}
#' @param n Number of desired colors. If number of requested colors is beyond the scope of the palette,
#' colors are automatically interpolated. If n is not provided, the length of the palette is used.
#' @param type Either "continuous" or "discrete". Use continuous if you want to automatically
#' interpolate between colors.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @return A vector of colors.
#' @examples
#' erk.brewer("erksemi")
#'
#' erk.brewer("erklight", direction=-1)
#'
#' erk.brewer("erk", 4, override.order=TRUE)
#'
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Species, y=Petal.Length, fill=Species)) +
#' geom_violin() +
#' scale_fill_manual(values=erk.brewer("erk", 3))
#'
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
#' geom_point(size=2) +
#' scale_color_manual(values=erk.brewer("erk", 3))
#'
#' ggplot(data=iris, aes(x=Species, y=Sepal.Width, color=Sepal.Width)) +
#' geom_point(size=3) +
#' scale_color_gradientn(colors=erk.brewer("erk"))
#' @keywords colors
#' @export
erk.brewer <- function(palette_name, n, type = c("discrete", "continuous"), direction = c(1, -1), override.order=FALSE) {

  `%notin%` <- Negate(`%in%`)

  palette <- erkPalettes[[palette_name]]

  if (is.null(palette)|is.numeric(palette_name)){
    stop("Palette does not exist.")
  }

  if (missing(n)) {
    n <- length(palette[[1]])
  }

  if (missing(direction)) {
    direction <- 1
  }

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  if (missing(type)) {
    if(n > length(palette[[1]])){type <- "continuous"}
    else{type <- "discrete"}
  }

  type <- match.arg(type)


  if (type == "discrete" && n > length(palette[[1]])) {
    stop("Number of requested colors greater than what discrete palette can offer, \n use continuous instead.")
  }

  continuous <-  if(direction==1){grDevices::colorRampPalette(palette[[1]])(n)
  }else{
    grDevices::colorRampPalette(rev(palette[[1]]))(n)}

  discrete <- if(direction==1 & override.order==FALSE){
    palette[[1]][order(palette[[2]][1:n])]
  }else if(direction==-1 & override.order==FALSE){
    rev(palette[[1]][order(palette[[2]][1:n])])
  } else if(direction==1 & override.order==TRUE){
    palette[[1]][palette[[2]][1:n]]
  } else{
    rev(palette[[1]][palette[[2]][1:n]])
  }

  out <- switch(type,
                continuous = continuous,
                discrete = discrete
  )
  structure(out, class = "palette", name = palette_name)

}

# Function for printing palette

#' @export
#' @importFrom grDevices rgb
#' @importFrom graphics rect par image text

print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.92, n + 1, 1.08, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 2.5, family = "serif")
}




# erkolor palettes for plotting with ggplot2

#' erkolor palettes for plotting with ggplot2
#'
#' Function for using \code{erkolor} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_erk_d}} and \code{\link{scale_fill_erk_d}}
#' for discrete scales and \code{\link{scale_color_erk_c}} and \code{\link{scale_fill_erk_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' #' \code{erk}, \code{erksemi}, \code{erklight}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
#' geom_point() +
#' scale_color_erk_d("Juarez")
#' @export
scale_color_erk_d <- function(palette_name, direction=1, override.order=FALSE, ...){
  erk.brewer.disc <- function(palette_name, direction = c(1, -1), override.order=FALSE) {

    `%notin%` <- Negate(`%in%`)
    palette <- erkPalettes[[palette_name]]
    if (is.null(palette)|is.numeric(palette_name)){
      stop("Palette does not exist.")
    }

    if (direction %notin% c(1, -1)){
      stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
    }

    function(n)   discrete <- if(direction==1 & override.order==FALSE){
      palette[[1]][order(palette[[2]][1:n])]
    }else if(direction==-1 & override.order==FALSE){
      rev(palette[[1]][order(palette[[2]][1:n])])
    } else if(direction==1 & override.order==TRUE){
      palette[[1]][palette[[2]][1:n]]
    } else{
      rev(palette[[1]][palette[[2]][1:n]])
    }


  }



discrete_scale(aesthetics = "colour", scale_name="met_d",
               palette = erk.brewer.disc(palette_name=palette_name, direction=direction, override.order=override.order),
               ...)
}


#' erkolor palettes for plotting with ggplot2
#'
#' Function for using \code{erkolor} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_erk_d}} and \code{\link{scale_fill_erk_d}}
#' for discrete scales and \code{\link{scale_color_erk_c}} and \code{\link{scale_fill_erk_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{erk}, \code{erksemi}, \code{erklight}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Species, y=Sepal.Length, fill=Species)) +
#' geom_violin() +
#' scale_fill_erk_d("Lakota")
#' @export
scale_fill_erk_d <- function(palette_name, direction=1, override.order=FALSE, ...){
  erk.brewer.disc <- function(palette_name, direction = c(1, -1), override.order=FALSE) {

    `%notin%` <- Negate(`%in%`)
    palette <- erkPalettes[[palette_name]]
    if (is.null(palette)|is.numeric(palette_name)){
      stop("Palette does not exist.")
    }

    if (direction %notin% c(1, -1)){
      stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
    }

    function(n)   discrete <- if(direction==1 & override.order==FALSE){
      palette[[1]][order(palette[[2]][1:n])]
    }else if(direction==-1 & override.order==FALSE){
      rev(palette[[1]][order(palette[[2]][1:n])])
    } else if(direction==1 & override.order==TRUE){
      palette[[1]][palette[[2]][1:n]]
    } else{
      rev(palette[[1]][palette[[2]][1:n]])
    }
  }

  discrete_scale(aesthetics = "fill", scale_name="met_d",
                 palette = erk.brewer.disc(palette_name=palette_name, direction=direction, override.order=override.order),
                 ...)
}


#' erkolor palettes for plotting with ggplot2
#'
#' Function for using \code{erkolor} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_erk_d}} and \code{\link{scale_fill_erk_d}}
#' for discrete scales and \code{\link{scale_color_erk_c}} and \code{\link{scale_fill_erk_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{erk}, \code{erksemi}, \code{erklight}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ... Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Sepal.Length)) +
#' geom_point() +
#' scale_color_erk_c("Isfahan1", direction=-1)
#' @export
scale_color_erk_c <- function(palette_name, direction=1, ...){

  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  scale_color_gradientn(colors=erk.brewer(palette_name=palette_name, direction=direction, override.order = F),
                        ...)
}


#' erkolor palettes for plotting with ggplot2
#'
#' Function for using \code{erkolor} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_erk_d}} and \code{\link{scale_fill_erk_d}}
#' for discrete scales and \code{\link{scale_color_erk_c}} and \code{\link{scale_fill_erk_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{erk}, \code{erksemi}, \code{erklight}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ... Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}
#' @import ggplot2
#' @export
scale_fill_erk_c <- function(palette_name, direction=1, ...){

  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  scale_fill_gradientn(colors=erk.brewer(palette_name=palette_name, direction=direction, override.order = F),
                       ...)
}


#' erkolor palettes for plotting with ggplot2
#'
#' Function for using \code{erkolor} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_erk_d}} and \code{\link{scale_fill_erk_d}}
#' for discrete scales and \code{\link{scale_color_erk_c}} and \code{\link{scale_fill_erk_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{erk}, \code{erksemi}, \code{erklight}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
#' geom_point() +
#' scale_colour_erk_d("Juarez")
#' @export

scale_colour_erk_d <- scale_color_erk_d

#' erkolor palettes for plotting with ggplot2
#'
#' Function for using \code{erkolor} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_erk_d}} and \code{\link{scale_fill_erk_d}}
#' for discrete scales and \code{\link{scale_color_erk_c}} and \code{\link{scale_fill_erk_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{erk}, \code{erksemi}, \code{erklight}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ... Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Sepal.Length)) +
#' geom_point() +
#' scale_colour_erk_c("Isfahan1", direction=-1)
#' @export

scale_colour_erk_c <- scale_color_erk_c



#' View all Palettes available
#'
#' Function for viewing all palettes available in erkolor.
#'
#' @param n Number of requested colors. If n is left blank, default palette is returned.
#' @param colorblind_only Should only colorblind friendly palettes be returned? Default is set to FALSE.
#' @param sequential Should palettes displayed all at once, or one at a time. Default is all at once (FALSE).
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @examples
#' # All Palettes
#' display_all(sequential = FALSE, colorblind_only = FALSE)
#'
#' # All Colorblind Palettes
#' display_all(sequential = FALSE, colorblind_only = TRUE)
#'
#' # 5 Colors of all Palettes
#' display_all(5, sequential = FALSE, colorblind_only = FALSE)
#' @export
#' @importFrom graphics rect par layout polygon


display_all <- function(n, sequential = FALSE, colorblind_only = FALSE, direction = 1, override.order=FALSE){
  if(colorblind_only){
    N = length(colorblind_palettes)
    pal_names = colorblind_palettes
  }else{
    N = length(erkPalettes)
    pal_names = names(erkPalettes)
  }

  orig_pars <- par()

  plot_palette = function(name,n){
    par(mar = c(0.1,0.1,1,0.1))
    nn = ifelse(missing(n), length(erk.brewer(name)), n)
    plot(0,type='n',bty='n',xaxt='n',yaxt='n',xlab='',ylab='',
         ylim = c(0,1),xlim=c(0,nn), main = name)
    for(j in 1:nn){
      polygon(x = c(j-1,j-1,j,j),
              y = c(0,1,1,0),
              border = NA,
              col = erk.brewer(name, nn, direction= direction,override.order=override.order)[j])
    }
  }

  if(sequential){
    for(i in 1:N){

      if(missing(n)){

        plot_palette(pal_names[i])
        if(i < N) cat("Hit 'Enter' for next palette");readline()

      }else{

        plot_palette(pal_names[i],n)
        if(i < N) cat("Hit 'Enter' for next palette");readline()
      }
    }
  }else{

    if(missing(n)){

      if(colorblind_only){

        layout(matrix(1:N,6,4))
        for(i in 1:N) plot_palette(pal_names[i])

      }else{

        layout(matrix(1:N,8,7))
        for(i in 1:N) plot_palette(pal_names[i])
      }

    } else{

      if(colorblind_only){

        layout(matrix(1:N,6,4))
        for(i in 1:N) plot_palette(pal_names[i],n)

      }else{

        layout(matrix(1:N,8,7))
        for(i in 1:N) plot_palette(pal_names[i],n)

      }

    }

    layout(matrix(1,1,1))
    par(mar = orig_pars$mar)

  }
}

