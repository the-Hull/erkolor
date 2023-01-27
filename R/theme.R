#' ERK Theme
#'
#' @param base_family 'sans', 'serif', 'mono' or any installed font, like 'Open Sans Light'
#' @param base_size numeric, defaults to 10 points.
#'
#' @return ggproto object
#' @export
theme_erk <- function(base_family = 'sans', base_size = 10){
  font <- base_family   #assign font family up front

  theme_minimal(base_family = base_family) %+replace%    #replace elements we want to change

    theme(

      plot.title.position = "plot",
      plot.caption.position = "plot",
      # plot.caption = element_text(size = rel(3.5), hjust = 1),
      # plot.caption = ggtext::element_textbox_simple(
      #   size = 14,
      #   halign = 0,
      #   lineheight = 1.25,
      #   hjust = 0,
      #   padding = margin(t = 5, 0, 5, 15)
      # ),

      plot.margin = margin(t = 0.25, r = 0.25, l = 0.25, b = 0.25, unit = "cm"),


      panel.background = element_rect(
        fill = "white",
        color = "white"
      ),
      plot.background = element_rect(
        fill = "white",
        color = "white"
      ),

      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = '#CCCCCC'),

      text = element_text(
        color = "#666666",
        family = base_family
      ),


      axis.text = element_text(
        color = "#666666"
      ),
      # axis.text.y = element_blank(),
      # axis.text.x = element_blank(),
      #
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15, l = 5), angle = 90)
    )
}
