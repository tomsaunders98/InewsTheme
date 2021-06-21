library(grid)
library(extrafont)
library(scales)
library(Cairo)
library(stats)
library(ggplot2)

#' Rolling Average
#' @param n The number of days to execute rolling average over
ra <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 2)}

#' Inews Pallette
inews_pal <- function() {
  values <- c("#E33A11","#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#fdbf6f","#cab2d6","#6a3d9a", "#ffff99", "#b15928")
  max_n <- length(values)
  f <- scales::manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

#'Inews Colour scale
scale_colour_inews <- function(...) {
  discrete_scale("colour", "inews", inews_pal(), ...)
}

#'Inews fill scale
scale_fill_inews <- function(...) {
  discrete_scale("fill", "inews", inews_pal(), ...)
}

#' Inews basic theme
#'
#' @param base_size Basic size of graph, defaults to 25
#' @param base_family base font family, not implemented
theme_inews_basic <- function(base_size = 25, base_family="") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      #Plot/general

      plot.margin = margin(t = 10,r = 10, b = 10,l =10, unit = "pt"),

      #Format Legend
      legend.title=element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.spacing = unit(0, "points"),
      legend.key.size = unit(1, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.margin = margin(t = 0,r = 0, b = 0,l =0, unit = "pt"),
      legend.box.margin = margin(t = 5,r = 0, b = 0,l =0, unit = "pt"),
      legend.box.spacing = unit(4, "points"),
      legend.text = element_text(size = rel(0.9)),


      #Formatting axis
      axis.title = element_blank(),
      axis.text = element_text(size = rel(0.7), margin=margin(0,0,0,0, unit="pt")),
      axis.ticks = element_line(size = rel(0.7), colour = "#878787"),
      axis.ticks.y = element_blank(),
      axis.ticks.length = unit(5, "points"),
      axis.line.x = element_line(colour = "#878787", size = rel(0.5)),

      #Format backgrounds + panels
      panel.background = element_rect(linetype = 0),
      panel.border = element_blank(),
      panel.grid.major = element_line(colour = "#e0e1e2", size = rel(0.5)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      #formatting text
      plot.title = element_text(size = rel(1.5), colour = "#000000", family = "Bitter", face="bold", hjust = 0),
      text = element_text(size=rel(1), family="Bitter", colour = "#898a8c"),
      plot.subtitle = element_text(size = rel(1.1), colour = "#525354", family="Bitter", hjust=0, margin=margin(t=5)),
      plot.caption = element_text(family="Bitter", colour = "#898a8c", size = rel(0.5), hjust = 0)

    )

}

#' Inews ggparlaiment theme
#'
#' @param base_size Basic size of graph, defaults to 25
#' @param base_family base font family, not implemented
theme_inews_parl <- function(base_size = 25, base_family="") {
  theme_void(base_size = base_size, base_family = base_family) %+replace%
    theme(
      #Format Legend
      legend.title = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.spacing = unit(0, "points"),
      legend.key.size = unit(1, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt"),
      legend.box.margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt"),
      legend.box.spacing = unit(4, "points"),
      legend.text = element_text(size = 10),
      #Format plot (with margins)
      plot.title.position = "panel",
      plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "cm"),

      #Format titles/text formatting
      plot.title = element_text(size = rel(15), colour = "#000000", family = "Bitter", face = "bold", hjust = 0),
      text = element_text(size = rel(1), family = "Bitter", colour = "#898a8c"),
      plot.subtitle = element_text(size = rel(10), colour = "#525354", family = "Bitter", hjust = 0, margin=margin(t=5)),
      plot.caption = element_text(family = "Bitter", colour = "#898a8c", size = rel(0.5), hjust = 0)
      )


}

theme_inews_facet <- function(base_size = 25, base_family="") {
  theme_inews_basic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust=0.5)
    )
}


#' Inews map theme
#'
#' @param base_size Basic size of graph, defaults to 25
#' @param base_family base font family, not implemented
theme_inews_map <- function(base_size = 25, base_family=""){
  theme_void(base_size = 25, base_family = "") %+replace%
    theme(
      #Format legend
      legend.title = element_blank(),
      legend.position = "right",
      legend.direction = "vertical",
      legend.justification = "right",
      legend.spacing = unit(0, "points"),
      legend.key.size = unit(2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      legend.box.spacing = unit(4, "points"),
      legend.text = element_text(size = rel(0.9)),

      #Format text elements
      plot.title = element_text(size = rel(1.5), colour = "#000000", family = "Bitter", face = "bold",  hjust = 0),
      text = element_text(size = rel(1), family = "Bitter", colour = "#898a8c"),
      plot.subtitle = element_text(size = rel(1.1), colour = "#525354", family = "Bitter", hjust = 0, margin=margin(t=5)),
      plot.caption = element_text(family = "Bitter", colour = "#898a8c", size = rel(0.5), hjust = 0),

      #Add small margin
      plot.margin = margin(t = 10,r = 10, b = 10,l =10, unit = "pt")
    )
}

#' Save plots (ggsave wrapper)
#' @param filename The filename specified
#' @param plot The plot to render, defaults to last plot
#' @param width_i Specified width in cm, defaults to 15
#' @param height_i Specified height in cm, defaults to 10
#' @param type Adds presets for maps/parls and other types to render
#' @param l_size Enables to turn off limiting size
save_inews <- function(filename, plot=last_plot(), width_i = 15, height_i = 10, type="basic", l_size=TRUE){
  cap_all <- ggplot_build(plot)
  ################ Auto Formatting Legends ###############
  plotAndPrintRatio <- function(g, width, height) {
    gGrob <- ggplotGrob(g)
    tmpfile <- tempfile(pattern = "png")
    png(tmpfile, width = width, height = height, units="cm", res=300) # it is necessary to open a device
    plot(g)
    legendSize <- as.numeric(convertWidth(grobWidth(gGrob$grobs[[15]]), unitTo = "inches"))
    plotSize <-   as.numeric(convertWidth(grobWidth(gGrob$grobs[[7]]), unitTo = "inches"))
    # the ratio of legend size to plot size
    dev.off()
    return(legendSize / plotSize)
  }
  val <- plotAndPrintRatio(plot, width_i, height_i)

  if(val > 1){
    rows = ceiling(val)
    pos_tps <- names(cap_all[[3]][[9]])
    if ("colour" %in% pos_tps){
      plot <- plot + guides(colour = guide_legend(nrow = rows))
    }
    if ("fill" %in% pos_tps){
      plot <- plot + guides(fill = guide_legend(nrow = rows))
    }
    if ("size" %in% pos_tps){
      plot <- plot + guides(size = guide_legend(nrow = rows))
    }
    if ("linetype" %in% pos_tps){
      plot <- plot + guides(linetype = guide_legend(nrow = rows))
    }
    if ("alpha" %in% pos_tps){
      plot <- plot + guides(alpha = guide_legend(nrow = rows))
    }
  }




  # Add copyright

  cap <-  cap_all[[3]][[9]]$caption
  cap <- paste(cap, "By Tom Saunders", sep="\n")
  newcap <- paste(cap, "© The I", sep="\n")
  plot <- plot +
    labs(caption = newcap)

  # Add presets:
  if (type == "map"){
    height_i = 25
    width_i = 25
  } else if (type == "parl"){
    height_i= 20
    width_i = 20
  }

  ggsave(filename, plot, dpi = 300, type = "cairo", width = width_i, height = height_i, units = "cm", limitsize = l_size)

}


