library(grid)
library(extrafont)
library(scales)
library(Cairo)
library(stats)
library(ggplot2)
library(ggrepel)
library(paletteer)
library(ggtext)
library(directlabels)
library(stringr)

.onLoad <- function(libname, pkgname) {
  # to show a startup message
  ggplot2::update_geom_defaults("text", list(family = "St Ryde", colour="#898c89", size = 0.8))
  ggplot2::update_geom_defaults("label", list(family = "St Ryde", colour="#898c89", size = 0.8))
  ggplot2::update_geom_defaults("line", list(colour="#E33A11"))
  #font_import(prompt = FALSE, pattern = "Bitter|Stryde")
  extrafont::loadfonts(device = "win", quiet = TRUE)
}

inews_lab <- function(label, size=0.6, gap = 0.2){
  directlabels::geom_dl(aes(label = {{label}}), method = list(dl.trans(x = x + 0.2), cex=size, fontfamily = "Stag", "last.points"))
}



#' Rolling Average
#' @param n The number of days to execute rolling average over
ra <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}

#' Inews Pallette
inews_pal <- function() {
  values <- c("#E33A11","#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#fdbf6f","#cab2d6","#6a3d9a",  "#b15928", "#c6c623")
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
theme_inews_basic <- function(base_size = 25, base_family="", fill="white") {
  if(fill == "none") {
    f_val = NA
  }else if (fill == "grey"){
    f_val = "#f0f0f0"
  }else{
    f_val = "#ffffff"
  }
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      #Plot/general

      plot.margin = margin(t = 10,r = 10, b = 10,l =10, unit = "pt"),
      plot.background = element_rect(fill = f_val, colour=NA),
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
      legend.text = element_text(size = rel(0.6)),


      #Formatting axis
      axis.title = element_blank(),
      axis.text = element_text(size = rel(0.7), margin=margin(0,0,0,0, unit="pt"), family = "Stag"),
      axis.ticks = element_line(size = rel(0.7), colour = "#878787"),
      axis.ticks.y = element_blank(),
      axis.ticks.length = unit(5, "points"),
      axis.line = element_line(colour = "#878787", size = rel(0.5)),
      axis.line.y = ggplot2::element_blank(),

      #Format backgrounds + panels
      panel.background = element_rect(linetype = 0, fill=f_val),
      panel.border = element_blank(),
      panel.grid.major = element_line(colour = "#e0e1e2", size = rel(0.5)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      #formatting text
      plot.title = element_text(size = rel(2), colour = "#000000", family = "Bitter Black", hjust = 0.5),
      text = element_text(size=rel(1), family="Stag", colour = "#898a8c"),
      plot.subtitle = element_text(size = rel(0.8), colour = "#9C9C9C", family="Bitter Regular", hjust=0.5, margin=margin(t=8, b=5, unit="pt")),
      plot.caption = ggtext::element_markdown(family="Stag", colour = "#898a8c", size = rel(0.5), hjust = 0, margin=margin(t=5, unit="pt")),
      complete = TRUE
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
theme_inews_map <- function(base_size = 25, base_family="", fill="White"){
  if(fill == "White"){
    f_val = "#ffffff"
  }
  if(fill == "Grey") {
    f_val = "#f0f0f0"
  }
  theme_void(base_size = 25, base_family = "") %+replace%
    theme(
      #Format legend
      legend.title = element_blank(),
      plot.background = element_rect(fill = f_val, colour=NA),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.key.height = unit(2, "lines"),
      legend.key.width = unit(3, "lines"),
      legend.justification = "left",
      legend.box.margin = margin(t = 10, unit = "pt"),
      legend.spacing = unit(0, "points"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      legend.box.spacing = unit(4, "points"),
      legend.text = element_text(size = rel(0.9)),

      #Format text elements
      plot.title = element_text(size = rel(2), colour = "#000000", family = "Bitter", face = "bold",  hjust = 0),
      text = element_text(size = rel(1), family = "Bitter", colour = "#898a8c"),
      plot.subtitle = element_text(size = rel(1.3), colour = "#525354", family = "Bitter", hjust = 0, margin=margin(t=5)),
      plot.caption = element_markdown(family = "Bitter", colour = "#898a8c", size = rel(1), hjust = 0),

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
save_inews <- function(filename, plot=last_plot(), width = 15, height = 11, type="basic", units="cm", labs="none"){
  cap_all <- ggplot2::ggplot_build(plot)


  ## Presets for different graph types
  if (type == "map"){
    height_i = 25
    width_i = 25

  } else if (type == "parl"){
    height_i= 20
    width_i = 20
  }



  ## Adding label space
  if(labs != "none" & is.numeric(labs)){
    plot <- plot +
      theme(
        legend.position = "none",
        plot.margin = margin(r = labs, unit = "cm")
        )
    width = width + (labs - 1)
  }




  # Add copyright

  cap <-  cap_all[[3]][[9]]$caption
  newcap <- paste(cap, "<br>By Tom Saunders <span style='font-family:Arial'> · ©</span><span style='font-family:Bitter; color:#E33A11;'><b> i</b></span> ", sep="")
  plot <- plot +
    labs(caption = newcap)



  # Remove clipping if default coords
  if (plot$coordinates$default == T) {
    plot <- plot +
      coord_cartesian(clip = 'off')
  }

  # Set device from filename
  device = stringr::str_extract(filename, "(?<=\\.).+")






  if(device == "png"){
    ggsave(filename, plot, dpi = 300, type = "cairo", width = width, height = height, units = units, limitsize = FALSE)
  }
  if(device == "eps"){
    ggsave(filename, plot, dpi = 300, device=cairo_ps, width = width, height = height, units = units, limitsize = FALSE)

  }
  if(device == "svg"){
    ggsave(filename, plot, dpi = 300, device=svg, width = width, height = height, units = units, limitsize = FALSE)
  }
}


#' Update ggplot defaults for text etc. to Inews





#' Binned scale (scale_fill_fermenter wrapper with better gradients)
#' @breaks Either values to turn into breaks or custom breaks
#' @palette paleteer pallette
#' @direction Direction of colours
scale_inews_ferm <- function(palette = palette,breaks = breaks, direction= 1,type="discrete",labels = NA, na.value = "grey50", ...){
  if(type == "discrete"){
    colours <- as.vector(paletteer::paletteer_d(palette, length(breaks)+1))
  }
  if(type == "continuous"){
    colours <- as.vector(paletteer::paletteer_c(palette, length(breaks)+1))
  }
  if(direction == -1){
    colours <- rev(colours)
  }
  if(is.na(labels)){
    binned_scale("fill",
                 "foo",
                 ggplot2:::binned_pal(scales::manual_pal(colours)),
                 guide="coloursteps",
                 breaks = breaks,
                 na.value = na.value
    )
  }else{
    binned_scale("fill",
                 "foo",
                 ggplot2:::binned_pal(scales::manual_pal(colours)),
                 guide="coloursteps",
                 breaks = breaks,
                 na.value = na.value,
                 labels = labels
    )
  }

}



