library(scales)
library(stats)
library(ggplot2)
library(ggtext)
library(directlabels)
library(stringr)
library(systemfonts)
library(ragg)
library(svglite)
library(RColorBrewer)
library(rcartocolor)

#' Simple preloaded functions
#' Updates ggplot defaults +  imports relevant fonts to registry + sets helpful global variables
.onLoad <- function(libname, pkgname) {
  # to show a startup message
  library(magrittr)
  library(directlabels)
  ggplot2::update_geom_defaults("text", list(family = "Rubik-SemiBold", colour="#000000", size = 2.5))
  ggplot2::update_geom_defaults("label", list(family = "Rubik-SemiBold", colour="#000000", size = 2.5))
  ggplot2::update_geom_defaults("line", list(colour="#E35D3B"))
  ggplot2::update_geom_defaults("point", list(colour="#E35D3B"))
  ggplot2::update_geom_defaults("col", list(fill="#E35D3B"))
  ggplot2::update_geom_defaults("sf", list(colour="#ffffff", size=0.1))
  #North Pole Lambert Azimuthal Equal Area projection
  lambert <<- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

  ## Loading in fonts
  c_fonts <- as.data.frame(systemfonts::registry_fonts()) %>%
    dplyr::filter(stringr::str_detect(family, "Bitter|Rubik"))

  if(nrow(c_fonts) == 0){
    all_fonts <- as.data.frame(systemfonts::system_fonts()) %>%
      dplyr::filter(family %in% c("Bitter", "Rubik") & !(stringr::str_detect(path, "AppData")))

    for (x in 1:nrow(all_fonts)) {
      if (all_fonts[x, "name"] != all_fonts[x, "family"]){
        systemfonts::register_font(
          name = all_fonts[x, "name"],
          plain = all_fonts[x, "path"]
        )
      }
    }
  }
}


#' Inews labelling
#' @param label passes aes to directlabels to allow for quicker line labelling
#' @param size allows to increase size of labels
#' @param type Horizontal labels for bar charts, vertical for lines
#' @export
labels_inews <- function(label, size = 0.6, type="line"){
  if(type == "bar"){
    ggplot2::geom_text(aes(label = {{label}}), hjust=0)
  }else{
    directlabels::geom_dl(aes(label = {{label}}), method = list(directlabels::dl.trans(x = x + 0.2), cex=size, fontfamily = "Rubik-Regular", "last.points"))
  }
}



#' Rolling Average
#' @param n The number of days to execute rolling average over
#' @export
ra <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}

#' Inews Pallette
#' @param palette A collection of different palettes, along with discrete palette to be used easily with scale_inews_ferm() + Colorbrewer/cartocolr palettes
#' @param length the number of colours for the palette, passed automatically from break length
#' @param direction The direction of palette, passed from scale_inews_ferm
#' @export
inews_pal <- function(palette = "qual", length = NA, direction = 1) {
  if(stringr::str_detect(palette, "::")){
    lib <- stringr::str_extract(palette, ".+(?=::)")
    pal <- stringr::str_extract(palette, "(?<=::).+")
    if(lib == "RColorBrewer"){
      values <- RColorBrewer::brewer.pal(length, pal)
    } else if (lib == "rcartocolor"){
      values <- rcartocolor::brewer.pal(length, pal)
    }
  } else{
    if (palette == "qual"){
      values <- c("#E35D3B","#5c909d","#f88379","#4EBA60","#F29F05","#03A6A6","#D35F9F", "#8EC720",  "#ee7800", "#0388a6", "#856eb4",  "#368F1B")
    } else if (palette == "seq_good"){
      values <- RColorBrewer::brewer.pal(length, "PuBu")
    } else if (palette == "seq_bad"){
      values <- RColorBrewer::brewer.pal(length, "OrRd")
    } else if (palette == "diverg"){
      values <- RColorBrewer::brewer.pal(length, "RdBu")
      values <- replace(values, values=="#F7F7F7", "#f8f6e9") ## Get rid of ugly white in middle,
    } else if (palette == "mint") {
      values <- rcartocolor::carto_pal(length, "Mint")
    } else if (palette == "teal") {
      values <- rcartocolor::carto_pal(length, "Teal")
    }
  }

  if(direction == -1){
    values <- rev(values)
  }
  max_n <- length(values)
  f <- scales::manual_pal(values)
  attr(f, "max_n") <- max_n
  f
}

#'Inews Colour scale
#' @export
scale_colour_inews <- function(...) {
  discrete_scale("colour", "inews", inews_pal(), ...)
}

#'Inews fill scale
#' @export
scale_fill_inews <- function(...) {
  discrete_scale("fill", "inews", inews_pal(), ...)
}




#' Inews main theme
#' @param base_size Basic size of graph, defaults to 25
#' @param base_family base font family, not implemented
#' @param fill Enables invisible PNG background as well as grey/white background
#' @export
theme_inews <- function(base_size = 25, base_family="", fill="white") {
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
      plot.margin = margin(t = 10, b = 10, r = 10, unit = "pt"),
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
      legend.text = element_text(size = rel(0.6), colour = "#000000", family="Rubik-SemiBold"),


      #Formatting axis
      axis.title = element_blank(),
      axis.text = element_text(size = rel(0.6), hjust=0, family = "Rubik-SemiBold"),
      axis.text.x = element_text(margin = margin(t=1, unit = "pt")),
      axis.ticks = element_line(size = rel(0.3), colour = "#878787"),
      axis.ticks.y = element_blank(),
      axis.ticks.length = unit(3, "points"),
      axis.line = element_line(colour = "#878787", size = rel(0.3)),
      axis.line.y = ggplot2::element_blank(),

      #Format panels
      panel.background = element_rect(linetype = 0, fill=f_val),
      panel.border = element_blank(),
      panel.grid.major = element_line(colour = "#e0e1e2", size = rel(0.3)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      ##Facet text format
      strip.text.x = element_text(size = rel(0.8), colour = "#000000", family = "Bitter-SemiBold", hjust = 0.5),
      strip.text.y = element_blank(),

      #formatting text
      plot.title = element_text(size = rel(2), colour = "#000000", family = "Bitter-Black", hjust = 0.5),
      text = element_text(size=rel(1), family="Rubik-SemiBold", colour = "#898a8c"),
      plot.subtitle = element_text(size = rel(1), colour = "#9C9C9C", family="Bitter-Regular", hjust=0.5, margin=margin(t=8, b=5, unit="pt")),
      plot.caption = ggtext::element_markdown(family="Rubik-Regular", colour = "#898a8c", size = rel(0.5), hjust = 0, margin=margin(t=5, unit="pt")),
      complete = TRUE
    )

}





#' Inews map theme
#' @param fill Enables invisible, sea-esq, grey or white backgrounds to maps
#' @param direcrete boolean determines size of legend if variable is discrete
#' @param base_size Basic size of graph, defaults to 25
#' @param base_family base font family, not implemented
#' @export
theme_inews_map <- function(base_size = 25, base_family="", fill="White", discrete = FALSE){
  if(fill == "none"){
    f_val = NA
  }else if(fill == "grey") {
    f_val = "#f0f0f0"
  } else if (fill == "sea") {
    f_val = "#afdef2"
  } else{
    f_val = "#ffffff"
  }
  if(discrete == TRUE){
    l_width = 1
  } else{
    l_width = 5
  }
  theme_void(base_size = 25, base_family = "") %+replace%
    theme(
      #Format legend
      legend.title = element_blank(),

      legend.position = "top",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.key.height = unit(1, "lines"),
      legend.key.width = unit(l_width, "lines"),
      legend.box.margin = margin(t = 10,unit = "pt"),
      legend.spacing = unit(0, "points"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      legend.box.spacing = unit(1, unit = "pt"),
      legend.text = element_text(size = rel(0.9), colour = "#000000", family="Rubik-SemiBold"),

      #Format plot
      plot.margin = margin(t = 10,b = 10,unit = "pt"),
      plot.background = element_rect(fill = f_val, colour=NA),

      #Format text elements
      plot.title.position = "panel",
      plot.title = element_text(size = rel(3.5), colour = "#000000", family = "Bitter-Black", hjust = 0.5),
      text = element_text(size=rel(1), family="Rubik-SemiBold", colour = "#898a8c"),
      plot.subtitle = element_text(size = rel(1.5), colour = "#9C9C9C", family="Bitter-Regular", hjust=0.5, margin=margin(t=8, b=5, unit="pt")),
      plot.caption = ggtext::element_markdown(family="Rubik-Regular", colour = "#898a8c", size = rel(1), hjust = 1, margin=margin(t=5, unit="pt")),
      complete = TRUE
    )
}

#' Save plots (ggsave wrapper)
#' @param filename The filename specified
#' @param plot The plot to render, defaults to last plot
#' @param width Specified width in cm, defaults to 15
#' @param height Specified height in cm, defaults to 11
#' @param units Specift units for width and height
#' @param type Adds presets for different graph types or different places on website
#' @param margin Add margins to plot
#' @export
save_inews <- function(filename, plot=last_plot(), width = 15, height = 11, type="basic", units="cm", margin="none"){




  ## Adding label space
  if(margin != "none" & is.numeric(margin)){
    plot <- plot +
      theme(
        plot.margin = margin(r = margin, unit = "cm")
        )
    width = width + (margin - 1)
  }

  ## Presets for different graph types
  if (type == "map"){
    height = 18
    width = 25
  } else if (type == "facet"){
    height = 20
  } else if (type == "fimage"){
    units = "px"
    height = 360
    width = 640
    ## Change the  text size
    plot <- plot +
      theme(
        legend.text = element_text(size = rel(0.7)),
        axis.text = element_text(size = rel(0.7)),
        plot.title = element_text(size = rel(2)),
        text = element_text(size=rel(2)),
        plot.subtitle = element_text(size = rel(0.8)),
        plot.caption = ggtext::element_markdown(size = rel(0.4)),
      )
  } else if (type == "box"){
    units = "px"
    height = 800
    width = 1440
    ## Change the  text size
    plot <- plot +
      theme(
        legend.text = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        plot.title = element_text(size = rel(4)),
        text = element_text(size=rel(2)),
        plot.subtitle = element_text(size = rel(2)),
        plot.caption = ggtext::element_markdown(size = rel(1)),
      )
  } else if (type == "fimage_map"){
    units = "px"
    height = 360
    width = 640
    ## Change the  text size
    plot <- plot +
      theme(
        legend.text = element_text(size = rel(0.5)),
        plot.title = element_text(size = rel(2), hjust=0.5),
        text = element_text(size=rel(2)),
        plot.subtitle = element_text(size = rel(0.8), hjust=0.5),
        legend.justification = "center",
        plot.caption = ggtext::element_markdown(size = rel(0.4)),
        legend.key.width = unit(3, "lines"),
      )
  }

  ##Build plot
  built_plot <- ggplot2::ggplot_build(plot)

  # Add copyright
  if(!(type %in% c("box", "fimage", "fimage_map"))){
    cap <-  built_plot$plot$labels$caption
    newcap <- paste(cap, "<br>By Tom Saunders <span style='font-family:Rubik'> · ©</span><span style='font-family:Bitter; color:#E33A11;'><b> i</b></span> ", sep="")
    built_plot$plot$labels$caption = newcap
  }

  ##Wrapping subtitle
  subtitle <- built_plot$plot$labels$subtitle
  if (!is.null(subtitle)){
    if (units == "px"){
      wrap_m <- 0.1
    } else{
      wrap_m <- 5
    }
    new_sub <- stringr::str_wrap(subtitle, wrap_m*width)
    built_plot$plot$labels$subtitle = new_sub
  }

  ## Turn clipping off
  if(built_plot$plot$coordinates$clip == "on"){
    built_plot$plot$coordinates$clip = "off"
  }

  # Set device from filename
  device = stringr::str_extract(filename, "(?<=\\.).+")
  if (!(device %in% c("svg", "png"))) {
    return(message("Only svg or png"))
  }

  ##convert back to plot
  plot <- ggplot2::ggplot_gtable(built_plot)
  ## Render graph
  if(device == "png"){
      ggsave(filename, plot, device = ragg::agg_png(width = width, height = height, units = units), limitsize = FALSE)
  }
  if(device == "svg"){
    ggsave(filename, plot, dpi = 300, width = width, height = height, units = units, limitsize = FALSE)
  }
}


#' Internal function to enable raster on coloursteps, seemingly not possible natively in ggplot2
guide_coloursteps_inews <- function(even.steps = TRUE, show.limits = NULL, ticks = FALSE, ...) {
  guide <- ggplot2::guide_colourbar(raster = TRUE, ticks = ticks,  ...)
  guide$even.steps <- even.steps
  guide$show.limits <- show.limits
  class(guide) <- c('colorsteps', class(guide))
  guide
}

#' Fermenter scale for maps
#' @param palette palette string from inews_pal
#' @param break must be specified, determines when to break map distitions
#' @param direction direction of palette, passed to inews_pal
#' @param labels add labels if % etc.
#' @param na.value Na value, defaults to light grey
#' @export
scale_inews_ferm <- function(palette = palette, breaks = breaks, direction = 1, labels = waiver(), na.value = "#DCDCDC", ...){
  # Retrieve palette
  length = length(breaks) + 1
  colours <- inews_pal(palette, length = length, direction)

  binned_scale("fill",
               "foo",
               ggplot2:::binned_pal(colours),
               guide=guide_coloursteps_inews(
                     even.steps = FALSE,
                     show.limits = FALSE
                   ),
               breaks = breaks,
               na.value = na.value,
               labels = labels
  )
}



