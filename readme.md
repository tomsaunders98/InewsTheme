# Inews Theme


## Functions

##### Pallettes
  * (11 colours max):
    * Scale_colour_inews: Colours
    * scale_fill_inews: fills
    
##### Themes
  * theme_inews_basic 
    * Line graphs
    * Bar charts
    * Area charts
    * Histograms
    * Dumbells (might need to adjust colours for this, specify a manual scale)
    * __Notes__
      * Use __legend.position = "none"__ for single line/bar charts
      
  * theme_inews_parl
    * Used with __geom_parliament_seats()__ from ggparlaiment package
  * theme_inews_map
    * Used with __geom_sf()__ and possible other map plots
    
##### Other
  * save_inews
    * Uses ggsave with most common settings:
      * width = 15 (cm)
      * height = 15 (cm)
      
      

