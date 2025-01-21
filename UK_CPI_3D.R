# ============================================================================= #
#
library(ggplot2)
library(tidyverse)
library(sf)
library(data.table)
library(rayshader)
#
UK = read_sf("NUTS1_Jan_2018_UGCB_in_the_UK_2022.geojson")
UK_inf = fread("UK_infl_reg.csv")
UK_inf_DEC = t(tail(UK_inf, 1)[,-c(1:4)])
UK_order = c(9,8,7,5,6,4,1,2,3,10,11,12)
UK$INF = UK_inf_DEC[UK_order]
#
theme_custom = theme_void() + 
               theme(plot.margin = margin(1,1,10,1,"pt"),
                     plot.background = element_rect(fill="white",color=NA))
#
UK_3D = ggplot(UK) +
          geom_sf(aes(fill = INF), 
                  lwd = 0.75, 
                  color = "black") +
          scale_fill_fermenter(
                  name = " ",
                  breaks = seq(2.2, 3.7, 0.25),
                  direction = 1,
                  palette = "YlGnBu") +
          labs(title = "UK Regional Inflation",
               subtitle = "ONS Micro CPI, DEC 2024",
               caption = "by Artur Semeyutin") +
          theme_custom
#
phivechalf = 30 + 60 * 1/(1 + exp(seq(-8, 20, length.out = 180)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = seq(0, 359, length.out = 360)
zoomvec = 0.4 + 0.15 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull = c(zoomvec, rev(zoomvec))
#
plot_gg(UK_3D, multicore = TRUE, width = 4, height = 5, 
        fov = 70, offset_edges =  TRUE)
render_movie(filename = "./UK_INF.gif", type = "custom", frames = 360, 
             phi = phivecfull, zoom = zoomvecfull, theta = thetavec)
#
# ============================================================================= #