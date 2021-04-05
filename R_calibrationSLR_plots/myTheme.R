# Title     : myTheme.R
# Objective : Custom theme for ggplot2
# Created by : mjacquet
# Last version : 12.11.2020

# Get in scripts as :
# source("C:/Users/mjacquet/Scripts/myTheme.R")

myTheme = function( )
{theme(panel.background = element_rect(fill = NA), 
        axis.line = element_line(color="black"),
	   	#legend.position = "bottom",
	   	#legend.title = element_blank(),
        legend.position = "none",
        # legend.title=element_blank(),
        # legend.text=element_text(size=22),
        element_line(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title=element_text(hjust=0.5, margin = margin(t = 0, r = 0, b = 40, l = 0)),
        axis.title = element_text(size = 16))
}

####################################################################################
####################################################################################