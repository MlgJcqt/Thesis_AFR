# Title     : plot_data.R
# Objective : Plot data from functions in PlotFunc.R
# Created by : mjacquet
# last version : 12.11.2020

######## DATA FORM
# csv 2 col
# (headers)     h | lr
#              hp | value
#               . | value
#               . | value
#              hd | value
#               . | value
#               . | value

# --> " dataX " = dataframe of lr values

########

# Get functions in PlotFunc.R
source("C:/Users/mjacquet/Scripts/PlotFunc.R")

# Set output directory
plotDir <- "C:/Users/mjacquet/R_Plots/"

# plot grid for 3 sets of data to compare
png( filename=paste0( plotDir, "/RMEP_RMED.png" ), width=2400, heigh=800 )
grid.arrange(

  rmeprmed( data1, lims = c(-12,20) ),
  rmeprmed( data2, lims = c(-12,20) ),
  rmeprmed( data3, lims = c(-12,20) ),

  ncol = 3
)
dev.off( )

png( filename=paste0( plotDir, "/DET.png" ), width=2400, heigh=800 )
grid.arrange(

  detcurve( data1 ),
  detcurve( data2 ),
  detcurve( data3 ),

  ncol = 3
)
dev.off( )

png( filename=paste0( plotDir, "/ECE.png" ), width=2400, heigh=800 )
grid.arrange(

  ecePlot( data1 ),
  ecePlot( data2 ),
  ecePlot( data3 ),

  ncol = 3
)
dev.off()

#while (dev.cur()>1) dev.off()

####################################################################################
####################################################################################