# Title     : PlotFunc.R
# Objective : functions for custom plots : ECE, Distributions, Tippett, DET
# Created by : Marco De Donno (2015)
# last version : mjacquet (29.10.2020)

######## DATA FORM
# csv 2 col
# (headers)     h | lr
#              hp | value
#               . | value
#               . | value
#              hd | value
#               . | value
#               . | value
########

library( MASS )
library( gplots )
library( survival )
library( comparison )
library( fitdistrplus )
library( ggplot2 )
library( gridExtra )
library( isotone )
library( plyr )
library( reshape2 )
library( ROCR )
library( scales )
library( magrittr )
library(tidyr)
library(dplyr)

# Get custom theme for ggplot2
source("C:/Users/mjacquet/Scripts/myTheme.R")

par(cex.axis = 12)

############## DISTRIBUTIONS PLOT ##############

distriPlot = function( data, title = NULL, ylab = "Density", xlab = "LLR",  lims = NULL, ymax = NULL, ribbon = TRUE, calibrate = FALSE )
{
  data <- na.omit(data)

  data = data %>% subset( select = c( "h", "lr" ) )
  
  if( calibrate !=  FALSE )
  {
    datacal = data.calibrate( data )
    
    tmp = datacal$h
    tmp = ifelse( tmp  ==  "hp", "scal", "dcal" )
    
    if( calibrate  ==  "pava" )
    {
      tmp = cbind( tmp, datacal$pavaLR )
    } else if( calibrate  ==  "logistic" ) {
      tmp = cbind( tmp, datacal$logisticLR )
    } else {
      print( "calibration not correct" )
      return( NULL )
    }
    
    tmp = as.data.frame( tmp )
    names( tmp ) = c( "h", "lr" )
    
    data = rbind( data, tmp )
    
  }
  
  data$lr = data$lr %>% as.character %>% as.numeric
  data$loglr = data$lr %>% log10
  
  densS = density( subset( data, h == "hp" )$loglr )
  densD = density( subset( data, h == "hd" )$loglr )
  
  dens = rbind( 
    data.frame( h = "hp", x = densS$x, y = densS$y ),
    data.frame( h = "hd", x = densD$x, y = densD$y )
  )
  
  if( is.null( lims ) )
  {
    noninf = subset( data, !is.infinite( loglr ) )
    lims = c( min( noninf$loglr ) - 0.5 , max( noninf$loglr ) + 0.5, 0, ceiling( max( dens$y ) * 10 ) / 10 )
  }
  
  if( !is.null( ymax ) )
    lims[4] = ymax
  
  if( calibrate !=  FALSE ) {
    
    densScal = density( subset( data, h == "scal" )$loglr )
    densDcal = density( subset( data, h == "dcal" )$loglr )
    
    densCal = rbind( 
      data.frame( h = "scal", x = densScal$x, y = densScal$y ),
      data.frame( h = "dcal", x = densDcal$x, y = densDcal$y )
    )
    
  }
  
  ret = ggplot( dens, aes( x = x, y = y, linetype = h ) ) +
    myTheme() +
    
    geom_line( size = 1 )+
    
    geom_hline( yintercept = 0 ) +
    geom_vline( xintercept = 0, linetype = "dotted" )+
    scale_linetype_manual(values=c("solid", "dashed"))
  
  
  if( calibrate !=  FALSE )
    ret = ret + geom_line( inherit.aes = F, data = densCal, aes( x = x, y = y, color = h ), size = 1.1, linetype = "dashed" )
  
  ret = ret +    
    xlab( xlab %>% iconv( "UTF-8" ) ) + ylab( ylab %>% iconv( "UTF-8" ) ) +

    xlab( "LLR" ) + ylab( "Density" ) +
    
    xlim( lims[1], lims[2] ) +
    ylim( lims[3], lims[4] )
  
  if( ribbon  ==  TRUE & nrow( subset( dens, h  ==  "hp" & x <= 0 ) ) !=  0 )
    ret = ret + geom_ribbon( data = subset( dens,h == "hp" & x <= 0 ), aes( ymax = y ), ymin = 0, fill = "#1e1e1e", colour = NA, alpha = 0.2 )
  
  if( ribbon  ==  TRUE & nrow( subset( dens, h  ==  "hd" & x >=  0 ) ) !=  0 )
    ret = ret + geom_ribbon( data = subset( dens,h == "hd" & x >= 0 ), aes( ymax = y ), ymin = 0, fill = "grey", colour = NA, alpha = 0.2 )
  
  if( !is.null( title ) )
    ret = ret + ggtitle( title %>% iconv( "UTF-8" ) )
  
  return( ret )
}


############## ECE ##############

ecePlot = function( data, title = NULL, lims = NULL, ymax = NULL )
{

  data <- na.omit(data)

  if( is.null( lims ) )
    lims = c( -4, 4, 0, 1 )
  
  if( !is.null( ymax ) )
    lims[4] = ymax
  
  data$h = data$h %>% as.character
  data$lr = data$lr %>% as.character %>% as.numeric
  
  s = subset( data, h == "hp" )$lr
  d = subset( data, h == "hd" )$lr
  
  ece = calc.ece( s, d, prior = seq( from = 0.0001, to = 0.9999, length = 1000 ) )
  
  dataece = as.data.frame( cbind( prior = ece$prior, obs = ece$ece, null = ece$ece.null, cal = ece$ece.cal ) )
  dataece$prior = log10( dataece$prior / ( 1-dataece$prior ) )
  dataece = melt( dataece, id = c( "prior" ) )
  
  cllr = round( ece$ece[500], digits = 3 )
  cllrmin = round( ece$ece.cal[500], digits = 3 )
  cllrcal = cllr - cllrmin
  
  eceplot = ggplot( dataece, aes( x = prior, y = value, colour = factor( variable ), linetype = factor( variable ), size = variable ) ) +
    
    myTheme() +
    
    geom_line( ) +
    
    xlab( "Logit( P( H1 ) )" ) + ylab( "ECE" ) +
    
    geom_vline( xintercept = 0, linetype = "dotted" ) +
    geom_hline( yintercept = 0 ) +
    
    scale_color_manual( values = c( "#000000","#000000","#000000" ) ) +
    scale_linetype_manual(values = c("longdash", "dotted", "dotted")) +
    scale_size_manual(values = c(1, 1, 2.2)) +
    
    xlim( lims[1], lims[2] ) +
    ylim( lims[3], lims[4] ) +
    
    annotate( "text", label = paste( "Cllr : ", cllr, "\nCllr min : ", cllrmin, "\nCllr cal : ", cllrcal, sep = "" ), x = lims[2], y = lims[4], vjust = 1, hjust = 1, cex = 5)
  
  if( !is.null( title ) )
    eceplot = eceplot + ggtitle( title )
  
  return( eceplot )
}


eceindhp = function( lr, prior )
  return( prior * log2( 1 + ( 1 / ( lr * ( prior / ( 1 - prior ) ) ) ) ) )

cllrhp = function( lr )
  return( eceindhp( lr, 0.5 ) )


eceindhd = function( lr, prior )
  return( ( 1 - prior ) * log2( 1 + ( lr * ( prior / ( 1 - prior ) ) ) ) )

cllrhd = function( lr )
  return( eceindhd( lr, 0.5 ) )



############## RMEP/RMED ##############

rmeprmed = function( data, xlab = "LLR", ... )
{
  data <- na.omit(data)
  rmep = sum( subset( data, h == "hd" )$lr > 1 ) / sum( data$h  ==  "hd" ) * 100
  rmed = sum( subset( data, h == "hp" )$lr < 1 ) / sum( data$h  ==  "hp" ) * 100
  
  rmep = round( rmep, digits = 2 )
  rmed = round( rmed, digits = 2 )
  
  ret = distriPlot( data, xlab = xlab, ... ) +
    myTheme() +
    
    annotate( "text", label = paste( "  RMEP : ", rmep, " %      ", "\nRMED : ", rmed, " %      ", sep = "" ), x = Inf, y = Inf, vjust = 2, hjust = 1, size = 5 )
  
  return( ret )
}


############## DET CURVE ##############

detcurve = function( data )
{
  data <- na.omit(data)
  pred = prediction( data$lr, data$h )
  perf = performance( pred, "fnr", "fpr" )
  
  det = data.frame( x = perf@x.values, y = perf@y.values )
  names( det ) = c( "x","y" )
  
  eer = round( det[ order( abs( det$x - det$y ), decreasing = F ), ][1,1] * 100, 2 )
  
  bk = c( 0.001, 0.01, 0.1, 0.5, 1 )
  
  ret = ggplot( det, aes( x = x, y = y ) ) +
    myTheme() +
    
    geom_abline( intercept = 0, slope = 1, linetype = "dotted" ) +
    
    geom_line( size = 1 ) +
    
    xlab( "FAR" ) +
    ylab( "FRR" ) +
    
    scale_x_log10( breaks = bk ) +
    scale_y_log10( breaks = bk ) +
    
    annotate( "text",label = paste( "EER : ", eer, " %", sep = "" ), x = 0.7, y = 0.8, vjust = 0, hjust = 1, size = 5 )
  
  return( ret )
}


############## TIPPETT PLOT ##############

tippettplot = function( data, min = NULL, max = NULL, calibrate = NULL )
{
  data <- na.omit(data)
  if( is.null( calibrate ) )
    data$loglr = data$lr %>% log10
  else if( calibrate == "logistic" )
    data$loglr = data.calibrate( data )$logisticLogLR
  else if( calibrate == "pava" )
    data$loglr = data.calibrate( data )$pavaLogLR
  
  ccdf <- data %>% group_by(h) %>% mutate(ccdf = 1 - ecdf(loglr)(loglr))

  if( is.null( min ) )
    min = min( data$loglr )
  
  if( is.null( max ) )
    max = max( data$loglr )
  
  rmep = sum( subset( data, h  ==  "hd", select = loglr ) > 0 ) / nrow( data %>% subset( h == "hd" ) ) * 100
  rmed = sum( subset( data, h  ==  "hp", select = loglr ) < 0 ) / nrow( data %>% subset( h == "hp" ) ) * 100
  
  rmep = rmep %>% round( digits = 2 )
  rmed = rmed %>% round( digits = 2 )

  
  ret = ggplot( ccdf, aes( x = loglr,y = ccdf,color = h ) ) +
    
    myTheme() +
    
    geom_hline( yintercept = 0 ) +
    geom_step( size = 1.1, direction = "hv" ) +
    geom_vline( xintercept = 0, linetype = "longdash" ) +
    
    geom_hline( yintercept = 1 ) +
    
    xlab( expression( "Log10( LR )" ) ) +
    ylab( "CCDF" ) +
    
    xlim( min, max ) +
    
    scale_color_manual( values = c( colintra, colinter ) ) +

    annotate( "text", label = paste( "RMEP : ", rmep, "%" ), x = max, y = 0.99, vjust = 1, hjust = 1, size = 7 ) +
    annotate( "text", label = paste( "RMED : ", rmed, "%" ), x = min, y = 0.01, vjust = 0, hjust = 0, size = 7 )

  return( ret )
}


####################################################################################
####################################################################################