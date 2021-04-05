# Title     : CalibrationFunc.R
# Objective : Set of functions used to prepare data for calibration, to plot calibration charts
# Created by : C. Champod (29.10.2020)
# last version : mjacquet (19.01.2021)


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


# Get custom theme for ggplot2
source("C:/Users/mjacquet/Scripts/myTheme.R")


############## DATA CALIBRATION FUNCTION ##############
# PAVA
#Function to calibrate based on PAVA.
#It borrows the function comparison::calibrate.set()
data.calibrate.pava = function( data , method = "laplace"){
  
  s = subset( data, h == "hp" )$lr
  d = subset( data, h == "hd" )$lr
  
  # Calibration PAVA
  datacal = calibrate.set( s, d, method = method)
  return( datacal )
}

# LOG REG
#To get the calibrated LR in the same format as the PAVA calibrated LR
#a list LR.cal.ss and LR.cal.ds as obtained from calibrate.set()

data_to_list_LR2 <- function(LRres, colLR = "SLR.brut"){
  data1 <- as.data.frame(LRres) %>% subset(LRres$h == "hp")
  data2 <- as.data.frame(LRres) %>% subset(LRres$h == "hd")
  LR.ss <- data1 %>%
    select(all_of(colLR)) %>%
    unlist() %>% as.numeric %>%
    setNames(data1$image2)
  LR.ds <- data2 %>%
    select(all_of(colLR)) %>%
    unlist() %>% as.numeric %>%
    setNames(data2$image2)
  ouput <- list(LR.ss=LR.ss,LR.ds=LR.ds)
}


############## ECE PLOT FUNCTION ##############
#Functions to compute ECE plot
#Added the calibrated data directly in the parameter of the function to get ece plots

calc.ece2 <- function (LR.ss, LR.ds,
                       prior = seq(from = 0.01, to = 0.99, length = 99),
                       CalLR = CalLR){
  n.ss = length(LR.ss)
  n.ds = length(LR.ds)
  n.prior = length(prior)
  odds = prior/(1 - prior)
  LR.null.ss = rep(1, n.ss)
  LR.null.ds = rep(1, n.ds)
  cal.set = CalLR
  LR.cal.ss = cal.set$LR.cal.ss
  LR.cal.ds = cal.set$LR.cal.ds
  ECE = NULL
  ECE.null = NULL
  ECE.cal = NULL
  for (ctr in 1:n.prior) {
    bit.1 = prior[ctr]/n.ss
    bit.2a = log2(1 + (1/(LR.ss * odds[ctr])))
    bit.2b = log2(1 + (1/(LR.null.ss * odds[ctr])))
    bit.2c = log2(1 + (1/(LR.cal.ss * odds[ctr])))
    bit.3 = (1 - prior[ctr])/n.ds
    bit.4a = log2(1 + (LR.ds * odds[ctr]))
    bit.4b = log2(1 + (LR.null.ds * odds[ctr]))
    bit.4c = log2(1 + (LR.cal.ds * odds[ctr]))
    ECE[ctr] = (bit.1 * sum(bit.2a)) + (bit.3 * sum(bit.4a))
    ECE.null[ctr] = (bit.1 * sum(bit.2b)) + (bit.3 * sum(bit.4b))
    ECE.cal[ctr] = (bit.1 * sum(bit.2c)) + (bit.3 * sum(bit.4c))
  }
  result = list(prior = prior, ece.null = ECE.null, ece = ECE, 
                ece.cal = ECE.cal)
  class(result) = "ece"
  return(result)
}

ecePlot2 = function( data, title = NULL, lims = NULL, ymax = NULL, CalLR){
  if( is.null( lims ) )
    lims = c( -4, 4, 0, 2 )
  
  if( !is.null( ymax ) )
    lims[4] = ymax
  
  data$h = data$h %>% as.character
  data$lr = data$lr %>% as.character %>% as.numeric
  
  s = subset( data, h == "hp" )$lr
  d = subset( data, h == "hd" )$lr
  
  ece = calc.ece2( s, d, prior = seq( from = 0.0001, to = 0.9999, length = 1000 ),
                   CalLR = CalLR)
  
  dataece = as.data.frame( cbind( prior = ece$prior, obs = ece$ece, 
                                  null = ece$ece.null, cal = ece$ece.cal ) )
  dataece$prior = log10( dataece$prior / ( 1-dataece$prior ) )
  dataece = melt( dataece, id = c( "prior" ) )
  
  cllr = round( ece$ece[500], digits = 3 )
  cllrmin = round( ece$ece.cal[500], digits = 3 )
  cllrcal = cllr - cllrmin
  
  eceplot = ggplot( dataece, aes( x = prior, y = value, linetype = factor( variable ), size = variable ) ) +

    myTheme() +

    geom_line( ) +
    
    xlab( "logit(Pr(Hp))" ) + ylab( "ECE" ) +
    
    geom_vline( xintercept = 0, linetype = "dotted" ) +
    geom_hline( yintercept = 0 ) +
    
    scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
    scale_size_manual(values = c(0.8, 1, 1)) +
    
    xlim( lims[1], lims[2] ) +
    ylim( lims[3], lims[4] ) +
    
    annotate( "text",
              label = paste( "Cllr : ", cllr, "\nCllr min : ", cllrmin, "\nCllr cal : ", cllrcal, sep = "" ),
              x = lims[2], y = lims[4],
              vjust = 1, hjust = 1, cex = 5)

  
  if( !is.null( title ) )
    eceplot = eceplot + ggtitle( title )
  
  return( eceplot )
}

############## DISTRIBUTIONS PLOT FUNCTION ##############

#Function to compute the plot of distributions of LRs before and after calibration
distriPlot2 = function(data, datacal, CalLR, title = NULL, xlab = "Log10(LR)", ylab = "Density",
                       lims = c(-8,8), ymax = NULL, ribbon = FALSE){
  
  data = data %>% subset( select = c( "h", "lr" ) )
  
  data$lr = data$lr %>% as.character %>% as.numeric
  data$loglr = data$lr %>% log10
  
  densS = density( subset( data, h == "hp" )$loglr )
  densD = density( subset( data, h == "hd" )$loglr )
  
  dens = rbind( 
    data.frame( h = "hp", x = densS$x, y = densS$y ),
    data.frame( h = "hd", x = densD$x, y = densD$y )
  )
  
  densScal = density(log10(CalLR$LR.cal.ss))
  densDcal = density(log10(CalLR$ LR.cal.ds))

  densSScal <- data.frame( h = "hp_cal", x = densScal$x, y = densScal$y )
  densDScal <- data.frame( h = "hd_cal", x = densDcal$x, y = densDcal$y )

  
  if( is.null( lims ) )
  {
    log10CalLR <- log10(c(CalLR$LR.cal.ss,CalLR$LR.cal.ds))
    lims = c( min( log10CalLR ) - 2 , max( log10CalLR ) + 2, 0, ceiling( max( dens$y ) * 10 ) / 10 )
    #lims <- c(round(min(subset( data, h == "hd" )$lr), 5), round(max(subset( data, h == "hp" )$lr), 5) )
  }

  if( !is.null( ymax ) )
    lims[4] = ymax

  ret = ggplot( dens, aes( x = x, y = y, colour = h) ) +
    myTheme() +
    geom_line( size = 1 , linetype = "solid")+
    scale_color_manual( values = c("grey38", "gray76") ) +

    geom_hline( yintercept = 0 ) +
    geom_vline( xintercept = 0, linetype = "dotted" )

  ret = ret +
    geom_line( inherit.aes = F, data = densDScal, aes( x = x, y = y ),
               size = 0.8, linetype = "dashed", colour = "firebrick2")+
    geom_line( inherit.aes = F, data = densSScal, aes( x = x, y = y ),
           size = 1, linetype = "dashed", colour = "turquoise3")

  ret = ret +    
    xlab( xlab %>% iconv( "UTF-8" ) ) + ylab( ylab %>% iconv( "UTF-8" ) ) +
    xlim( lims[1], lims[2] ) +
    ylim( lims[3], lims[4] )
  
  if( ribbon  ==  TRUE & nrow( subset( dens, h  ==  "hp" & x <= 0 ) ) !=  0 )
    ret = ret + geom_ribbon( data = subset( dens,h == "hp" & x <= 0 ), aes( ymax = y ), ymin = 0, fill = "#1e1e1e", colour = NA, alpha = 0.2 )

  if( ribbon  ==  TRUE & nrow( subset( dens, h  ==  "hd" & x >=  0 ) ) !=  0 )
    ret = ret + geom_ribbon( data = subset( dens,h == "hd" & x >= 0 ), aes( ymax = y ), ymin = 0, fill = "grey", colour = NA, alpha = 0.2 )
  
  if( !is.null( title ) )
    ret = ret + ggtitle( title %>% iconv( "UTF-8" ) )
  
  #Add RMEP and RMED
  rmep = sum( subset( data, h == "hd" )$lr > 1 ) / sum( data$h  ==  "hd" ) * 100
  rmed = sum( subset( data, h == "hp" )$lr < 1 ) / sum( data$h  ==  "hp" ) * 100
  rmep = round( rmep, digits = 2 )
  rmed = round( rmed, digits = 2 )

  datacal_hp <- data.frame(lr = datacal$LR.cal.ss)
  datacal_hd <- data.frame(lr = datacal$LR.cal.ds)

  rmepcal = sum( datacal_hd > 1 ) / count( datacal_hd ) * 100
  rmedcal = sum( datacal_hp < 1 ) / count( datacal_hp ) * 100
  rmepcal = as.numeric(round( rmepcal, digits = 2 ))
  rmedcal = as.numeric(round( rmedcal, digits = 2 ))

  
  ret = ret +
    annotate( "text",
              label = paste0("  RMEP : ", rmep, " %",
                             "\nRMED : ", rmed, " %",
                             "\n",
                             "\nRMEPcal : ", rmepcal, " %",
                             "\nRMEDcal : ", rmedcal, " %"),
              x = Inf, y = Inf,
              vjust = 2, hjust = 1 ) +
    
    myTheme()
  
  return( ret )
}


############## CALIBRATED SLR DISTRIBUTIONS PLOT FUNCTION ##############

distriPlot3 = function(data, datacal, CalLR, title = NULL, xlab = "Log10(SLR)", ylab = "Density",
                       lims = NULL, ymax = NULL, ribbon = TRUE){

  data = data %>% subset( select = c( "h", "lr" ) )

  data$lr = data$lr %>% as.character %>% as.numeric
  data$loglr = data$lr %>% log10

  densS = density( subset( data, h == "hp" )$loglr )
  densD = density( subset( data, h == "hd" )$loglr )

  dens = rbind(
    data.frame( h = "hp", x = densS$x, y = densS$y ),
    data.frame( h = "hd", x = densD$x, y = densD$y )
  )

  densScal = density(log10(CalLR$LR.cal.ss))
  densDcal = density(log10(CalLR$ LR.cal.ds))

  densSScal <- data.frame( h = "hp_cal", x = densScal$x, y = densScal$y )
  densDScal <- data.frame( h = "hd_cal", x = densDcal$x, y = densDcal$y )

  densCal = rbind(
    densSScal,
    densDScal
  )

  if( is.null( lims ) )
  {
    log10CalLR <- log10(c(CalLR$LR.cal.ss,CalLR$LR.cal.ds))
    lims = c( min( log10CalLR ) - 2 , max( log10CalLR ) + 2, 0, ceiling( max( dens$y ) * 10 ) / 10 )
    #lims <- c(round(min(subset( data, h == "hd" )$lr), 5), round(max(subset( data, h == "hp" )$lr), 5) )
  }

  if( !is.null( ymax ) )
    lims[4] = ymax

  ret = ggplot( dens, aes( x = x, y = y, colour = h) ) +
    myTheme() +
    geom_line( size = 1 , linetype = "solid")+
    scale_color_manual( values = c("white", "white") ) +

    geom_hline( yintercept = 0 ) +
    geom_vline( xintercept = 0, linetype = "dotted" )

  ret = ret +
    geom_line( inherit.aes = F, data = densDScal, aes( x = x, y = y ),
               size = 0.8, linetype = "solid", colour = "firebrick2", alpha = 0.7)+
    geom_line( inherit.aes = F, data = densSScal, aes( x = x, y = y),
           size = 1, linetype = "solid", colour = "turquoise3", alpha = 0.7)

  ret = ret +
    xlab( xlab %>% iconv( "UTF-8" ) ) + ylab( ylab %>% iconv( "UTF-8" ) ) +
    xlim( lims[1], lims[2] ) +
    ylim( lims[3], lims[4] )


  if( ribbon  ==  TRUE & nrow( subset( densCal, h  ==  "hp_cal" & x <= 0 ) ) !=  0 )
    ret = ret + geom_ribbon( data = subset( densCal,h == "hp_cal" & x <= 0 ), aes( ymax = y ), ymin = 0, fill = "grey", colour = NA, alpha = 0.2 )

  if( ribbon  ==  TRUE & nrow( subset( densCal, h  ==  "hd_cal" & x >=  0 ) ) !=  0 )
    ret = ret + geom_ribbon( data = subset( densCal,h == "hd_cal" & x >= 0 ), aes( ymax = y ), ymin = 0, fill = "#1e1e1e", colour = NA, alpha = 0.2 )

  if( !is.null( title ) )
    ret = ret + ggtitle( title %>% iconv( "UTF-8" ) )

  #Add RMEP and RMED
  rmep = sum( subset( data, h == "hd" )$lr > 1 ) / sum( data$h  ==  "hd" ) * 100
  rmed = sum( subset( data, h == "hp" )$lr < 1 ) / sum( data$h  ==  "hp" ) * 100
  rmep = round( rmep, digits = 2 )
  rmed = round( rmed, digits = 2 )

  datacal_hp <- data.frame(lr = datacal$LR.cal.ss)
  datacal_hd <- data.frame(lr = datacal$LR.cal.ds)

  rmepcal = sum( datacal_hd > 1 ) / count( datacal_hd ) * 100
  rmedcal = sum( datacal_hp < 1 ) / count( datacal_hp ) * 100
  rmepcal = as.numeric(round( rmepcal, digits = 2 ))
  rmedcal = as.numeric(round( rmedcal, digits = 2 ))


  ret = ret +
    annotate( "text",
              label = paste0("\nRMEPcal : ", rmepcal, " %",
                             "\nRMEDcal : ", rmedcal, " %"),
              x = Inf, y = Inf,
              vjust = 2, hjust = 1 ) +

    myTheme()

  return( ret )
}

############## PLOTS ##############

#Wrapper to plot calibration plots
PlotCalibration <- function(CalData, datacal, CalLR, ymax=10){
  p1 <- ecePlot2(CalData, CalLR=CalLR) + ggtitle(deparse(substitute(CalLR)))
  p2 <- distriPlot2(CalData, datacal, CalLR, ymax = ymax)
  p3 <- grid.arrange(p1,p2)
  return(p3)
}

####################################################################################
####################################################################################