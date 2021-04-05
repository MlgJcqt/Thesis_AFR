# Title     : Calibration_LR.R
# Objective : Calibrate SLR + plots & export calSLR values
# Created by : C. Champod & mjacquet (11.11.2020)
# Last version : 19.01.2021

######## DATA FORM
# csv Hp
# (headers)     h | lr
#              hp | value
#               . | value
#               . | value
# csv Hd
#              hd | value
#               . | value
#               . | value
########

library(comparison)
library(tidyverse)
library(isotone) #require to do the PAVA
library(plotly)
library(gridExtra) #for Grid.arrange()
library(reshape2) #for melt()

# Get functions in CalibrationFunc.R
source("C:/Users/mjacquet/Scripts/CalibrationFunc.R")

# Intput directory
path <- "C:/Users/mjacquet/SLR/"

# Output directory
plotDir <- "C:/Users/mjacquet/R_Plots/"


############## DATA ##############
set.seed(1234)
Nsubsample <- 10000

ResLR1_hp <- read_delim(paste0(path, "data_Hp.csv"),";",
                          escape_double = FALSE,
                        col_types = cols(lr = col_number()),
                        locale = locale(decimal_mark = ","),
                        trim_ws = TRUE) %>%
        filter(!is.na(lr))

ResLR1_hd <- read_delim(paste0(path, "data_Hd.csv"),";",
                        escape_double = FALSE,
                        col_names = FALSE,
                        col_types = cols(X2 = col_number()),
                        locale = locale(decimal_mark = ","),
                        trim_ws = TRUE) %>%
        rename(h = X1, lr = X2) %>%
        sample_n(Nsubsample) %>%
        filter(!is.na(lr)) %>%
        filter(lr!=0)

ResLR1 <- bind_rows(ResLR1_hp,ResLR1_hd)


############## CALIBRATION ##############
# PAVA
PAVA_ResLR1 <- data.calibrate.pava(ResLR1, method = "laplace")

SLR_PAVA_hp <- as.data.frame(PAVA_ResLR1$LR.cal.ss)
SLR_PAVA_hd <- as.data.frame(PAVA_ResLR1$LR.cal.ds)

write.csv2(SLR_PAVA_hp, paste0(plotDir, "PAVA_Hp.csv"), row.names = F)
write.csv2(SLR_PAVA_hd, paste0(plotDir, "PAVA_Hd.csv"), row.names = F)


# REG LOG
CalData_ResLR1 <- data_to_list_LR(ResLR1)
Param_ResLR1 <- logistic.calibrate.get.model(CalData_ResLR1$LR.ss,
                                             CalData_ResLR1$LR.ds)
logistic_ResLR1 <- logistic.calibrate.set(CalData_ResLR1$LR.ss,
                                          CalData_ResLR1$LR.ds)

# export calibrated SLR values in csv
SLR_reglog_hp <- as.data.frame(logistic_ResLR1$LR.cal.ss)
SLR_reglog_hd <- as.data.frame(logistic_ResLR1$LR.cal.ds)

write.csv2(SLR_reglog_hp, paste0(plotDir, "LogReg_Hp.csv"), row.names = F)
write.csv2(SLR_reglog_hd, paste0(plotDir, "LogReg_Hd.csv"), row.names = F)


############## PLOTS ##############
# plot only calibrated curves
png( filename=paste0( plotDir, "distrib_cal_PAVA.png" ), width=800, heigh=600 )
distriPlot3(data=ResLR1, datacal = PAVA_ResLR1, CalLR=PAVA_ResLR1, lims = c(-5,5), ymax = 0.7, ribbon = TRUE)
dev.off( )

# plot distriplot raw & calibrated SLR
png( filename=paste0( plotDir, "distrib_logreg.png" ), width=800, heigh=800 )
distriPlot2(data=ResLR1, datacal = logistic_ResLR1, CalLR=logistic_ResLR1, lims = c(-8,20), ymax = 1.5)
dev.off( )

# plot ECE raw & calibrated SLR
png( filename=paste0( plotDir, "ECE_logreg.png" ), width=800, heigh=800 )
PlotCalibration(CalData=ResLR1, datacal = logistic_ResLR1, CalLR=logistic_ResLR1, ymax=1.5)
dev.off( )

# while (dev.cur()>1) dev.off()

####################################################################################
####################################################################################
