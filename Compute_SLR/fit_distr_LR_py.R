# Name : fit_distr_LR_py.R
# Objective : compute SLR under H1 et H2 from GEV / KDE / Normale / Weibull distributions
# Created by : mjacquet
# Last version : 20.12.2020

######## METHODE
# Choose GEV / KDE / Normale / Weibull fitting method under H1 et H2 (cf l.52-113 and l.142-184)


######## DATA FORM
# csv 3 col :      name img1 ; name img 2 ; score value


library(tidyr)
library(dplyr )
library(fitdistrplus)
library(EnvStats)
library(extRemes)
library(stats)
library(reticulate)
library(stringr)

path1 <- "C:/Users/MlgJcqt/Scores/Poubelle_temp/temp_exe_R_py/intra/"
path2 <- "C:/Users/MlgJcqt/Scores/Poubelle_temp/temp_exe_R_py/inter/"
path3 <- "C:/Users/MlgJcqt/Scores/Poubelle_temp/temp_exe_R_py/comp/"


######### A CHANGER SELON DATA DANS PYTHON
path4 <- "C:/Users/MlgJcqt/LRs/anch_Tanch/H1/"
path5 <- "C:/Users/MlgJcqt/LRs/anch_Tanch/H2/"
#########

pathintra <- list.files(path1, pattern = ".csv", full.names = FALSE)
intra <- read.csv(paste0(path1, pathintra),header=F,sep=";")
a <- intra[[3]]

pathinter <- list.files(path2, pattern = ".csv", full.names = FALSE)
inter <- read.csv(paste0(path2, pathinter),header=F,sep=";")
b <- inter[[3]]

################ pour H1
pathcomp <- list.files(path3, pattern = ".csv", full.names = TRUE)
compH1 <- read.csv(pathcomp,header=F,sep=";")
img_compH1 <- (compH1[1:2])
score_compH1 <- compH1[[3]]


# parametres distribution scores intra/inter puis compute LR

mat <- matrix(ncol = 4, nrow = length(score_compH1), byrow = TRUE)
colnames(mat)<-c("Score", "Numerator", "Denominator", "LR")

### GEV
parameters.a <- fevd(a, method = "MLE", type = "GEV")
parameters.b <- fevd(b, method = "MLE", type = "GEV")
loc_scale_shape_a <- as.numeric(parameters.a$results$par)
loc_scale_shape_b <- as.numeric(parameters.b$results$par)

for (i in 1:length(score_compH1)){
  s <- score_compH1[i]
  num <- devd(s, loc =  loc_scale_shape_a[1], scale = loc_scale_shape_a[2], shape = loc_scale_shape_a[3], type = "GEV")
  denom <- devd(s, loc =  loc_scale_shape_b[1], scale = loc_scale_shape_b[2], shape = loc_scale_shape_b[3], type = "GEV")
  LR = (num/denom)
  mat[i,1:4] <- c(s, num, denom, LR)
}

# ### Normal
# parameters.a <- enorm(a, method = "mle")
# param.a <- as.numeric(parameters.a$parameters)
# parameters.b <- enorm(b, method = "mle")
# param.b <- as.numeric(parameters.b$parameters)
#
# for (i in 1:length(score_compH1)){
#   s <- score_compH1[i]
#   num <- dnorm(s, mean =  param.a[1], sd = param.a[2])
#   denom <- dnorm(s, mean =  param.b[1], sd = param.b[2])
#   LR = (num/denom)
#   mat[i,1:4] <- c(s, num, denom, LR)
# }
#
# ### Weibull
# parameters.a <- eweibull(a, method = "mle")
# param.a <- as.numeric(parameters.a$parameters)
# parameters.b <- eweibull(b, method = "mle")
# param.b <- as.numeric(parameters.b$parameters)
#
# for (i in 1:length(score_compH1)){
#   s <- score_compH1[i]
#   num <- dweibull(s, shape =  param.a[1], scale = param.a[2])
#   denom <- dweibull(s, shape =  param.b[1], scale = param.b[2])
#   LR = (num/denom)
#   mat[i,1:4] <- c(s, num, denom, LR)
# }
#
# ### KDE
# densa <- density(a)
# densb <- density(b)
# minima <- min(densa$y[densa$y > 0])
# minimb <- min(densb$y[densb$y > 0])
#
# for (i in 1:length(score_compH1))
# {
#   s <- score_compH1[i]
#   num <- density(a, from=s, to=s, n=1)$y
#   denom <- density(b, from=s, to=s, n=1)$y
#   if (num < minima){
#     num <- minima
#   }
#   if (denom < minimb){
#     denom <- minimb
#   }
#   LR <- (num/denom)
#   mat[i,1:4] <- c(s, num, denom, LR)
# }


# export SLR dans csv
mat <- as.data.frame(mat)
complet <- cbind(img_compH1, mat)

pathcomp <- list.files(path3, pattern = ".csv", full.names = FALSE)

POIscenar <- str_replace(pathcomp, "_comp_H1", "")
write.csv2(complet, paste0(path4, "LRs_",POIscenar), row.names = F)


################ pour H2
# intra et inter idem + comp des x premiers resultats de l'inter vs la trace ou le susp

# LR compute sur les 10'000 meilleurs scores sous H2
perc <- round((length(b)*100)/100, 0)
 if (perc>10000){
   perc <- 10000
 }

compH2 <- inter[1:perc, ]
img_compH2 <- (compH2[1:2])
score_compH2 <- compH2[[3]]

mat2 <- matrix(ncol = 4, nrow = length(score_compH2), byrow = TRUE)
colnames(mat2)<-c("Score", "Numerator", "Denominator", "LR")

### GEV
for (i in 1:length(score_compH1)){
  s <- score_compH1[i]
  num <- devd(s, loc =  loc_scale_shape_a[1], scale = loc_scale_shape_a[2], shape = loc_scale_shape_a[3], type = "GEV")
  denom <- devd(s, loc =  loc_scale_shape_b[1], scale = loc_scale_shape_b[2], shape = loc_scale_shape_b[3], type = "GEV")
  LR = (num/denom)
  mat[i,1:4] <- c(s, num, denom, LR)
}

# ### Normal
# for (i in 1:length(score_compH1)){
#   s <- score_compH1[i]
#   num <- dnorm(s, mean =  param.a[1], sd = param.a[2])
#   denom <- dnorm(s, mean =  param.b[1], sd = param.b[2])
#   LR = (num/denom)
#   mat[i,1:4] <- c(s, num, denom, LR)
# }
#
#
# ### Weibull
# for (i in 1:length(score_compH1)){
#   s <- score_compH1[i]
#   num <- dweibull(s, shape =  param.a[1], scale = param.a[2])
#   denom <- dweibull(s, shape =  param.b[1], scale = param.b[2])
#   LR = (num/denom)
#   mat[i,1:4] <- c(s, num, denom, LR)
# }
#
# ### KDE
# for (i in 1:length(score_compH1))
# {
#   s <- score_compH1[i]
#   num <- density(a, from=s, to=s, n=1)$y
#   denom <- density(b, from=s, to=s, n=1)$y
#   if (num < minima){
#     num <- minima
#   }
#   if (denom < minimb){
#     denom <- minimb
#   }
#   LR <- (num/denom)
#   mat[i,1:4] <- c(s, num, denom, LR)
# }

# export SLR in csv
mat2 <- as.data.frame(mat2)
complet2 <- cbind(img_compH2, mat2)

write.csv2(complet2, paste0(path5, "LRs_",POIscenar), row.names = F)
