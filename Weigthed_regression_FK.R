
library(ggplot2)
library(zoo)
library(corrplot)
library(lattice)
require(graphics)
require(grDevices)
library(pheatmap)   
library(gplots)
library(dplyr)
library(gclus)
library(relaimpo)
library(plyr)

## Set working directory
setwd("C:/RWEM_processed_calvin/R")

# mydata <- read.csv("Quarterly_Data_V2.csv", na.string = 0, stringsAsFactors = FALSE)
# mydata <- read.csv("Quarterly_Data_V2_cost.csv", na.string = 0, stringsAsFactors = FALSE)
# mydata <- read.csv("Quarterly_Data_V4.csv", na.string = 0, stringsAsFactors = FALSE)
# mydata <- read.csv("Quarterly_Data_V6.csv", na.string = 0, stringsAsFactors = FALSE)
mydata <- read.csv("Quarterly_Data_V7.csv", na.string = 0, stringsAsFactors = FALSE)

# na.string = 0
names(mydata)[2] <- paste("STOPPED")  ### sites stopped
names(mydata)[3] <- paste("IDENTIFIED") ### site identified 
names(mydata)[4] <- paste("ILLEGAL") ### ILLEGAL_EXP_EVENT

names(mydata)[5] <- paste("ILL_EXP_INSP") ### ILLEGAL_EXP_INSPECTIONS
names(mydata)[6] <- paste("ILL_EXP_RETURN") ### ILLEGAL_EXP_RETURNED

names(mydata)[7] <- paste("MIXE")  ### recycled mixed paper
names(mydata)[8] <- paste("NEWS")  ### recycled newspapers
names(mydata)[9] <- paste("GLAS")  ### recycled glas
names(mydata)[10] <- paste("STCN")  ### recycled steel cans
names(mydata)[11] <- paste("ALCN")  ### recycled Alu cans
names(mydata)[12] <- paste("PLBT")  ### recycled plastic bottles
names(mydata)[13] <- paste("HOUSE")  ### house waste
names(mydata)[14] <- paste("RECY")  ### RECYCLE_P
names(mydata)[15] <- paste("DRY")  ### Dry recycle
names(mydata)[16] <- paste("DISP")  ### disposal
names(mydata)[17] <- paste("LA_W")  ### LA collected waste
names(mydata)[18] <- paste("CPI")  ### CPI_NOTNATIONAL_INDICIES_BASE2005
names(mydata)[19] <- paste("RPI")  ### RPI_NOTNATIONAL_INDICIES_BASE1987
names(mydata)[20] <- paste("GDP_MARKET")  ### GDP_CURRENT_MARKETPRICE
names(mydata)[21] <- paste("POP")  ### POPULATION_ENGLAND
names(mydata)[22] <- paste("DWEL")  ### DWELLINGS_ENGLAND
names(mydata)[23] <- paste("CTDR")  ### TURNOVER(£M)_COLLECTION&TREATMENT&DISPOSAL&RECOVERY
names(mydata)[24] <- paste("WATE")  ### Water Supply; Sewerage, Waste Management and Remediation Activities (Index) (Seasonally adjusted) (2012 index year)
names(mydata)[25] <- paste("WATP")  ### Water Supply; Sewerage,Waste Management & Remediation Act (period-period growth (Seasonally adjusted) index 2012
# cost names
names(mydata)[26] <- paste("Tax") # Landfill tax
names(mydata)[27] <- paste("Fee_Land") # gate-fee for landfill (MEDIAN)
names(mydata)[28] <- paste("Incin") # gate-fee for incineration
names(mydata)[29] <- paste("EA_Tot_Enforc") # Total EA spend on enforcment
names(mydata)[30] <- paste("EA_Illegal") # EA spend on Illegal waste sites
names(mydata)[31] <- paste("EA_Illegal_exp") # EA spend on Illegal export of waste

names(mydata)[32] <- paste("EA_Tot_Enforc_1Q") # Total EA spend on enforcment (time lag 1 qtr)
names(mydata)[33] <- paste("EA_Illegal_1Q") # EA spend on Illegal waste sites (time lag 1 qtr)
names(mydata)[34] <- paste("EA_Illegal_exp_1Q") # EA spend on Illegal export of waste  (time lag 1 qtr)
names(mydata)[35] <- paste("EA_Tot_2Q") # total spend cumulative previous 2 Qtrs
names(mydata)[36] <- paste("GDP_Chained") # Gross Domestic Product: chained volume measures: Seasonally adjusted £m         
names(mydata)[37] <- paste("Petrol_Oil") # Typical retail prices of petroleum products and a crude oil price index
names(mydata)[38] <- paste("EA_Illegal_Ev") # Illegal export + evidence


mydata <- as.data.frame(mydata)
mydata$YYYY.QQ <- as.yearqtr(mydata$YYYY.QQ)


###### Functions to be used in coorrelograms plots ##########################
############################################################################

# funtion to define the p value (significance)
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

##########################################################################

### Correlation plot for all the variables

pdf(paste("Correlation_Matrix",".pdf",sep=""), width = 15, height = 15)
par(cex = 0.85)
# mydata_standard <- data.frame(scale(mydata[2:33]))
# calculate corrlation coefficients
M <- cor(mydata[2:35], use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))
# M <- cor(mydata_standard, use = "pairwise.complete.obs")
M[is.na(M)]=0
p.mat <- cor.mtest(M)


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(M, method="color", col=col(200),  
         type="upper", # order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex = 1, #Text label color and rotation
         # Combine with significance (p value)
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # p.mat = p.mat, 
         # hide correlation coefficient on the principal diagonal
         diag=TRUE)

dev.off()

M <- as.data.frame(M)
write.csv(M, "Correlation_Matrix.csv")

#################################################################################
# Total cost data vs Site identified
                    # Site Stopped 
                    # Illegal Export Event Stopped
# 

mydata_Total <- mydata %>%
  dplyr::select(STOPPED,
         IDENTIFIED,
         ILLEGAL,
         EA_Tot_Enforc,
         EA_Tot_Enforc_1Q,
         EA_Tot_2Q)


M <- cor(mydata_Total, use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))
# M <- cor(mydata_standard, use = "pairwise.complete.obs")
M[is.na(M)]=0
p.mat <- cor.mtest(M)



jpeg("TOT_Cost_vs_indicators.jpg",
     quality = 100, bg = "white", res = 200, width = 7, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)

corrplot(M, method = "ellipse", type="upper", col=col(200),
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex = 0.8, #Text label color and rotation
         # Combine with significance (p value)
         # p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         diag=FALSE)
         # main ="Total Cost data vs Sites")

par(oldpar)
dev.off()



# Multilinear regressions for Total EA spend on enforcment vs INDICATORS

fit_TOT <- lm(EA_Tot_Enforc ~ STOPPED + IDENTIFIED + ILLEGAL,
          data = mydata_Total, na.action= na.exclude) 

names <- c("SITES_STOPPED", "SITES_IDENTIFIED", "ILLEGAL_EXP_EVENT")
names <- as.data.frame(names)

summary(fit_TOT) # show results
signif(summary(fit_TOT)$r.squared, 5) ### R2


### Diagnostic plots ##### 
Coeff <- as.data.frame(coefficients(fit_TOT)) # model coefficients (intercef and slope from the regression)

###### STANDARD ERROR ---------------------------------------------------
# if we want to refer to the STANDARD ERRORS for the coefficient estimates the we can calucluated the variance in this way
STD_ERR_Coeff <- as.data.frame(summary(fit_TOT)$coefficients[,2])

library(relaimpo)


###############################################################################################
pdf(paste("Total_EA_spend_enforcment_IMPORTANCE",".pdf",sep=""), width = 20, height = 16.53)
IMPO_Tot_EA_Enforc <- calc.relimp(fit_TOT, type="lmg", rela=TRUE)

plot(IMPO_Tot_EA_Enforc, sort = FALSE,
     main ="Relative Importance of indicators vs Total_EA_£_Enforcment")

dev.off()
################################################################################################


R2_IMPO <-  as.data.frame(IMPO_Tot_EA_Enforc$lmg)*100  ### % of R2
Ranking_IMPO <- as.data.frame(IMPO_Tot_EA_Enforc$lmg.rank)  ### Rank of R2


IMPO_Stats_TOT <- cbind(names, Coeff[-1,], STD_ERR_Coeff[-1,], Coeff[1,], R2_IMPO, Ranking_IMPO)
IMPO_Stats_TOT <- cbind(Row.Names = rownames(IMPO_Stats_TOT), IMPO_Stats_TOT)
colnames(IMPO_Stats_TOT) <- c("Codes", "names", "Coeff. (slope)", "Standard Error(slope)", "Intercept", "Importance (% of R2)", "Ranking_R2")

write.csv(IMPO_Stats_TOT, file = "Total_EA_£_Enforcment_Importance_Statisitcs.csv", row.names=TRUE)

#####################################################################################################
#####################################################################################################


# Multilinear regressions for Total EA spend on the 1Q on enforcment vs INDICATORS

fit_TOT_1Q <- lm(EA_Tot_Enforc_1Q ~ STOPPED + IDENTIFIED + ILLEGAL,
              data = mydata_Total, na.action= na.exclude) 

names <- c("SITES_STOPPED", "SITES_IDENTIFIED", "ILLEGAL_EXP_EVENT")
names <- as.data.frame(names)

summary(fit_TOT_1Q) # show results
signif(summary(fit_TOT_1Q)$r.squared, 5) ### R2


### Diagnostic plots ##### 
Coeff <- as.data.frame(coefficients(fit_TOT_1Q)) # model coefficients (intercef and slope from the regression)

###### STANDARD ERROR ---------------------------------------------------
# if we want to refer to the STANDARD ERRORS for the coefficient estimates the we can calucluated the variance in this way
STD_ERR_TOT_1Q <- as.data.frame(summary(fit_TOT_1Q)$coefficients[,2])

library(relaimpo)


###############################################################################################
pdf(paste("Total_EA_spend_enforcment_1Q_IMPORTANCE",".pdf",sep=""), width = 20, height = 16.53)
IMPO_Tot_EA_Enforc_1Q <- calc.relimp(fit_TOT_1Q, type="lmg", rela=TRUE)

plot(IMPO_Tot_EA_Enforc_1Q, sort = FALSE,
     main ="Relative Importance of indicators vs Total_EA_£_Enforcment_1Q")

dev.off()
################################################################################################


R2_IMPO <-  as.data.frame(IMPO_Tot_EA_Enforc_1Q$lmg)*100  ### % of R2
Ranking_IMPO <- as.data.frame(IMPO_Tot_EA_Enforc_1Q$lmg.rank)  ### Rank of R2


IMPO_Stats <- cbind(names, Coeff[-1,], STD_ERR_TOT_1Q[-1,], Coeff[1,],R2_IMPO, Ranking_IMPO)
IMPO_Stats <- cbind(Row.Names = rownames(IMPO_Stats), IMPO_Stats)
colnames(IMPO_Stats) <- c("Codes", "names", "Coeff. (slope)", "Standard Error(slope)", "Intercept", "Importance (% of R2)", "Ranking_R2")

write.csv(IMPO_Stats, file = "Total_EA_£_Enforcment_1Q_Importance_Statisitcs.csv", row.names=TRUE)


#####################################################################################################
#####################################################################################################


# Multilinear regressions for Total EA spend on the 2Q on enforcment vs INDICATORS

fit_TOT_2Q <- lm(EA_Tot_2Q ~ STOPPED + IDENTIFIED + ILLEGAL,
                 data = mydata_Total, na.action= na.exclude) 

names <- c("SITES_STOPPED", "SITES_IDENTIFIED", "ILLEGAL_EXP_EVENT")
names <- as.data.frame(names)

summary(fit_TOT_2Q) # show results
signif(summary(fit_TOT_2Q)$r.squared, 5) ### R2


### Diagnostic plots ##### 
Coeff <- as.data.frame(coefficients(fit_TOT_2Q)) # model coefficients (intercef and slope from the regression)

###### STANDARD ERROR ---------------------------------------------------
# if we want to refer to the STANDARD ERRORS for the coefficient estimates the we can calucluated the variance in this way
STD_ERR_TOT_2Q <- as.data.frame(summary(fit_TOT_2Q)$coefficients[,2])

library(relaimpo)


###############################################################################################
pdf(paste("Total_EA_spend_enforcment_2Q_IMPORTANCE",".pdf",sep=""), width = 20, height = 16.53)
IMPO_Tot_EA_Enforc_2Q <- calc.relimp(fit_TOT_2Q, type="lmg", rela=TRUE)

plot(IMPO_Tot_EA_Enforc_2Q, sort = FALSE,
     main ="Relative Importance of indicators vs Total_EA_£_Enforcment_2Q")

dev.off()
################################################################################################


R2_IMPO <-  as.data.frame(IMPO_Tot_EA_Enforc_2Q$lmg)*100  ### % of R2
Ranking_IMPO <- as.data.frame(IMPO_Tot_EA_Enforc_2Q$lmg.rank)  ### Rank of R2


IMPO_Stats <- cbind(names, Coeff[-1,], STD_ERR_TOT_2Q[-1,], Coeff[1,], R2_IMPO, Ranking_IMPO)
IMPO_Stats <- cbind(Row.Names = rownames(IMPO_Stats), IMPO_Stats)
colnames(IMPO_Stats) <- c("Codes", "names", "Coeff. (slope)","Standard Error(slope)", "Intercept" ,"Importance (% of R2)", "Ranking_R2")

write.csv(IMPO_Stats, file = "Total_EA_£_Enforcment_2Q_Importance_Statisitcs.csv", row.names=TRUE)






#################################################################################
#################################################################################
#################################################################################
#################################################################################
# ILLEGAL site cost data vs Site identified & Site Stopped 


mydata_Illegal <- mydata %>%
  dplyr::select(STOPPED,
         IDENTIFIED,
         EA_Illegal,
         EA_Illegal_1Q)

M <- cor(mydata_Illegal, use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))
M[is.na(M)]=0
p.mat <- cor.mtest(M)

jpeg("ILLEGAL_Cost_vs_indicators.jpg",
     quality = 100, bg = "white", res = 200, width = 7, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)

corrplot(M, method = "ellipse", type="upper", col=col(200),
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex = 0.8, #Text label color and rotation
         # Combine with significance (p value)
         # p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         diag=FALSE)
# main ="Total Cost data vs Sites")

par(oldpar)
dev.off()


# Multilinear regressions for EA spend on illegal waste sites vs INDICATORS

fit_ILLEGAL <- lm(EA_Illegal ~ STOPPED + IDENTIFIED,
              data = mydata_Illegal, na.action= na.exclude) 

names <- c("SITES_STOPPED", "SITES_IDENTIFIED")
names <- as.data.frame(names)

summary(fit_ILLEGAL) # show results
signif(summary(fit_ILLEGAL)$r.squared, 5) ### R2


### Diagnostic plots ##### 
Coeff <- as.data.frame(coefficients(fit_ILLEGAL)) # model coefficients (intercef and slope from the regression)

###### STANDARD ERROR ---------------------------------------------------
# if we want to refer to the STANDARD ERRORS for the coefficient estimates the we can calucluated the variance in this way
STD_ERR_ILLEGAL <- as.data.frame(summary(fit_ILLEGAL)$coefficients[,2])

library(relaimpo)


###############################################################################################
pdf(paste("EA_spend_on_illegal_sites_IMPORTANCE",".pdf",sep=""), width = 20, height = 16.53)
IMPO_Illegal_EA <- calc.relimp(fit_ILLEGAL, type="lmg", rela=TRUE)

plot(IMPO_Illegal_EA, sort = FALSE,
     main ="Relative Importance of indicators vs EA_£_Illegal_waste_sites")

dev.off()
################################################################################################


R2_IMPO <-  as.data.frame(IMPO_Illegal_EA$lmg)*100  ### % of R2
Ranking_IMPO <- as.data.frame(IMPO_Illegal_EA$lmg.rank)  ### Rank of R2


IMPO_Stats_ILLEGAL <- cbind(names, Coeff[-1,], STD_ERR_ILLEGAL[-1,], Coeff[1,], R2_IMPO, Ranking_IMPO)
IMPO_Stats_ILLEGAL <- cbind(Row.Names = rownames(IMPO_Stats_ILLEGAL), IMPO_Stats_ILLEGAL)
colnames(IMPO_Stats_ILLEGAL) <- c("Codes", "names", "Coeff. (slope)", "Standard Error(slope)", "Intercept", "Importance (% of R2)", "Ranking_R2")

write.csv(IMPO_Stats_ILLEGAL, file = "EA_£_Illegal_waste_Importance_Statisitcs.csv", row.names=TRUE)

#####################################################################################################
#####################################################################################################



# Multilinear regressions for EA spend on illegal waste sites vs INDICATORS 1Q

fit_ILLEGAL_1Q <- lm(EA_Illegal_1Q ~ STOPPED + IDENTIFIED,
                  data = mydata_Illegal, na.action= na.exclude) 

names <- c("SITES_STOPPED", "SITES_IDENTIFIED")
names <- as.data.frame(names)

summary(fit_ILLEGAL_1Q) # show results
signif(summary(fit_ILLEGAL_1Q)$r.squared, 5) ### R2


### Diagnostic plots ##### 
Coeff <- as.data.frame(coefficients(fit_ILLEGAL_1Q)) # model coefficients (intercef and slope from the regression)

###### STANDARD ERROR ---------------------------------------------------
# if we want to refer to the STANDARD ERRORS for the coefficient estimates the we can calucluated the variance in this way
STD_ERR_ILLEGAL_1Q <- as.data.frame(summary(fit_ILLEGAL_1Q)$coefficients[,2])

library(relaimpo)


###############################################################################################
pdf(paste("EA_spend_on_illegal_sites_1Q_IMPORTANCE",".pdf",sep=""), width = 20, height = 16.53)
IMPO_Illegal_EA_1Q <- calc.relimp(fit_ILLEGAL_1Q, type="lmg", rela=TRUE)

plot(IMPO_Illegal_EA_1Q, sort = FALSE,
     main ="Relative Importance of indicators vs EA_£_Illegal_waste_sites")

dev.off()
################################################################################################


R2_IMPO <-  as.data.frame(IMPO_Illegal_EA_1Q$lmg)*100  ### % of R2
Ranking_IMPO <- as.data.frame(IMPO_Illegal_EA_1Q$lmg.rank)  ### Rank of R2


IMPO_Stats_ILLEGAL_1Q <- cbind(names, Coeff[-1,], STD_ERR_ILLEGAL_1Q[-1,], Coeff[1,], R2_IMPO, Ranking_IMPO)
IMPO_Stats_ILLEGAL_1Q <- cbind(Row.Names = rownames(IMPO_Stats_ILLEGAL_1Q), IMPO_Stats_ILLEGAL_1Q)
colnames(IMPO_Stats_ILLEGAL_1Q) <- c("Codes", "names", "Coeff. (slope)", "Standard Error(slope)", "Intercept", "Importance (% of R2)", "Ranking_R2")

write.csv(IMPO_Stats_ILLEGAL_1Q, file = "EA_£_Illegal_waste_1Q_Importance_Statisitcs.csv", row.names=TRUE)




#################################################################################
#################################################################################
#################################################################################
#################################################################################
# ILLEGAL EXP site cost data vs Site identified & Site Stopped 


mydata_Illegal_EXP <- mydata %>%
  dplyr::select(ILLEGAL,
         EA_Illegal_exp,
         EA_Illegal_exp_1Q)

M <- cor(mydata_Illegal_EXP, use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))
M[is.na(M)]=0
p.mat <- cor.mtest(M)

jpeg("ILLEGAL_EXP_Cost_vs_indicators.jpg",
     quality = 100, bg = "white", res = 200, width = 7, height = 6.5, units = "in")
par(mar=c(5, 2, 5, 3) + 0.3)
oldpar <- par(las=1)

corrplot(M, method = "ellipse", type="upper", col=col(200),
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex = 0.8, #Text label color and rotation
         # Combine with significance (p value)
         # p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         diag=FALSE)
# main ="Total Cost data vs Sites")

par(oldpar)
dev.off()












#####################################################################################

####### covariance matrix for M #################################################
#### it is about the relationshp among VARIABLES (columns) of thematrix "mydata" 

# M <- cbind(mydata[,2:18], mydata[,22:23])  ### leave out CTDR, POP and DWEL  
# M <- as.matrix(sapply(M, as.numeric))
M <- mydata[,-1]

# Standardise variables (rescale data based on the meand and Standard deviation)
M_standard <- data.frame(scale(M))

#### covariance matrix #################
M_cov <- cov(M, use='pairwise') 
M_cov_norm <- cov(M_standard, use='pairwise')  #### standardized
write.csv(M_cov, file = "Covar_Matrix.csv", row.names=TRUE)
write.csv(M_cov_norm, file = "Covar_Matrix_Standard.csv", row.names=TRUE)


# levelplot(M_cov)

### Covariance Matrix non normalized
# pdf(paste("Covariance_Matrix_1",".pdf",sep=""), width = 15, height = 15)
jpeg('Covariance_Matrix.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


heatmap <- pheatmap(M_cov, col=bluered(256), cluster_cols=TRUE, 
                    fontsize_row=15,fontsize_col=15, border_color=NA, margins=c(7,7))


dev.off()



### Covariance Matrix normalized
# pdf(paste("Covariance_Matrix_1",".pdf",sep=""), width = 15, height = 15)
jpeg('Covariance_Matrix_normalized.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


heatmap <- pheatmap(M_cov_norm, col=bluered(256), cluster_cols=TRUE, 
                    fontsize_row=15,fontsize_col=15, border_color=NA, margins=c(7,7))


dev.off()



### Covariance Matrix 2
pdf(paste("Covariance_Matrix_a",".pdf",sep=""), width = 17, height = 15)
x  <- as.matrix(M_cov_norm)
rc <- rainbow(nrow(x), start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
hv <- heatmap(x, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(15,12),
              cexRow = 3, cexCol = 3)
dev.off()



# k <- ncol(M) #number of variables
# n <- nrow(M) #number of subjects
# 
# #create means for each column
# M_mean <- matrix(data=1, nrow=n) %*% cbind(mean(M$STOPPED, na.rm=TRUE),
#                                            mean(M$IDENTIFIED, na.rm=TRUE),
#                                            mean(M$ILLEGAL, na.rm=TRUE),
#                                            mean(M$MIXE, na.rm=TRUE),
#                                            mean(M$NEWS, na.rm=TRUE),
#                                            mean(M$GLAS, na.rm=TRUE),
#                                            mean(M$STCN, na.rm=TRUE),
#                                            mean(M$ALCN,na.rm =TRUE),
#                                            mean(M$PLBT, na.rm=TRUE),
#                                            mean(M$HOUSE, na.rm=TRUE),
#                                            mean(M$RECY, na.rm=TRUE),
#                                            mean(M$DRY, na.rm=TRUE),
#                                            mean(M$DISP, na.rm=TRUE),
#                                            mean(M$LA_W, na.rm=TRUE),
#                                            mean(M$CPI, na.rm=TRUE),
#                                            mean(M$RPI, na.rm=TRUE),
#                                            mean(M$GDP, na.rm=TRUE),
#                                            #mean(M$POP, na.rm=TRUE),
#                                            #mean(M$DWEL, na.rm=TRUE),
#                                            #mean(M$CTDR, na.rm=TRUE),
#                                            mean(M$WATE, na.rm=TRUE),
#                                            mean(M$WATP, na.rm=TRUE))
# 
# 
#  M_mean <- as.matrix(sapply(M_mean, as.numeric))
#                                            
# #creates a difference matrix
# D <- M - M_mean
# D <- as.matrix(sapply(D, as.numeric))
# 
# 
# #creates the covariance matrix
# C <- ((n-1)^-1) * t(D) %*% D


########## Multilienar Regression ############################################
##############################################################################


 fit <- lm(STOPPED ~ MIXE + NEWS + GLAS + STCN + ALCN + PLBT + HOUSE + RECY +
           DRY + DISP + LA_W + CPI + RPI + GDP_MARKET + WATE + WATP,
           data = mydata, na.action= na.exclude)  ### leave out CTDR, POP and DWEL  


# fit <- lm(IDENTIFIED ~ MIXE + NEWS + GLAS + STCN + ALCN + PLBT + HOUSE + RECY +
#             DRY + DISP + LA_W + CPI + RPI + GDP + WATE + WATP, 
#           data = mydata)  ### leave out CTDR, POP and DWEL  (few observations)

# fit <- lm(ILLEGAL ~ MIXE + NEWS + GLAS + STCN + ALCN + PLBT + HOUSE + RECY +
#             DRY + DISP + LA_W + CPI + RPI + GDP + WATE + WATP, 
#           data = mydata)  ### leave out CTDR, POP and DWEL  (few observations)


names <- c("RECYCLE_MIXEDPAPER_£", "RECYCLE_NEWSPAPER_£", "RECYCLE_MIXGLASS_£",
           "RECYCLE_STEELCANS_£", "RECYCLE_ALUMCANS_£", "RECYCLE_PLASTICBOTTLE_£",
           "HOUSEWASTE_T", "RECYCLE_P","DRYRECYCLE_P", "DISPOSAL_P","LA_COLLECTWASTE_T",
           "CPI_NOTNATIONAL_INDICIES_BASE2005", "RPI_NOTNATIONAL_INDICIES_BASE1987",
           "GDP_CURRENT_MARKETPRICE", "E : Water Supply; Sewerage, Waste Management and Remediation Activities (Index) (Seasonally adjusted) (2012 index year)",
           "Water Supply; Sewerage,Waste Management & Remediation Act (period-period growth (Seasonally adjusted) index 2012")
names <- as.data.frame(names)

summary(fit) # show results
signif(summary(fit)$r.squared, 5) ### R2
 

### Diagnostic plots ##### 
Coeff <- as.data.frame(coefficients(fit)) # model coefficients (intercef and slope from the regression)

 confint(fit, level=0.95) # CIs for model parameters 
 fitted(fit) # predicted values
 residuals(fit) # residuals
 anova(fit) # anova table 
 Covariance_Matrix <-  vcov(fit) # covariance matrix for model parameters 
 influence(fit) # regression diagnostics

# Calculate Relative Importance for Each Predictor
# Function to calculate relative importance metrics for linear models,
# calc.relimp calculates several relative importance metrics for the linear model.
# The recommended metrics are lmg (R^2 partitioned by averaging over orders,
# like in Lindemann, Merenda and Gold (1980, p.119ff)) and pmvd
# (a newly proposed metric by Feldman (2005) that is provided
# in the non-US version of the package only). For completeness and comparison
# purposes, several other metrics are also on offer (cf. e.g. Darlington (1968)).

library(relaimpo)

# pdf(paste("Relative_Importance_Site_Stopped",".pdf",sep=""), width = 20, height = 16.53)
# pdf(paste("Relative_Importance_Site_Identified",".pdf",sep=""), width = 20, height = 16.53)
pdf(paste("Relative_Importance_Site_Illegal",".pdf",sep=""), width = 20, height = 16.53)

# IMPO_Sites_Stopped <- calc.relimp(fit, type="lmg", rela=TRUE)
# IMPO_Sites_Identified <- calc.relimp(fit, type="lmg", rela=TRUE)
IMPO_Sites_Illegal <- calc.relimp(fit, type="lmg", rela=TRUE)

# plot(IMPO_Sites_Stopped, sort = FALSE,
#     main ="Relative Importance of Variables vs Sites Stopped")

# plot(IMPO_Sites_Identified, sort = FALSE,
#      main ="Relative Importance of Variables vs Sites Identified")

plot(IMPO_Sites_Illegal, sort = FALSE,
     main ="Relative Importance of Variables vs Sites Illegal")

dev.off()



# R2_IMPO <-  as.data.frame(IMPO_Sites_Stopped$lmg)*100  ### % of R2
# R2_IMPO <-  as.data.frame(IMPO_Sites_Identified$lmg)*100  ### % of R2
R2_IMPO <-  as.data.frame(IMPO_Sites_Illegal$lmg)*100  ### % of R2

# Ranking_IMPO <- as.data.frame(IMPO_Sites_Stopped$lmg.rank)  ### Rank of R2
# Ranking_IMPO <- as.data.frame(IMPO_Sites_Identified$lmg.rank)  ### Rank of R2
Ranking_IMPO <- as.data.frame(IMPO_Sites_Illegal$lmg.rank)  ### Rank of R2


IMPO_Stats <- cbind(names, Coeff[-1,], R2_IMPO, Ranking_IMPO)
IMPO_Stats <- cbind(Row.Names = rownames(IMPO_Stats), IMPO_Stats)
colnames(IMPO_Stats) <- c("Codes", "names", "Coeff. (slope)", "Importance_R2", "Ranking_R2")

# write.csv(IMPO_Stats, file = "Sites_Stopped_Importance_Statisitcs.csv", row.names=TRUE)
# write.csv(IMPO_Stats, file = "Sites_Identified_Importance_Statisitcs.csv", row.names=TRUE)
write.csv(IMPO_Stats, file = "Sites_Illegal_Importance_Statisitcs.csv", row.names=TRUE)


##########################################################################################################################
####////////////////////////////////////////////////////////////////////////////////////////////////////##################
####////////////////////////////////////////////////////////////////////////////////////////////////////##################
##########################################################################################################################
###### MULTILINEAR REGRESSION WITH COSTING DATA ##########################################################################

# Multilinear regressions for Illegal sites STOPPED versus selected variables

mydata_STOPPED <- mydata %>%
  dplyr::select(STOPPED, 
                EA_Illegal,
                Incin, 
                Tax,
                WATE,
#                Fee_Land, 
                
                
 #               MIXE,
  #                NEWS,
                #  GLAS,
   #               STCN,
    #              ALCN,
     #             PLBT,
      #            HOUSE,
       #           RECY,
        #          DISP,
         #         RPI,
                 
                 # WATP,
                
                
                
               # POP,
              #  DWEL,
             #   LA_W,
                DISP,
                DRY,
                CPI,
               GDP_MARKET)
              #  GDP_Chained)
               # Petrol_Oil)

mydata_STOPPED <- mydata_STOPPED %>%
                filter(STOPPED != "NA")
 
 # mydata_STOPPED <- mydata_STOPPED[1:27,]


fit_ILLEGAL_STOPPED <- lm(STOPPED ~ EA_Illegal +
                            Incin + 
                          Tax +
                          WATE +
                            
                          #  MIXE +
                          #  NEWS +
                          #  GLAS +
                           # STCN +
                          #  ALCN +
                          #  PLBT +
                           # HOUSE +
                           # RECY +
                          #  DISP +
                          #  RPI +
                           # WATE +
                          #  WATP+
                      
                               #  POP +
                              #   DWEL +
                              DISP +  
                             # LA_W +
                                DRY +
                                CPI +
                                GDP_MARKET,
                              #  GDP_Chained,
                              #  Petrol_Oil,
                         data = mydata_STOPPED, na.action= na.exclude)

names <- c("EA spend on Illegal waste sites",
           "gate-fee for incineration",
           "Landfill tax",
           "gate-fee for landfill (MEDIAN)",
          # "POPULATION_ENGLAND",
          # "DWELLINGS_ENGLAND",
           "LA_COLLECTWASTE_T",
           "DRYRECYCLE_P",
           "CPI_NOTNATIONAL_INDICIES_BASE2005",
          "GDP_CURRENT_MARKETPRICE")
        #   "Gross Domestic Product: chained volume measures: Seasonally adjusted £m")
         # "Typical retail prices of petroleum products and a crude oil price index")

names <- as.data.frame(names)


# regression (fit) main coefficients
Coeff_STOPPED <- as.data.frame(coefficients(fit_ILLEGAL_STOPPED)) # model coefficients (intercept and slope from the regression)


# get R2
summary(fit_ILLEGAL_STOPPED) # show results



SIGNIF_STOPPED <- signif(summary(fit_ILLEGAL_STOPPED)$r.squared, 5) ### R2
SIGNIF_STOPPED <- as.data.frame(SIGNIF_STOPPED)


###### VARIANCE-----------------------------------------------------------
# residual variance
Residual_VARIANCE <- (summary(fit_ILLEGAL_STOPPED)$sigma)**2
Residual_VARIANCE <- as.data.frame(Residual_VARIANCE)

# if we refers to the estimated residual variance
VARIANCE_STOPPED <- summary(fit_ILLEGAL_STOPPED)$sigma
VARIANCE_STOPPED <- as.data.frame(VARIANCE_STOPPED)
# Type names( summary(m) ) and names(m) for other information you can access.

# Variance of the slope
Slope_VARIANCE <- vcov(fit_ILLEGAL_STOPPED)[7,7]
Slope_VARIANCE <- as.data.frame(Slope_VARIANCE)
# covariance matrix of the linear regression
vcov(fit_ILLEGAL_STOPPED)
############################################################################

###### STANDARD ERROR ---------------------------------------------------
# if we want to refer to the STANDARD ERRORS for the coefficient estimates the we can calucluated the variance in this way
STD_ERR_Coeff_STOPPED <- as.data.frame(summary(fit_ILLEGAL_STOPPED)$coefficients[,2])


### Diagnostic plots ##### 

library(relaimpo)


###############################################################################################
jpeg('Illegal_Stopped_IMPORTANCE.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

IMPO_Illegal_Stopped <- calc.relimp(fit_ILLEGAL_STOPPED, type="lmg", rela=TRUE)

plot(IMPO_Illegal_Stopped, sort = FALSE,
     main ="Relative Importance of Selected Variables vs Illegal waste sites stopped")

dev.off()
################################################################################################


R2_IMPO_STOPPED <-  as.data.frame(IMPO_Illegal_Stopped$lmg)*100  ### % of R2
Ranking_IMPO_STOPPED <- as.data.frame(IMPO_Illegal_Stopped$lmg.rank)  ### Rank of R2

IMPO_Stats_STOPPED <- cbind(names, Coeff_STOPPED[-1,], STD_ERR_Coeff_STOPPED[-1,], Coeff_STOPPED[1,], SIGNIF_STOPPED, VARIANCE_STOPPED, R2_IMPO_STOPPED, Ranking_IMPO_STOPPED)
IMPO_Stats_STOPPED <- cbind(Row.Names = rownames(IMPO_Stats_STOPPED), IMPO_Stats_STOPPED)
colnames(IMPO_Stats_STOPPED) <- c("Codes", "names", "Coeff. (slope)", "Standard Error(slope)" ,"Intercept", "R2", "Variance", "Importance (% of R2)", "Ranking_R2")
emptyrow <- matrix(c(rep.int(NA,length(IMPO_Stats_STOPPED))),nrow=1,ncol=length(IMPO_Stats_STOPPED))
emptyrow <- data.frame(emptyrow)
colnames(emptyrow) <- colnames(IMPO_Stats_STOPPED)

# rbind the empty row to data
IMPO_Stats_STOPPED <- rbind(IMPO_Stats_STOPPED,emptyrow)
IMPO_Stats_STOPPED$Intercept[length(IMPO_Stats_STOPPED)-1] <- Coeff_STOPPED[1,]
IMPO_Stats_STOPPED$R2[length(IMPO_Stats_STOPPED)-1] <- SIGNIF_STOPPED[1,]
IMPO_Stats_STOPPED$Variance[length(IMPO_Stats_STOPPED)-1] <- VARIANCE_STOPPED[1,]


IMPO_Stats_STOPPED$Intercept[1:nrow(IMPO_Stats_STOPPED)-1] <- "-"
IMPO_Stats_STOPPED$R2[2:nrow(IMPO_Stats_STOPPED)-1] <- "-"
IMPO_Stats_STOPPED$Variance[2:nrow(IMPO_Stats_STOPPED)-1] <- "-"
row.names(IMPO_Stats_STOPPED) <- NULL

write.csv(IMPO_Stats_STOPPED, file = "Illegal_Stopped_IMPORTANCE_Statisitcs.csv", row.names=TRUE)

#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################


# Multilinear regressions for Illegal sites STOPPED versus selected variables

mydata_EXPORT <- mydata %>%
  dplyr::select(ILLEGAL, 
                EA_Illegal_exp,
                Incin, 
                Tax,
                WATE,
              #  POP,
               # DWEL,
                DISP,
                DRY,
                CPI,
                GDP_MARKET)
              #  GDP_Chained,
              #  Petrol_Oil)

mydata_EXPORT <- mydata_EXPORT %>%
  filter(ILLEGAL != "NA")


fit_ILLEGAL_EXPORT <- lm(ILLEGAL ~ EA_Illegal_exp +
                            Incin +
                            Tax +
                            WATE +
                            #  POP +
                            # DWEL +
                            DISP +
                            DRY +
                            CPI +
                            GDP_MARKET,
                          #  GDP_Chained +
                           # Petrol_Oil,
                            data = mydata_EXPORT)  #na.action= na.exclude

names <- c("EA_Illegal_exp",
           "gate-fee for incineration",
           "Landfill tax",
           "E : Water Supply; Sewerage, Waste Management and Remediation Activities (Index) (Seasonally adjusted) (2012 index year)",
           #  "POPULATION_ENGLAND",
           # "DWELLINGS_ENGLAND",
           "DISPOSAL_P",
           "DRYRECYCLE_P",
           "CPI_NOTNATIONAL_INDICIES_BASE2005",
           "GDP_CURRENT_MARKETPRICE")
        #  "Gross Domestic Product: chained volume measures: Seasonally adjusted £m",
        # "Typical retail prices of petroleum products and a crude oil price index")
names <- as.data.frame(names)

summary(fit_ILLEGAL_EXPORT) # show results
SIGNIF_EXPORT<- signif(summary(fit_ILLEGAL_EXPORT)$r.squared, 5) ### R2
SIGNIF_EXPORT <- as.data.frame(SIGNIF_EXPORT)


### Diagnostic plots ##### 
Coeff_EXPORT <- as.data.frame(coefficients(fit_ILLEGAL_EXPORT)) # model coefficients (intercef and slope from the regression)


###### VARIANCE-----------------------------------------------------------

# if we refers to the estimated residual variance
VARIANCE_EXPORT <- summary(fit_ILLEGAL_EXPORT)$sigma
VARIANCE_EXPORT <- as.data.frame(VARIANCE_EXPORT)

############################################################################



###### STANDARD ERROR ---------------------------------------------------
# if we want to refer to the STANDARD ERRORS for the coefficient estimates the we can calucluated the variance in this way
STD_ERR_Coeff_EXPORT <- as.data.frame(summary(fit_ILLEGAL_EXPORT)$coefficients[,2])


library(relaimpo)


###############################################################################################
jpeg('Illegal_Export_events_IMPORTANCE.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

IMPO_Illegal_Export <- calc.relimp(fit_ILLEGAL_EXPORT, type="lmg", rela=TRUE)

plot(IMPO_Illegal_Export, sort = FALSE,
     main ="Relative Importance of Selected Variables vs Illegal export events")

dev.off()
################################################################################################


R2_IMPO_EXPORT <-  as.data.frame(IMPO_Illegal_Export$lmg)*100  ### % of R2
Ranking_IMPO_EXPORT <- as.data.frame(IMPO_Illegal_Export$lmg.rank)  ### Rank of R2


IMPO_Stats_EXPORT <- cbind(names, Coeff_EXPORT[-1,], STD_ERR_Coeff_EXPORT[-1,], Coeff_EXPORT[1,], SIGNIF_EXPORT, VARIANCE_EXPORT, R2_IMPO_EXPORT, Ranking_IMPO_EXPORT)
IMPO_Stats_EXPORT <- cbind(Row.Names = rownames(IMPO_Stats_EXPORT), IMPO_Stats_EXPORT)
colnames(IMPO_Stats_EXPORT) <- c("Codes", "names", "Coeff. (slope)", "Standard Error(slope)", "Intercept", "R2", "Variance", "Importance (% of R2)", "Ranking_R2")
emptyrow <- matrix(c(rep.int(NA,length(IMPO_Stats_EXPORT))),nrow=1,ncol=length(IMPO_Stats_EXPORT))
emptyrow <- data.frame(emptyrow)
colnames(emptyrow) <- colnames(IMPO_Stats_EXPORT)

# rbind the empty row to data
IMPO_Stats_EXPORT <- rbind(IMPO_Stats_EXPORT,emptyrow)
IMPO_Stats_EXPORT$Intercept[length(IMPO_Stats_EXPORT)] <- Coeff_EXPORT[1,]
IMPO_Stats_EXPORT$R2[length(IMPO_Stats_EXPORT)] <- SIGNIF_EXPORT[1,]
IMPO_Stats_EXPORT$Variance[length(IMPO_Stats_EXPORT)] <- VARIANCE_EXPORT[1,]


IMPO_Stats_EXPORT$Intercept[1:nrow(IMPO_Stats_EXPORT)-1] <- "-"
IMPO_Stats_EXPORT$R2[2:nrow(IMPO_Stats_EXPORT)-1] <- "-"
IMPO_Stats_EXPORT$Variance[2:nrow(IMPO_Stats_EXPORT)-1] <- "-"
row.names(IMPO_Stats_EXPORT) <- NULL

write.csv(IMPO_Stats_EXPORT, file = "Illegal_Export_Events_IMPORTANCE_Statisitcs.csv", row.names=TRUE)

#####################################################################################################
#####################################################################################################



fitted_mydata_EXPORT <- cbind( (mydata_EXPORT$EA_Illegal_exp)*IMPO_Stats_EXPORT$`Coeff. (slope)`[1] ,
                        (mydata_EXPORT$Incin)*IMPO_Stats_EXPORT$`Coeff. (slope)`[2],
                        (mydata_EXPORT$Tax)*IMPO_Stats_EXPORT$`Coeff. (slope)`[3],
                        (mydata_EXPORT$WATE)*IMPO_Stats_EXPORT$`Coeff. (slope)`[4],
                        (mydata_EXPORT$DISP)*IMPO_Stats_EXPORT$`Coeff. (slope)`[5],
                        (mydata_EXPORT$DRY)*IMPO_Stats_EXPORT$`Coeff. (slope)`[6],
                        (mydata_EXPORT$CPI)*IMPO_Stats_EXPORT$`Coeff. (slope)`[7] )

#### make row sum....
#### add coeff to total reow sum for each entry

write.csv(fitted_mydata_EXPORT, "fitted.csv")

####################################################################################################
####################################################################################################
                         
  
############## Relative importance of each variable ###############################################
###################################################################################################

IMPO_Illegal_Stopped <- calc.relimp(fit_ILLEGAL_STOPPED, type="lmg", rela=TRUE)
IMPO_Illegal_Stopped <- as.data.frame(IMPO_Illegal_Stopped@lmg)*100
colnames(IMPO_Illegal_Stopped) <- "IMPACT"
IMPO_Illegal_Stopped$variable <- rownames(IMPO_Illegal_Stopped)
IMPO_Illegal_Stopped$media <- "illegal"
row.names(IMPO_Illegal_Stopped) <- NULL


IMPO_Illegal_Export <- calc.relimp(fit_ILLEGAL_EXPORT, type="lmg", rela=TRUE)
IMPO_Illegal_Export <- as.data.frame(IMPO_Illegal_Export@lmg)*100
colnames(IMPO_Illegal_Export) <- "IMPACT"
IMPO_Illegal_Export$variable <- rownames(IMPO_Illegal_Export)
IMPO_Illegal_Export$media <- "export"
row.names(IMPO_Illegal_Export) <- NULL


# rbind all Importance data
IMPACTS_WASTE <- rbind(IMPO_Illegal_Stopped, IMPO_Illegal_Export)

# str(IMPACTS_WASTE)
# 
# # calculate midpoints of bars
# IMPACTS_WASTE <- ddply(IMPACTS_WASTE, .(media), 
#                        transform, pos = cumsum(IMPACT) - (0.5 * IMPACT)
# )

# calculate midpoints of bars
IMPACTS_WASTE$pos <- (IMPACTS_WASTE$IMPACT)/2


jpeg('Relative_Importance_Plot.jpg',
     quality = 100, bg = "white", res = 200, width = 10, height = 7, units = "in")
     par(mar=c(4, 10, 9, 2) + 0.3)
     oldpar <- par(las=1)
     
     q <- ggplot(data = IMPACTS_WASTE, 
                 aes(variable, IMPACT, fill = variable)) +
           theme_bw() + 
              geom_bar(stat = "identity") + facet_grid(. ~ media) +
       theme(strip.text.x = element_text(size = 20, colour = "black")) +
       guides(fill=FALSE) +
       # scale_fill_manual(values=c("#7f7fff", "#7fbf7f", "#ff0000", "#e5e500", "#8e8e8e",
       #                            "#7f7fff", "#7fbf7f", "#ff0000", "#e5e500", "#8e8e8e",
       #                            "#e5e500", "#8e8e8e")) +
       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
       theme(axis.text.x=element_text(size=15,face="bold", colour = "black")) +
       theme(axis.title.x = element_blank()) +                                     
       ylab("Relative Importance (%)\n") +                                                       
       theme(axis.title.y = element_text(face="bold", colour="#990000", size=20),
             axis.text.y  = element_text(angle=0, vjust=0.5, size=17)) +
       #  geom_text(aes(label = paste(round(IMPACT), "%", sep = ""), y = pos), size = 4) +
       geom_text(aes(label = paste(round(IMPACT),"%", sep = "")), size = 4, hjust = 0.5, vjust = -0.5) +
       ggtitle("Relative Importance of each variable") + 
       theme(plot.title = element_text(lineheight=.8, face="bold", size = 18))
     q
     
     par(oldpar)
     dev.off()



