#Generate a mismatch count and % reativity for each epitope
#against each of its respective alleles.

setwd("~/GitHub/Test/R")
dat <- read.csv("master.csv")
library(dplyr)

#Aim: Group by epitope and get all of its allele mismatches and percentage reactivity

#1 Subset by epitope
#2 Add column to label epitope as POS or NEG based on cutOff
#3 For each MM allele of the souce epitope, count number of times it appears (total MM count)
#--> Count number of times it appears in POS/NEG groups

#myEp <- "114R"
#ep <- subset(dat, epitope == myEp)

#Group data by patient and allele MM
#test <- length(which(ep$PatientID == 15 & ep$epitope == "114R" & ep$MFI > 500))


#Change epitope to levels as apply function doesn't like strings
x <- as.factor(dat$epitope)
levels(x) <- 1:length(levels(x))
x <- as.numeric(x)
dat$x <- x



#ep$Test <- apply(ep[,c('PatientID','MFI')],MARGIN = 1, function(x) length(which(ep$PatientID == x[1] & ep$MFI < 500)))

#Coutnt alleles of epitopes being  over cutoff to help classify them POS or NEG
#subset dat by columns we need  dat [,c('col1', 'col2'...etc      
 dat$Test <- apply(dat[,c('PatientID','x','MFI')], MARGIN = 1, function(x) length(which(dat$PatientID == x[1] & dat$x == x[2] & dat$MFI < 500)))  






