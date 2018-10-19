library(shiny)
library(dplyr)
library(readxl)

#set dir
setwd("~/MEGAsync/shiny/epitopeData/epData")


#Data
dat<- read.csv("Book1.csv")


cutOff <- 500

#Epitope reaction frequencies can be shown two ways:
# 1) By Allele - only epitopes associate with an allele MM are counted
# 2) By Epitope - all epitope MM are counted regardless of allele MM they were presented on



#Group data by patient and allele MM
 gp1<- dat %>%
  group_by(PatientID, allele, epitope) %>%
  summarise(a_min = min(MFI))
 
 #Add DSA POS-NEG col
 gp1$pos <- gp1$a_min > cutOff
 
gp1$epTot <- apply(gp1[, grep("PatientID", names(gp1))], MARGIN = 1, 
               function(x) sum(x == gp1$PatientID, na.rm = TRUE))

gp1$epPos <- apply(gp1[, grep("PatientID", names(gp1))], MARGIN = 1, 
                    function(x) sum(x == gp1$PatientID & gp1$pos == TRUE, na.rm = T))
# insert function in col based on condition
#myfile %>% mutate(V5 = ifelse(V1 == 1 & V2 != 4, 1, ifelse(V2 == 4 & V3 != 1, 2, 0)))

#Add percentage col
gp1$perc <- (gp1$pos / gp1$epTot) * 100



