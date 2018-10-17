# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")
# get ggPlot
library(ggplot2)



# unique Values
Matchmaker_wb <- unique(preg_MatchMaker_wb_1 [c("PatientID","Preg" ,"Locus","MFI_baseline",
                                                "Kid_alleles_ep_MFI.Allele","total_epitope")])
# just reactiv epitope
pos_Allele <- subset(Matchmaker_wb,  MFI_baseline > 500)

pos_Allele$Preg_Lable <- pos_Allele$Preg


pos_Allele$Preg_Lable[pos_Allele$Preg == 0] <- 0
pos_Allele$Preg_Lable[pos_Allele$Preg == 1] <- 1
pos_Allele$Preg_Lable[pos_Allele$Preg >1] <- 2


# Plot
ggplot(data=pos_Allele, aes(x=pos_Allele$Locus, y=pos_Allele$total_epitope, group=pos_Allele$Locus)) + 
  geom_boxplot(aes(fill=pos_Allele$Locus)) + facet_grid(as.numeric(pos_Allele$Preg_Lable))

