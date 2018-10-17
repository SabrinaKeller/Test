# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")

# get ggPlot
library(ggplot2)

# get only Alleles not on SAB
Alleles_notSAB <- subset(preg_MatchMaker_wb_1, is.na(MFI_baseline))

# unique No of Patient
unique_notSAB <- unique(Alleles_notSAB [c("PatientID", "Kid_alleles_ep_MFI.Allele","No_pos_epitope")])

# dot plot
ggplot(unique_notSAB, aes(x=Kid_alleles_ep_MFI.Allele, y=No_pos_epitope)) + 
  geom_dotplot(binaxis='y', stackdir='center', binwidth = NULL, dotsize = 0.25) + 
  labs (x = "Alleles not on SAB", y="No of pos epitopes") + scale_y_continuous(breaks=seq(0,10,1))


  