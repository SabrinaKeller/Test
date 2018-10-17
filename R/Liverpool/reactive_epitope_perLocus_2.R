# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")

# unique Values
Matchmaker_wb <- unique(preg_MatchMaker_wb_1 [c("PatientID","Preg" ,"Locus","MFI_baseline",
                                                "Kid_alleles_ep_MFI.Allele","reactive_epitope", "%_pos_epitope")])
# just reactiv epitope
pos_Allele <- subset(Matchmaker_wb,  MFI_baseline > 500)

# get only one Locus
A_Ep_2 <- subset(pos_Allele, Locus == "A" & Preg > "1")
B_Ep_2 <- subset(pos_Allele, Locus == "B" & Preg > "1") 
C_Ep_2 <- subset(pos_Allele, Locus == "C" & Preg > "1")

# Boxplot of No reactive Epitope by Locus 
boxplot(A_Ep_2$`%_pos_epitope`,B_Ep_2$`%_pos_epitope`,C_Ep_2$`%_pos_epitope`, data=pos_epitope, 
        main="reactive Epitopes per Locus over 1 Pregnancy", names = c("A","B","C"),
        xlab="Locus", ylab="% pos epitope")