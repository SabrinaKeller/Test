# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")

# unique Values
Matchmaker_wb <- unique(preg_MatchMaker_wb_1 [c("PatientID","Preg" ,"Locus","MFI_baseline",
                                                "Kid_alleles_ep_MFI.Allele","total_epitope")])
# just reactiv epitope
pos_Allele <- subset(Matchmaker_wb,  MFI_baseline > 500)

# get only one Locus
A_Ep <- subset(pos_Allele, Locus == "A")
B_Ep <- subset(pos_Allele, Locus == "B") 
C_Ep <- subset(pos_Allele, Locus == "C")

# Boxplot of No reactive Epitope by Locus 
boxplot(A_Ep$total_epitope,B_Ep$total_epitope,C_Ep$total_epitope, data=pos_Allele, 
        main="mismatched Epitopes per Locus", names = c("A","B","C"),
        xlab="Locus", ylab="Number of mismatched Epitopes")