# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")
View(preg_MatchMaker_wb_1)

# get only one Locus

Alocus <- subset(preg_MatchMaker_wb_1, Locus == "A")
Blocus <- subset(preg_MatchMaker_wb_1, Locus =="B") 
Clocus <- subset(preg_MatchMaker_wb_1, Locus == "C")

# get only mothers with 1 Pregnancy

Alocus_1 <- subset(Alocus, Preg == "1")
Blocus_1 <- subset(Blocus, Preg == "1")
Clocus_1 <- subset(Clocus, Preg == "1")

# No of all mothers

No_ALocus_1 <- nrow(unique(Alocus_1 ["PatientID"]))
No_BLocus_1 <- nrow(unique(Blocus_1 ["PatientID"]))
No_CLocus_1 <- nrow(unique(Clocus_1 ["PatientID"]))

# Add the missing patients; epitopes are the same in mother and child 
No_All_Patients_A_1 <- No_ALocus_1+2
No_All_Patients_B_1 <- No_BLocus_1+3
No_All_Patients_C_1 <- No_CLocus_1+1

# Remove duplicates
No_A_Alleles_1 <- unique(Alocus_1 [c("PatientID" , "Kid_alleles_ep_MFI.Allele","MFI_baseline")])
No_B_Alleles_1 <- unique(Blocus_1 [c("PatientID" , "Kid_alleles_ep_MFI.Allele","MFI_baseline")])
No_C_Alleles_1 <- unique(Clocus_1 [c("PatientID" , "Kid_alleles_ep_MFI.Allele","MFI_baseline")])

# Sum reactive Allele
No_R_A_Alleles_1 <- length(which(No_A_Alleles_1$`MFI_baseline` >"500"))
No_R_B_Alleles_1 <- length(which(No_B_Alleles_1$`MFI_baseline` >"500"))
No_R_C_Alleles_1<- length(which(No_C_Alleles_1$`MFI_baseline` >"500"))

# calculate the percentage of reactive mothers
mother_R_A_1 <- No_R_A_Alleles_1/No_All_Patients_A_1*100
mother_R_B_1 <- No_R_B_Alleles_1/No_All_Patients_B_1*100
mother_R_C_1 <- No_R_C_Alleles_1/No_All_Patients_C_1*100

# Barplot
barplot(c(mother_R_A_1, mother_R_B_1, mother_R_C_1),
        names.arg=c("A","B","C"),
        ylim=c(0,25),
        col=c("BLUE","GREEN","RED"),
        ylab="% of mother reacted",
        xlab="Locus",
        main = "Mother reacting 1 Pregnancy")



