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

Alocus_2 <- subset(Alocus, Preg > "1")
Blocus_2 <- subset(Blocus, Preg > "1")
Clocus_2 <- subset(Clocus, Preg > "1")

# No of all mothers

No_ALocus_2 <- nrow(unique(Alocus_2 ["PatientID"]))
No_BLocus_2 <- nrow(unique(Blocus_2 ["PatientID"]))
No_CLocus_2 <- nrow(unique(Clocus_2 ["PatientID"]))

# Add the missing patients; epitopes are the same in mother and child 
No_All_Patients_A_2 <- No_ALocus_2+1
No_All_Patients_B_2 <- No_BLocus_2+1
No_All_Patients_C_2 <- No_CLocus_2+2

# Remove duplicates
No_A_Alleles_2 <- unique(Alocus_2 [c("PatientID" , "Kid_alleles_ep_MFI.Allele","MFI_baseline")])
No_B_Alleles_2 <- unique(Blocus_2 [c("PatientID" , "Kid_alleles_ep_MFI.Allele","MFI_baseline")])
No_C_Alleles_2 <- unique(Clocus_2 [c("PatientID" , "Kid_alleles_ep_MFI.Allele","MFI_baseline")])

# Sum reactive Allele
No_R_A_Alleles_2 <- length(which(No_A_Alleles_2$`MFI_baseline` >"500"))
No_R_B_Alleles_2 <- length(which(No_B_Alleles_2$`MFI_baseline` >"500"))
No_R_C_Alleles_2 <- length(which(No_C_Alleles_2$`MFI_baseline` >"500"))

# calculate the percentage of reactive mothers
mother_R_A_2 <- No_R_A_Alleles_2/No_All_Patients_A_2*100
mother_R_B_2 <- No_R_B_Alleles_2/No_All_Patients_B_2*100
mother_R_C_2 <- No_R_C_Alleles_2/No_All_Patients_C_2*100

# Barplot
barplot(c(mother_R_A_2, mother_R_B_2, mother_R_C_2),
        names.arg=c("A","B","C"),
        ylim=c(0,25),
        col=c("BLUE","GREEN","RED"),
        ylab="% of mother reacted",
        xlab="Locus",
        main = "Mother reacting over 1 Pregnancy")



