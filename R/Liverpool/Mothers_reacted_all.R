# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")
View(preg_MatchMaker_wb_1)

# get only one Locus

Alocus <- subset(preg_MatchMaker_wb_1, Locus == "A")
Blocus <- subset(preg_MatchMaker_wb_1, Locus =="B") 
Clocus <- subset(preg_MatchMaker_wb_1, Locus == "C")


# No of all mothers

No_ALocus <- nrow(unique(Alocus ["PatientID"]))
No_BLocus <- nrow(unique(Blocus ["PatientID"]))
No_CLocus <- nrow(unique(Clocus ["PatientID"]))

# Add the missing patients; epitopes are the same in mother and child 
No_All_Patients_A <- No_ALocus+5
No_All_Patients_B <- No_BLocus+17
No_All_Patients_C <- No_CLocus+6


# Remove duplicates
No_A_Alleles <- unique(Alocus [c("PatientID" , "Kid_alleles_ep_MFI.Allele","MFI_baseline")])
No_B_Alleles <- unique(Blocus [c("PatientID" , "Kid_alleles_ep_MFI.Allele","MFI_baseline")])
No_C_Alleles <- unique(Clocus [c("PatientID" , "Kid_alleles_ep_MFI.Allele","MFI_baseline")])

# Sum reactive Allele

No_R_A_Alleles <- length(which(No_A_Alleles$`MFI_baseline` >"500"))
No_R_B_Alleles <- length(which(No_B_Alleles$`MFI_baseline` >"500"))
No_R_C_Alleles <- length(which(No_C_Alleles$`MFI_baseline` >"500"))



# calculate the percentage of reactive mothers

mother_R_A <- No_R_A_Alleles/No_All_Patients_A*100
mother_R_B <- No_R_B_Alleles/No_All_Patients_B*100
mother_R_C <- No_R_C_Alleles/No_All_Patients_C*100

# Create data
data=data.frame(Locus=c("A","B","C") ,  "reactive mothers in %" =c(mother_R_A, mother_R_B, mother_R_C))
# Barplot
barplot(c(mother_R_A, mother_R_B, mother_R_C),
        names.arg=c("A","B","C"),
        ylim=c(0,25),
        col=c("BLUE","GREEN","RED"),
        ylab="% of mother reacted",
        xlab="Locus",
        main = "Mother reacting over all")



