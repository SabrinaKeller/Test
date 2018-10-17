# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")
names(preg_MatchMaker_wb_1)[names(preg_MatchMaker_wb_1) == "Kid_alleles_ep_MFI.Allele"] <- "Allele"
library(readxl)
alleleEps <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/AccessDB/alleleEps.xlsx")

library(readxl)
cRF_V1_2_10_Epitopes <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/R/cRF% (V1.2_10)Epitopes.xlsm", 
                                   sheet = "output")


# Importe Library
library(psych)
library(ggplot2)
library(dplyr)

# get Data for the Frequency of Epitope of A*0201 and Frequency of reacted Epitope of A*0201
Allele_A2 <- subset(preg_MatchMaker_wb_1, Allele == "A*0201" & MFI_baseline >500)
Allele_A2u <- unique(Allele_A2 [c( "PatientID", "Epitope", "All_Alleles_Pos" ) ])
Allele_A2F <- table(Allele_A2$Epitope)
Data_Allele <- as.data.frame.table(Allele_A2F)
names(Data_Allele)[names(Data_Allele) == "Var1"] <- "Epitope"
names(Data_Allele)[names(Data_Allele) == "Freq"] <- "No_Epitope"
Allele_posEp <- subset(Allele_A2u, All_Alleles_Pos == "Pos")
Allele_posEpF <- table(Allele_posEp$Epitope)
Allele_posEpFrequ <- as.data.frame.table(Allele_posEpF)
names(Allele_posEpFrequ)[names(Allele_posEpFrequ) == "Var1"] <- "Epitope"
names(Allele_posEpFrequ)[names(Allele_posEpFrequ) == "Freq"] <- "No_pos_Epitope"

# Left Join 
Data_Frequ <- merge(Data_Allele, Allele_posEpFrequ, by = "Epitope", all.x = TRUE)
Data_Frequ$No_pos_Epitope [is.na(Data_Frequ$No_pos_Epitope)] <- 0


# merge with cRF
 Data_Plot <- merge(Data_Frequ, cRF_V1_2_10_Epitopes, by="Epitope")
 Data_Plot$Percentage <- (Data_Plot$No_pos_Epitope/Data_Plot$No_Epitope*100)

 
# cRF Label
Data_Plot$cRF_A[Data_Plot$ABO_A_cRF < 15.5] <- "0-15 %"
Data_Plot$cRF_A[Data_Plot$ABO_A_cRF < 50.5 & Data_Plot$ABO_A_cRF> 15.49] <- "16-50 %"
Data_Plot$cRF_A[Data_Plot$ABO_A_cRF < 84.5 & Data_Plot$ABO_A_cRF > 50.49] <- "51 - 84 %"
Data_Plot$cRF_A[Data_Plot$ABO_A_cRF > 84.49] <- "85 - 100 %"



# get Data for the Frequency of reacted Epitope per Patient 
Re_Ep <- subset(preg_MatchMaker_wb_1, reactive_epitope == "Pos" & MFI_baseline >500)
Frequ_Re_Ep1 <- table(preg_MatchMaker_wb_1$Epitope)
Frequ_Re_Ep <- as.data.frame.table(Frequ_Re_Ep1)
names(Frequ_Re_Ep)[names(Frequ_Re_Ep) == "Var1"] <- "Epitope"
names(Frequ_Re_Ep)[names(Frequ_Re_Ep) == "Freq"] <- "No_pos_Patients"
Data_Plot$No_neg_Epitope <- (Data_Plot$No_Epitope - Data_Plot$No_pos_Epitope)

# Plot
ggplot(Data_Plot, aes(x = Epitope, y = No_Epitope, fill = c(No_pos_Epitope, No_neg_Epitope), label=
                        sprintf("%0.1f", round(Percentage, digits = 1)))) + 
  geom_col()+
  geom_text(size = 3.5, hjust = 0)+
  coord_flip()+
  ggtitle("Allele A*02:01 (n(mm epitope) = 31; n(pos epitope)=16)") +
  labs(x="Epitope", y="pos Epitope")



