# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")
names(preg_MatchMaker_wb_1)[names(preg_MatchMaker_wb_1) == "Kid_alleles_ep_MFI.Allele"] <- "Allele"
library(readxl)
cRF_V1_2_10_Epitopes <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/R/cRF% (V1.2_10)Epitopes.xlsm", 
                                   sheet = "output")

# Importe Library
library(psych)
library(ggplot2)
library(dplyr)

# get Data for the Frequency of Epitope of A*0201 and Frequency of reacted Epitope of A*0201
Allele_A2 <- subset(preg_MatchMaker_wb_1, Allele == "A*0201" & MFI_baseline >500)

Data_Plot1 <- merge(Allele_A2, cRF_V1_2_10_Epitopes, by="Epitope")

Data_Plot1$NO_Ep <- 1

# get Percentage of pos Epitope
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


Data_Frequ <- merge(Data_Allele, Allele_posEpFrequ, by = "Epitope", all.x = TRUE)
Data_Frequ$No_pos_Epitope [is.na(Data_Frequ$No_pos_Epitope)] <- 0

Data_Frequ$Percentage <- (Data_Frequ$No_pos_Epitope/Data_Frequ$No_Epitope*100)

# merge with main table with left join 
Data_Plot <- merge(Data_Plot1, Data_Frequ, by = "Epitope", all.x = TRUE)


# cRF Label
Data_Plot$cRF_A[Data_Plot$ABO_A_cRF < 15.5] <- "0-15 %"
Data_Plot$cRF_A[Data_Plot$ABO_A_cRF < 50.5 & Data_Plot$ABO_A_cRF> 15.49] <- "16-50 %"
Data_Plot$cRF_A[Data_Plot$ABO_A_cRF < 65.5 & Data_Plot$ABO_A_cRF > 50.49] <- "51 - 65 %"
Data_Plot$cRF_A[Data_Plot$ABO_A_cRF < 84.5 & Data_Plot$ABO_A_cRF > 65.49] <- "66 - 84 %"
Data_Plot$cRF_A[Data_Plot$ABO_A_cRF > 84.49] <- "85 - 100 %"

# Plot
ggplot(Data_Plot, aes(x = Epitope, y = NO_Ep, fill = reactive_epitope, 
                      label=sprintf("%0.1f", round(Percentage, digits = 1)))) + 
  geom_bar(position = 'dodge', stat='identity')+
  geom_col()+
  geom_text(size = 3.5, hjust = 1)+
  coord_flip()+
  facet_grid(rows = vars(cRF_A), scales = "free", space = "free")+  theme(strip.text.y = element_text(angle = 0))+
  ggtitle("Allele A*02:01") +
  labs(x="Epitope", y="Number of Patient")