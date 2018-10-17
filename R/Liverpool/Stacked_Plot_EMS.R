# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")
library(readxl)
EMS_AA_mismatches_PregData <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/EMS_AA_mismatches_PregData.xlsx")
library(readxl)
cRF_V1_2_10_ <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/cRF% (V1.2_10).xlsm", 
                           sheet = "output")
library(psych)
library(ggplot2)
library(scales)
library(dplyr)

#fix numbers as strings to numbers
EMS_AA_mismatches_PregData$EMS[is.na(EMS_AA_mismatches_PregData$EMS)] <- 0
EMS_AA_mismatches_PregData$EMS <- as.numeric(EMS_AA_mismatches_PregData$EMS)

# Data tables
preg_Mm <- unique(preg_MatchMaker_wb_1 [c("PatientID", "Kid_alleles_ep_MFI.Allele", "total_epitope_per_allele")])
Data_Mm <- aggregate(preg_Mm$total_epitope_per_allele, by=list(PatientID=preg_Mm$PatientID), FUN=sum)
colnames(Data_Mm)[which(names(Data_Mm) == "x")] <- "No_Mm_Ep"
Data_EMS <- aggregate(EMS_AA_mismatches_PregData$EMS, by=list(PatientID=EMS_AA_mismatches_PregData$PatientID), FUN=sum)
colnames(Data_EMS)[which(names(Data_EMS) == "x")] <- "EMS"

# merge Data tables
Data_graphis_EMS <- merge(Data_Mm, Data_EMS, by="PatientID")
Data_graph_EMS <- merge(Data_graphis_EMS, cRF_V1_2_10_, by="PatientID")

# Quantil
Quantil_EMS<-quantile(Data_graph_EMS$EMS, probs = seq(0, 1, 0.25), na.rm = FALSE,
                       names = TRUE)

Data_graph_EMS$Quantil[Data_graph_EMS$EMS < 12] <-"0-11.9"
Data_graph_EMS$Quantil[Data_graph_EMS$EMS <20.6  & Data_graph_EMS$EMS > 11.9] <- "12.0-20.5"
Data_graph_EMS$Quantil[Data_graph_EMS$EMS < 30.1 & Data_graph_EMS$EMS > 20.5] <- "20.6-30.0"
Data_graph_EMS$Quantil[Data_graph_EMS$EMS > 30.0] <- "30.1-59.3"

# cRF
Data_graph_EMS$cRF_A[Data_graph_EMS$ABO_A_cRF < 15.5] <- "0-15 %"
Data_graph_EMS$cRF_A[Data_graph_EMS$ABO_A_cRF < 50.5 & Data_graph_EMS$ABO_A_cRF > 15.49] <- "16-50 %"
Data_graph_EMS$cRF_A[Data_graph_EMS$ABO_A_cRF < 84.5 & Data_graph_EMS$ABO_A_cRF > 50.49] <- "51 - 84 %"
Data_graph_EMS$cRF_A[Data_graph_EMS$ABO_A_cRF > 84.49] <- "85 - 100 %"

# get Plot data
plot_data <- Data_graph_EMS %>% 
  count(cRF_A, Quantil)%>% 
  group_by(Quantil) %>% 
  mutate(percent = n/sum(n))

# Plot
ggplot(plot_data, aes(x = Quantil, y = percent, fill = cRF_A)) + 
  geom_col(position = "fill") + 
  geom_label(aes(label = percent(percent)), position = "fill", color = "white", vjust = 1, show.legend = FALSE) +
  scale_y_continuous(labels = percent)+
  ggtitle("EMS")+
  labs(x="EMS", y="Frequency")

