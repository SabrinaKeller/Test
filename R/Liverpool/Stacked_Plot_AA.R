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
EMS_AA_mismatches_PregData$Number_AA_MM[is.na(EMS_AA_mismatches_PregData$Number_AA_MM)] <- 0
EMS_AA_mismatches_PregData$Number_AA_MM <- as.numeric(EMS_AA_mismatches_PregData$Number_AA_MM)

# Data tables
Data_AA <- aggregate(EMS_AA_mismatches_PregData$Number_AA_MM,by=list(PatientID=EMS_AA_mismatches_PregData$PatientID), FUN=sum)
colnames(Data_AA)[which(names(Data_AA) == "x")] <- "AA"

# merge Data tables
Data_graph_AA <- merge(Data_AA, cRF_V1_2_10_, by="PatientID")
Data_graph_AA$AA[is.na(Data_graph_AA$AA)] <- 0 
Data_graph_AA$Quantil <- NA

# Quantil
Quantil_AA<-quantile(Data_graph_AA$AA, probs = seq(0, 1, 0.25), na.rm = FALSE,
                      names = TRUE)

Data_graph_AA$Quantil[Data_graph_AA$AA < 10] <-"0-9"
Data_graph_AA$Quantil[Data_graph_AA$AA < 15  & Data_graph_AA$AA > 9] <- "10-14"
Data_graph_AA$Quantil[Data_graph_AA$AA < 19 & Data_graph_AA$AA > 14] <- "15-18"
Data_graph_AA$Quantil[Data_graph_AA$AA > 18] <- "19-39"

# cRF
Data_graph_AA$cRF_A[Data_graph_AA$ABO_A_cRF < 15.5] <- "0-15 %"
Data_graph_AA$cRF_A[Data_graph_AA$ABO_A_cRF < 50.5 & Data_graph_AA$ABO_A_cRF > 15.49] <- "16-50 %"
Data_graph_AA$cRF_A[Data_graph_AA$ABO_A_cRF < 84.5 & Data_graph_AA$ABO_A_cRF > 50.49] <- "51 - 84 %"
Data_graph_AA$cRF_A[Data_graph_AA$ABO_A_cRF > 84.49] <- "85 - 100 %"

# get Plot data
plot_data_AA <- Data_graph_AA %>% 
  count(cRF_A, Quantil)%>% 
  group_by(Quantil) %>% 
  mutate(Percent = n/sum(n))

# Plot
ggplot(plot_data_AA, aes(x = Quantil, y = Percent, fill = cRF_A)) + 
  geom_col(position = "fill") + 
  geom_label(aes(label = percent(Percent)), position = "fill", color = "white", vjust = 1, show.legend = FALSE) +
  scale_y_continuous(labels = percent)+
  ggtitle("AA")+
  labs(x="Number of mismatched Amino acid", y="Frequency")