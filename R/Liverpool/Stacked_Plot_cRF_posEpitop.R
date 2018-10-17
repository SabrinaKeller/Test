# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")

library(readxl)
cRF_V1_2_10_ <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/cRF% (V1.2_10).xlsm", 
                           sheet = "output")

# Importe Library
library(psych)
library(ggplot2)
library(scales)
library(dplyr)

# get table
Stack_data_1 <- merge(cRF_V1_2_10_, preg_MatchMaker_wb_1, by="PatientID")
Stack_data_1u <- unique(Stack_data_1 [c("PatientID", "No_pos_epitope_perID","ABO_A_cRF")])
Stack_data_1u <- subset(Stack_data_1u, No_pos_epitope_perID > "0")

# quartils
Quantil_Ep_R<-quantile(Stack_data_1u$No_pos_epitope_perID, probs = seq(0, 1, 0.25), na.rm = FALSE,
                       names = TRUE)

Stack_data_1u$Quantil_R[Stack_data_1u$No_pos_epitope_perID < 3] <-"1"
Stack_data_1u$Quantil_R[Stack_data_1u$No_pos_epitope_perID < 7 & Stack_data_1u$No_pos_epitope_perID > 2] <- "2"
Stack_data_1u$Quantil_R[Stack_data_1u$No_pos_epitope_perID < 12 & Stack_data_1u$No_pos_epitope_perID > 6] <- "3"
Stack_data_1u$Quantil_R[Stack_data_1u$No_pos_epitope_perID > 11] <- "4"


Stack_data_1u$cRF_A[Stack_data_1u$ABO_A_cRF < 15.5] <- "0-15 %"
Stack_data_1u$cRF_A[Stack_data_1u$ABO_A_cRF < 50.5 & Stack_data_1u$ABO_A_cRF > 15.49] <- "16-50 %"
Stack_data_1u$cRF_A[Stack_data_1u$ABO_A_cRF < 84.5 & Stack_data_1u$ABO_A_cRF > 50.49] <- "51 - 84 %"
Stack_data_1u$cRF_A[Stack_data_1u$ABO_A_cRF > 84.49] <- "85 - 100 %"

# get Plot data
plot_data <- Stack_data_1u %>% 
  count(cRF_A, Quantil_R)%>% 
  group_by(Quantil_R) %>% 
  mutate(percent = n/sum(n))

# stackplot
ggplot(plot_data, aes(x = Quantil_R, y = percent, fill = cRF_A)) + 
  geom_col(position = "fill") + 
  geom_label(aes(label = percent(percent)), position = "fill", color = "white", vjust = 1, show.legend = FALSE) +
  scale_y_continuous(labels = percent)+
  ggtitle("reactive Epitope (Blood-type A for cRF)")+
  labs(x="Number of reactive Epitope", y="Frequency")+
  scale_x_discrete(labels=c("1"="0-2", "2"="3-6", "3"="7-11", "4"="12-27" ))


