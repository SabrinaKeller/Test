# Importe File
library(readxl)
preg_MatchMaker_wb_1 <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/preg_MatchMaker_wb_1.xlsm", 
                                   sheet = "unique_child_eps_epReg")

library(readxl)
cRF_V1_2_10_ <- read_excel("C:/Users/Sabrina/Desktop/AAASabrina/cRF% (V1.2_10).xlsm", 
                           sheet = "output")

# Importe library
library(psych)
library(ggplot2)
library(scales)
library(dplyr)

# Number of mismatched epitope 
Mm_Ep <- table(preg_MatchMaker_wb_1$PatientID)
Data_Ep <- as.data.frame.table(Mm_Ep)
names(Data_Ep)[names(Data_Ep) == "Var1"] <- "PatientID"
names(Data_Ep)[names(Data_Ep) == "Freq"] <- "Mm_Ep"

# add the cRF
Stack_data <- merge(Data_Ep, cRF_V1_2_10_, by="PatientID")

# quantils
Quantil_Ep<-quantile(Stack_data$Mm_Ep, probs = seq(0, 1, 0.25), na.rm = FALSE,
                     names = TRUE)

Stack_data$Quantil[Stack_data$Mm_Ep < 15] <- "0-14"
Stack_data$Quantil[Stack_data$Mm_Ep < 20 & Stack_data$Mm_Ep > 14] <- "15-19"
Stack_data$Quantil[Stack_data$Mm_Ep < 26 & Stack_data$Mm_Ep > 19] <- "20-25"
Stack_data$Quantil[Stack_data$Mm_Ep > 25] <- "26-52"

Stack_data$cRF_A[Stack_data$ABO_A_cRF < 15.5] <- "0-15 %"
Stack_data$cRF_A[Stack_data$ABO_A_cRF < 50.5 & Stack_data$ABO_A_cRF > 15.49] <- "16-50 %"
Stack_data$cRF_A[Stack_data$ABO_A_cRF < 84.5 & Stack_data$ABO_A_cRF > 50.49] <- "51 - 84 %"
Stack_data$cRF_A[Stack_data$ABO_A_cRF > 84.49] <- "85 - 100 %"

Stack_data$cRF_O[Stack_data$ABO_O_cRF < 15.5] <- "0-15 %"
Stack_data$cRF_O[Stack_data$ABO_O_cRF < 50.5 & Stack_data$ABO_O_cRF > 15.49] <- "16-50 %"
Stack_data$cRF_O[Stack_data$ABO_O_cRF < 84.5 & Stack_data$ABO_O_cRF > 50.49] <- "51 - 84 %"
Stack_data$cRF_O[Stack_data$ABO_O_cRF > 84.49] <- "85 - 100 %"

# get Plot data
plot_data_A <- Stack_data %>% 
  count(cRF_A, Quantil)%>% 
  group_by(Quantil) %>% 
  mutate(percent = n/sum(n))

plot_data_O <- Stack_data %>% 
  count(cRF_O, Quantil)%>% 
  group_by(Quantil) %>% 
  mutate(percent = n/sum(n))

# Plot
ggplot(plot_data_A, aes(x = Quantil, y = percent, fill = cRF_A)) + 
  geom_col(position = "fill") + 
  geom_label(aes(label = percent(percent)), position = "fill", color = "white", vjust = 1, show.legend = FALSE) +
  scale_y_continuous(labels = percent)+
  ggtitle("mismatched Epitope (Blood-type A for cRF)")+
  labs(x="Number of mismatched Epitope", y="Frequency")

ggplot(plot_data_O, aes(x = Quantil, y = percent, fill = cRF_O)) + 
  geom_col(position = "fill") + 
  geom_label(aes(label = percent(percent)), position = "fill", color = "white", vjust = 1, show.legend = FALSE) +
  scale_y_continuous(labels = percent)+
  ggtitle("mismatched Epitope (Blood-type O for cRF)")+
  labs(x="Number of mismatched Epitope", y="Frequency")



