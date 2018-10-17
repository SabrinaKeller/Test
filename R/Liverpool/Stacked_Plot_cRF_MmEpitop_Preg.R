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

# get data table
No_Ep <- table(preg_MatchMaker_wb_1$PatientID)
Frequ_Ep <- as.data.frame.table(No_Ep)
names(Frequ_Ep)[names(Frequ_Ep) == "Var1"] <- "PatientID"
names(Frequ_Ep)[names(Frequ_Ep) == "Freq"] <- "Mm_Ep"

Stack_data_Preg <- merge(Frequ_Ep, cRF_V1_2_10_, by="PatientID")
Stack_data_Preg <- merge(preg_MatchMaker_wb_1, Stack_data_Preg, by="PatientID")

Stack_data_Preg_A <- unique(Stack_data_Preg[c("PatientID",  "Preg", "ABO_A_cRF", "Mm_Ep")])
Stack_data_Preg_0 <- subset(Stack_data_Preg_A, Preg == "0")
Stack_data_Preg_1 <- subset(Stack_data_Preg_A, Preg > "0")

# quartils for first Preg
Quantil_Preg_0<-quantile(Stack_data_Preg_0$Mm_Ep, probs = seq(0, 1, 0.25), na.rm = FALSE,
                         names = TRUE)

Stack_data_Preg_0$Quantil[Stack_data_Preg_0$Mm_Ep < 15] <- "0-14"
Stack_data_Preg_0$Quantil[Stack_data_Preg_0$Mm_Ep < 21 & Stack_data_Preg_0$Mm_Ep > 14] <- "15-20"
Stack_data_Preg_0$Quantil[Stack_data_Preg_0$Mm_Ep < 26 & Stack_data_Preg_0$Mm_Ep > 20] <- "21-25"
Stack_data_Preg_0$Quantil[Stack_data_Preg_0$Mm_Ep > 25] <- "26-52"


Stack_data_Preg_0$cRF_A[Stack_data_Preg_0$ABO_A_cRF < 15.5] <- "0-15 %"
Stack_data_Preg_0$cRF_A[Stack_data_Preg_0$ABO_A_cRF < 50.5 & Stack_data_Preg_0$ABO_A_cRF > 15.49] <- "16-50 %"
Stack_data_Preg_0$cRF_A[Stack_data_Preg_0$ABO_A_cRF < 84.5 & Stack_data_Preg_0$ABO_A_cRF > 50.49] <- "51 - 84 %"
Stack_data_Preg_0$cRF_A[Stack_data_Preg_0$ABO_A_cRF > 84.49] <- "85 - 100 %"

#quartils for >1 Preg

Quantil_Preg_1<-quantile(Stack_data_Preg_1$Mm_Ep, probs = seq(0, 1, 0.25), na.rm = FALSE,
                         names = TRUE)

Stack_data_Preg_1$Quantil[Stack_data_Preg_1$Mm_Ep < 15] <- "0-14"
Stack_data_Preg_1$Quantil[Stack_data_Preg_1$Mm_Ep < 20 & Stack_data_Preg_1$Mm_Ep > 14] <- "15-19"
Stack_data_Preg_1$Quantil[Stack_data_Preg_1$Mm_Ep < 26 & Stack_data_Preg_1$Mm_Ep > 19] <- "20-25"
Stack_data_Preg_1$Quantil[Stack_data_Preg_1$Mm_Ep > 25] <- "26-40"


Stack_data_Preg_1$cRF_A[Stack_data_Preg_1$ABO_A_cRF < 15.5] <- "0-15 %"
Stack_data_Preg_1$cRF_A[Stack_data_Preg_1$ABO_A_cRF < 50.5 & Stack_data_Preg_1$ABO_A_cRF > 15.49] <- "16-50 %"
Stack_data_Preg_1$cRF_A[Stack_data_Preg_1$ABO_A_cRF < 84.5 & Stack_data_Preg_1$ABO_A_cRF > 50.49] <- "51 - 84 %"
Stack_data_Preg_1$cRF_A[Stack_data_Preg_1$ABO_A_cRF > 84.49] <- "85 - 100 %"

# get Plot data
plot_data_Preg <- Stack_data_Preg_0 %>% 
  count(cRF_A, Quantil)%>% 
  group_by(Quantil) %>% 
  mutate(percent = n/sum(n))

plot_data_Preg1 <- Stack_data_Preg_1 %>% 
  count(cRF_A, Quantil)%>% 
  group_by(Quantil) %>% 
  mutate(percent = n/sum(n))

# stackplot
ggplot(plot_data_Preg, aes(x = Quantil, y = percent, fill = cRF_A)) + 
  geom_col(position = "fill") + 
  geom_label(aes(label = percent(percent)), position = "fill", color = "white", vjust = 1, show.legend = FALSE) +
  scale_y_continuous(labels = percent)+
  ggtitle("mismatched Epitope first Pregnancy (Blood-type A for cRF)")+
  labs(x="Number of mismatched Epitope", y="Frequency")

ggplot(plot_data_Preg1, aes(x = Quantil, y = percent, fill = cRF_A)) + 
  geom_col(position = "fill") + 
  geom_label(aes(label = percent(percent)), position = "fill", color = "white", vjust = 1, show.legend = FALSE) +
  scale_y_continuous(labels = percent)+
  ggtitle("mismatched Epitope more than one Pregnancy (Blood-type A for cRF)")+
  labs(x="Number of mismatched Epitope", y="Frequency")
