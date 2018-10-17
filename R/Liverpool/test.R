# Percentage for Graphs

No_rows_total <- nrow(Data_graph_EMS[Data_graph_EMS$Quantil == "0-11.9",])
No_rows_1_1 <- nrow(Data_graph_EMS[Data_graph_EMS$Quantil == "0-11.9" & Data_graph_EMS$cRF_A == "0-15 %",])
No_rows_1_2 <- nrow(Data_graph_EMS[Data_graph_EMS$Quantil == "0-11.9" & Data_graph_EMS$cRF_A == "16-50 %",])
No_rows_1_3 <- nrow(Data_graph_EMS[Data_graph_EMS$Quantil == "0-11.9" & Data_graph_EMS$cRF_A == "51 - 84 %",])
No_rows_1_4 <- nrow(Data_graph_EMS[Data_graph_EMS$Quantil == "0-11.9" & Data_graph_EMS$cRF_A == "85 - 100 %",])