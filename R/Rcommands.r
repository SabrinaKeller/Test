#Select Folder (directory) where files are
setwd("I:/LAB DOCS/Tissue Typing/AAASabrina/R")

#Check whats in your dierctory
dir()

#Get Data from file and put into a variable
dat <- read.csv(file="Book2.csv", header = TRUE)

#Check the top 6 rows of data in your new variable
head(dat)

mdat <- melt(dat, id=c("Epitope","ElliPro","PolymorphicRes","Verified"))