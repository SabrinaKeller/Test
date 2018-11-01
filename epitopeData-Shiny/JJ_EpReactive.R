#Generate a mismatch count and % reativity for each epitope
#against each of its respective alleles.

setwd("~/GitHub/Test/epitopeData-Shiny")
dat <- read.csv("master.csv")
library(dplyr)
library(reshape2)
library(ggplot2)


#Aim: Group by epitope and get all of its allele mismatches and percentage reactivity

#1 Subset by epitope
#2 Add column to label epitope as POS or NEG based on cutOff
#3 For each MM allele of the source epitope, count number of times it appears (total MM count)
#--> Count number of times it appears in POS/NEG groups


#ep <- subset(dat, epitope == myEp)

#Group data by patient and allele MM
#test <- length(which(ep$PatientID == 15 & ep$epitope == "114R" & ep$MFI > 500))


#Change epitope to levels as apply function doesn't like strings
x <- as.factor(dat$epitope)
levels(x) <- 1:length(levels(x))
x <- as.numeric(x)
dat$x <- x



#Count alleles of epitopes being  over cutoff to help classify them POS or NEG
#subset dat by columns we need  dat [,c('col1', 'col2'...etc

#CAN WE DO THIS FOR SUBSET BY EPITOPE INSTEAD? LINE 42 FIRST***************
 dat$condition <- apply(dat[,c('PatientID','x','MFI')], MARGIN = 1, function(x) length(which(dat$PatientID == x[1] & dat$x == x[2] & dat$MFI < 500)))  


#INPUT: select an epitope 
myEp <- "107W"
ep <- subset(dat, dat$epitope == myEp)
#mmCount <- length(unique(ep$PatientID)) #allele MM



#Reduce df to 1 row per Px allele MM
MM <- ep[!duplicated(ep$PatientID), ]

#Count number  NEG epitopes of each MM allele that are the sources of the ep MM
gp <- MM %>%
  group_by(allele, epDSA = condition == 0) %>%
  summarise(epCount = n_distinct(PatientID))

#Apply factor to epDSA for NEG and POS
gp$epDSA <- factor(gp$epDSA, labels = c("NEG", "POS"))




#Stack Plot epitope reactivity that the mismatched alleles give rise to
ggplot(gp, 
       aes(x = allele, y = epCount, fill = reorder(epDSA, desc(epDSA)), label = "Epitope Reactivity")) +
  geom_bar(position = "fill", stat='identity')+
  geom_col()+
  # reverse the data labels the same as the stack order
  geom_text(aes(label=epCount), position =position_stack(vjust = 0.5)) +
  #geom_text(size = 3) +
  coord_flip()+
  ggtitle(myEp) +
  labs(x="Allele", y="Number of Patients")


# Box Plot MFI range of each allele of a given epitope
posEps <- subset(dat, dat$condition == 0)

thisEp <- "107W"
epi <- subset(posEps, epitope == thisEp)


p <- ggplot(epi, aes(x=epAlele, y=MFI)) + 
  geom_boxplot() +
  ggtitle(thisEp) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p

   





