#Generate a mismatch count and % reativity for each epitope
#against each of its respective alleles.

setwd("~/GitHub/Test/R")
dat <- read.csv("master.csv")
library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)

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
 dat$condition <- apply(dat[,c('PatientID','x','MFI')], MARGIN = 1, function(x) length(which(dat$PatientID == x[1] & dat$x == x[2] & dat$MFI < 500)))  


#INPUT: select an epitope 
myEp <- "114R"
ep <- subset(dat, dat$epitope == myEp)
#mmCount <- length(unique(ep$PatientID)) #allele MM

#Reduce df to 1 row per Px allele MM
MM <- ep[!duplicated(ep$PatientID), ]

#Count number  NEG epitopes of each MM allele that are the sources of the ep MM
gp <- MM %>%
  group_by(allele, epDSA = condition == 0) %>%
  summarise(epNeg = n_distinct(PatientID))

#Count number of POS epitopes where epDSA = TRUE
gp$epPos <- apply(gp[, grep("allele", names(gp))], MARGIN = 1, 
                   function(x) sum(x == gp$allele & gp$epDSA == TRUE, na.rm = T))

#Calc percentages - not currently used
gp$percNEG <- gp$epNeg / (gp$epNeg + gp$epPos) * 100
gp$percPOS <- 100 - gp$percNEG

#drop the epDSA col and melt using epNeg and epPos cols together as labelling
gp <- subset(gp, epDSA == FALSE)

#melt using the cols needed - ie not the percentage cols etc
gpMod <- gp[,c("allele", "epNeg", "epPos")]
dm1 <- melt(gpMod)





#melt in two stages, one for counts and one for percentages
#dm1 <- melt(gp[, c("allele","count","epPos")])
#Make sure cond (pos/neg label) is a factor and revrese it so they stack nicely
#dm1$epDSA <- as.factor(dm1$epDSA)
#dm1$epDSA <- factor(dm1$epDSA, levels = rev(levels(dm1$epDSA)))




#dm2 <- melt(gp[,c("allele", "cond", "percNEG","percPOS")])
#colnames(dm2) <- c("allele", "cond","variable2", "value2")
#dm <- merge(dm1,dm2)            

# From the source:
# "allele" is the column we want to keep the same
# "Test" is the column that contains the names of the new column to put things in
# "measurement" holds the measurements
#data_wide <- dcast(gp, allele ~ cond, value.var="count")

#Change NA to zeros
#data_wide[is.na(data_wide)] <- 0
#Add a percentage col
#data_wide$perc <- round(data_wide$'TRUE' / (data_wide$'TRUE' + data_wide$'FALSE') * 100,2)

#Check levels exist for epNeg and epPos
levels(dm1$variable)



#Plot epitope reactivity that the mismatched alleles give rise to
ggplot(dm1, 
  aes(x = allele, y = value, fill = reorder(variable, desc(variable)), label = reorder(value, desc(value)))) +
  geom_bar(position = "fill", stat='identity')+
  geom_col()+
 # reverse the data labels the same as the stack order
  geom_text(aes(label=value), position =position_stack(vjust = 0.5)) +
#geom_text(size = 3) +
  coord_flip()+
  ggtitle(myEp) +
  labs(x="Allele", y="Number of Patients")


   





