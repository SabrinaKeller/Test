shinyServer(
  function(input, output, session){
    
    library(data.table)
    library(dplyr)
    library(reshape2)
    library(ggplot2)
    
    

#Create UI for an Input dropdown to select epitopes upon upload of csv file     
    output$toCol <- renderUI({
      df <- read.csv("master.csv") 
      #df <- filedata()
      if (is.null(df)) return(NULL)
      
      

      

      #items=names(df)
      items <- unique(df$epitope)
      items <- sort(items)
      
      selectInput("epitopeDD", "epitopes:",items)
      

    })
    
   

     
     output$myPlot1 <- renderPlot({
      
       #Data is sent from function that fires when MFI changes
       df <- read.csv("master.csv") 
       #df <- filedata()
       
       #Plot will change upon input changes
       myEp <- input$epitopeDD
       ep <- subset(df, df$epitope == myEp)
       cutOff <- input$mfi
       
       #Change epitope to levels as apply function doesn't like strings
       x <- as.factor(ep$epitope)
       levels(x) <- 1:length(levels(x))
       x <- as.numeric(x)
       ep$x <- x
       
       #Add col to count number of each ep alleles below cutoff
       ep$condition <- apply(ep[,c('PatientID','x','MFI')], MARGIN = 1, function(x) length(which(ep$PatientID == x[1] & ep$x == x[2] & ep$MFI < cutOff)))
       
       #Reduce df to 1 row per Px allele MM
       MM <- ep[!duplicated(ep$PatientID), ]

       #Count number  NEG epitopes of each MM allele that are the sources of the ep MM
       gp <- MM %>%
         group_by(allele, epDSA = condition == 0) %>%
         summarise(epCount = n_distinct(PatientID))
       
       
       #Summary counts
       PNtots <- aggregate(epCount ~ epDSA, data = gp, sum)
       negTot <- PNtots[1,2]
       posTot <- PNtots[2,2]
       
       #percs
       negPerc <- round(negTot / (negTot + posTot) * 100,2)
       posPerc <- round(posTot / (negTot + posTot) * 100,2)
       
       #Prepare chart titles
       #plot1_title <- myEp & "Neg V Pos: " & netTot " / " & posTot & " Pos perc = " & posPerc
       stackPlot_title <- paste(c("Epitope:", myEp,",Neg V Pos: ", negTot, "/", posTot," Pos perc = ",posPerc, "%"), collapse = " ")
       
       
       #Apply factor to epDSA for NEG and POS
       gp$epDSA <- ifelse(gp$epDSA == FALSE,"NEG","POS")
       
   
       
       #Plot epitope reactivity that the mismatched alleles give rise to
       ggplot(gp, 
        aes(x = allele, y = epCount, fill = reorder(epDSA, desc(epDSA)), label = "Epitope Reactivity")) +
         #aes(x = allele, y = epCount, fill = myFill, label = "Epitope Reactivity")) +
         #) +
         geom_bar( position = "fill", stat='identity') +

         scale_fill_manual(values = myFill) +
         #scale_fill_manual(values = c("red", "blue")) +

         geom_col()+
           
         # reverse the data labels the same as the stack order
         geom_text(aes(label=epCount), position =position_stack(vjust = 0.5)) +
         coord_flip()+
         ggtitle(plot1_title) +
         labs(x="Allele", y="Number of Patients")


    }) #End plot1
     
     output$myPlot2 <- renderPlot({
       
       #Data is sent from function that fires when MFI changes
       df <- read.csv("master.csv") 

       
       #Plot will change upon input changes
       myEp <- input$epitopeDD
       ep <- subset(df, df$epitope == myEp)
       cutOff <- input$mfi 
         
       
       #Change epitope to levels as apply function doesn't like strings
       x <- as.factor(ep$epitope)
       levels(x) <- 1:length(levels(x))
       x <- as.numeric(x)
       ep$x <- x
       
       #Add col to count number of each ep alleles below cutoff
       ep$condition <- apply(ep[,c('PatientID','x','MFI')], MARGIN = 1, function(x) length(which(ep$PatientID == x[1] & ep$x == x[2] & ep$MFI < cutOff)))
 
       
       # Plot MFI range of each allele of a given epitope
       ep <- subset(ep, ep$condition == 0)
       posCount <- nrow(ep[!duplicated(ep$PatientID), ])
       
       #Title
       boxPlot_title <-   paste(c("Epitope:", myEp, ",Positive epitopes N = ", posCount), collapse = " ")
       
       ggplot(ep, aes(x=epAlele, y=MFI)) + 
         geom_boxplot() +
         ggtitle(boxPlot_title) +
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
       
       
       
       
     }) #End plot2
     

   
  
  } #function 


) #server


