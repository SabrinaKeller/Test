shinyServer(
  function(input, output, session){
    
    library(data.table)
    library(dplyr)
    library(reshape2)
    library(ggplot2)
    
    
    
    
    #This function is repsonsible for loading in the selected file
    filedata <- reactive({
      infile <- input$datafile
      # require that infile is not NULL (?req)
      # it will prevent propagating errors 
      req(infile) 
      
      read.csv(infile$datapath)

    })
    

#Create UI for an Input dropdown to select epitopes upon upload of csv file     
    output$toCol <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      

      #items=names(df)
      items <- unique(df$epitope)
      items <- sort(items)
      
      selectInput("epitopeDD", "epitopes:",items)
      

    })
    
   
  # #Make MFI slider reactive  - when 
  #   data <- eventReactive(input$mfi, {
  #   
  #     dat <- filedata()
  #     cutOff <- input$mfi
  #     
  #     
  # 
  #     #Change epitope to levels as apply function doesn't like strings
  #     x <- as.factor(dat$epitope)
  #     levels(x) <- 1:length(levels(x))
  #     x <- as.numeric(x)
  #     dat$x <- x
  #     
  #     #This line takes a while to execute
  #     dat$condition <- apply(dat[,c('PatientID','x','MFI')], MARGIN = 1, function(x) length(which(dat$PatientID == x[1] & dat$x == x[2] & dat$MFI < cutOff)))
  #       
  #       dat
  #   } , ignoreNULL = FALSE)
    
    
    
 
    

    
 
     
     output$myPlot1 <- renderPlot({
      
       #Data is sent from function that fires when MFI changes
       df <- filedata()
       
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
       
       #Apply factor to epDSA for NEG and POS
       #gp$epDSA <- factor(gp$epDSA, labels = c("NEG", "POS"))
       gp$epDSA <- ifelse(gp$epDSA == FALSE,"NEG","TRUE")
       

       #Plot epitope reactivity that the mismatched alleles give rise to
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


    }) #End plot1
     
     output$myPlot2 <- renderPlot({
       
       #Data is sent from function that fires when MFI changes
       df <- filedata()
       
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
       
       ggplot(ep, aes(x=epAlele, y=MFI)) + 
         geom_boxplot() +
         ggtitle(myEp) +
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
       
       
       
       
     }) #End plot2
   
  
  } #function 
) #server