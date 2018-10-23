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
      df1 <- filedata()
      if (is.null(df1)) return(NULL)
      
      
      
       
      #items=names(df)
      items <- unique(df1$epitope)
      selectInput("epitopeDD", "epitopes:",items)
      

    })
    
   
    
    # user_choice <- eventReactive(input$mfi, {
    # 
    # 
    # 
    #   #Change epitope to levels as apply function doesn't like strings
    #   x <- as.factor(df$epitope)
    #   levels(x) <- 1:length(levels(x))
    #   x <- as.numeric(x)
    #   df$x <- x
    # 
    #     df$condition <- apply(df[,c('PatientID','x','MFI')], MARGIN = 1, function(x) length(which(df$PatientID == x[1] & df$x == x[2] & df$MFI < 500)))
    #     df
    # } , ignoreNULL = FALSE)
    
    
    

    

    
 
     
     output$myPlot <- renderPlot({
       
       cutOff <- input$mfi
       df <- filedata()
       
       #Change epitope to levels as apply function doesn't like strings
       x <- as.factor(df$epitope)
       levels(x) <- 1:length(levels(x))
       x <- as.numeric(x)
       df$x <- x
       
      # df2 <- user_choice()
      

      # #Count alleles of epitopes being  over cutoff to help classify them POS or NEG
      # #subset dat by columns we need  dat [,c('col1', 'col2'...etc
      df$condition <- apply(df[,c('PatientID','x','MFI')], MARGIN = 1, function(x) length(which(df$PatientID == x[1] & df$x == x[2] & df$MFI < 500)))
       

        #user_choice()
       
      # #INPUT: select an epitope
      # #myEp <- "114R"
      myEp <- input$epitopeDD
      ep <- subset(df, df$epitope == myEp)
      #user_choice()
     #Reduce df to 1 row per Px allele MM
      MM <- ep[!duplicated(ep$PatientID), ]
      # 
      # #Count number  NEG epitopes of each MM allele that are the sources of the ep MM
      gp <- MM %>%
        group_by(allele, epDSA = condition == 0) %>%
        summarise(epNeg = n_distinct(PatientID))
      # 
      # #Count number of POS epitopes where epDSA = TRUE
      gp$epPos <- apply(gp[, grep("allele", names(gp))], MARGIN = 1,
                        function(x) sum(x == gp$allele & gp$epDSA == TRUE, na.rm = T))
      # 
      # #drop the epDSA col and melt using epNeg and epPos cols together as labelling
       gp <- subset(gp, epDSA == FALSE)
      # 
      # #melt using the cols needed - ie not the percentage cols etc
      gpMod <- gp[,c("allele", "epNeg", "epPos")]
      dm1 <- melt(gpMod)
      # 
      # #Plot epitope reactivity that the mismatched alleles give rise to
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









    }) #End plot
    
    
    
    
    

  
  
  
  
  } 
)