shinyServer(
  function(input, output, session){
    
    library(data.table)
    
    #This function is repsonsible for loading in the selected file
    filedata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      read.csv(infile$datapath)
    })
    
    
    #Functioin to draw plot
    output$myPlot <- renderPlot({
      
      #get file contents
      df <- filedata()
      test <- mean(df$MFI, na.rm=TRUE)
      tbl <- table(df[1:5,])
      
      DT = data.table(
        ID = c("b","b","b","a","a","c"),
        a = 1:6,
        b = 7:12,
        c = 13:18
      )
 
      #tbl <- setDT(df, keep.rownames=FALSE, key=NULL, check.names=FALSE)
      
      
   
       
      output$geotable <- renderTable(DT)

      
      
      
      alleleSelect <- input$allele
      cutOff <- input$mfi
      
      if(alleleSelect == "A*01:01"){
        
        randomVec <- rnorm(1000, mean= test, sd= 100)
      }
      else {
        randomVec <- rexp(5000,rate = 1 / 50)
      }
      
      hist(randomVec, col = "blue")
      
    })
    
    
    
  }
  
  
  
  
  
)