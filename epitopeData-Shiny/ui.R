shinyUI(
    pageWithSidebar(
      headerPanel("Epitope Mismatches by Allele"),
      
      sidebarPanel(
        #Selector for file upload
          # fileInput('datafile', 'Choose CSV file',
          #       accept=c('text/csv', 'text/comma-separated-values,text/plain')) ,

          uiOutput("toCol"),
        

        
        sliderInput("mfi", "Please select cut off",
                    min = 100, max = 10000, value = 500,step = 100)
 
        
      ),
      
      
   
      
   mainPanel(
       plotOutput("myPlot1"),
       plotOutput("myPlot2"),
       tableOutput("geotable"),
       textOutput("myText")
       
        
           )

)#End pageWithSidebar
    
)#END
