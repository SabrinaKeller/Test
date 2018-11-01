shinyUI(
    pageWithSidebar(
      headerPanel("Epitope Mismatches by Allele"),
      
      sidebarPanel(
        #Selector for file upload
          # fileInput('datafile', 'Choose CSV file',
          #       accept=c('text/csv', 'text/comma-separated-values,text/plain')) ,

          uiOutput("toCol"),
        
        #selectInput( "allele", label = "Please Select allele",
        #            choices = NULL),
        
        sliderInput("mfi", "Please select cut off",
                    min = 100, max = 10000, value = 500,step = 100)
 
        
      ),
      
      
      

      #These column selectors are dynamically created when the file is loaded
     # uiOutput("fromCol"),
      
      #uiOutput("amountflag"),
      #The conditional panel is triggered by the preceding checkbox
     # conditionalPanel(
      #  condition="input.amountflag==true",
       # uiOutput("amountCol")
      #),
      #The action button prevents an action firing before we're ready
      #actionButton("getgeo", "Get geodata")
      
   # )
      
      
      
      
      
      
   mainPanel(
       plotOutput("myPlot1"),
       plotOutput("myPlot2"),
       tableOutput("geotable"),
       textOutput("myText")
       
        
           )

)#End pageWithSidebar
    
)#END
