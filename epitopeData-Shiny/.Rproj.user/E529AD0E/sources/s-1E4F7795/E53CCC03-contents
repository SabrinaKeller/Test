shinyUI(
    pageWithSidebar(
      headerPanel("Header"),
      
      sidebarPanel(
        #Selector for file upload
          fileInput('datafile', 'Choose CSV file',
                accept=c('text/csv', 'text/comma-separated-values,text/plain')) ,               
        
        
        
        selectInput("allele", "Please Select allele",
                    choices = c("A*01:01", "A*02:01")),
        sliderInput("mfi", "Please select cut off",
                    min = 20, max = 10000, value = 500,step = 20)
        
        
#        conditionalPanel(condition= "Input.Distribution == 'Normal'",
#                         textInput("Mean", "Please Select the mean", 10),
#                         textInput("sd", "Please Select Standard Deviation",3)),
#        conditionalPanel(condition = "input.Distibution == 'Exponential'",
#                         textInput("lambda", "Please select exponential Lambda:",1))
        
      ),
      

      #These column selectors are dynamically created when the file is loaded
     # uiOutput("fromCol"),
      #uiOutput("toCol"),
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
       plotOutput("myPlot"),
       tableOutput("geotable")
        
           )

)#End pageWithSidebar
    
)#END
