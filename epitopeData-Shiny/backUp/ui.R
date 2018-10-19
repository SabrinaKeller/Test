shinyUI(
    pageWithSidebar(
      headerPanel("Header"),
      
      sidebarPanel(
        selectInput("Distribution", "Please Select distribution type",
                    choices = c("Normal", "Exponential")),
        sliderInput("sampleSize", "Please select sample size",
                    min = 100, max = 5000, value = 1000,step = 100),
        conditionalPanel(condition= "Input.Distribution == 'Normal'",
                         textInput("Mean", "Please Select the mean", 10),
                         textInput("sd", "Please Select Standard Deviation",3)),
        conditionalPanel(condition = "input.Distibution == 'Exponential'",
                         textInput("lambda", "Please select exponential Lambda:",1))
        
      ),
      
      #Selector for file upload
      #fileInput('datafile', 'Choose CSV file',
       #         accept=c('text/csv', 'text/comma-separated-values,text/plain'))
      #These column selectors are dynamically created when the file is loaded
      #uiOutput("fromCol"),
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
          plotOutput("myPlot")
          
      )
    )
        
      
    
)
