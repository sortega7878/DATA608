library(shiny)


mdf <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE, stringsAsFactors = FALSE)

dr2010 <- subset(mdf, Year == 2010)
causes <- unique(dr2010$ICD.Chapter)

shinyUI(fluidPage(    

  titlePanel("Mortality Rates by State on 2010"),
  fluidRow(      
    column(9, offset = 0,
           selectInput("cause", "Cause of Death:", choices= causes, width = '100%')
    ) 
    
  ),
  
  plotOutput("causePlot", height = 700, width = 700),
  verbatimTextOutput("stats"),
  
  hr(),
  helpText("")
))
