

library(shiny)



# R UI script for shiny app for 608 HW3, Problem #2

# load data from Github
m_df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE, stringsAsFactors = FALSE)

# find cause of death 2010 for all states
dr2010 <- subset(m_df, Year == 2010)

# get unique state names
states <- unique(m_df$State)

# select distinct causes of death from data frame
causes <- data.frame(unique(dr2010$ICD.Chapter), stringsAsFactors = FALSE )
colnames(causes) <- "cause"

# remove pregnancy cause due to large amounts of missing data
causes <- subset(causes, cause != "Pregnancy, childbirth and the puerperium")
causes <- as.list(causes$cause)

# Use a fluid Bootstrap layout
shinyUI(fluidPage(    
  
  # Give the page a title
  titlePanel("Mortality Rate State vs National Average, 2000 - 2010"),
  
  # define dropdown
  fluidRow(      
    # Define dropdown for cause of death
    column(9, offset = 0,
           selectInput("cause", "Cause of Death:", choices= causes, width = '100%')
    ),
    # Define dropdown for State
    column(3, offset = 0,
           selectInput("state", "State:", choices= states, width = '100%')
    )
  ), # end fluidRow
  
  hr(),
  
  # helpText("Please wait while data is loaded"),
  plotOutput("MortImpPlot", height = 500, width = 900),
  verbatimTextOutput("stats"),
  
  hr(),
  helpText("")

))
