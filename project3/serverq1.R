#shinyServer(

# R Server script for shiny app 608 HW3 Question 1

library(ggplot2)


mdf <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE, stringsAsFactors = FALSE)
dr2010 <- subset(mdf, Year == 2010)
causes <- as.data.frame(unique(dr2010$ICD.Chapter))
colnames(causes) <- 'cause'

shinyServer(function(input, output) {

  crude <- reactive({ crude <- subset(dr2010, ICD.Chapter == input$cause)   })
  output$causePlot <- renderPlot({
    ggplot(crude(), aes(x= Crude.Rate, y= reorder(State, Crude.Rate))) +
      scale_x_continuous(limits=c(0, max(crude()$Crude.Rate) + 4), expand = c(0, 0)) +
      geom_segment(aes(yend=State), xend=0, colour="blue", size = 1.1) +
      geom_point(size=4, colour = "firebrick") +
      theme_bw() +
      theme(panel.grid.major.y = element_blank(), axis.title=element_text(size=14,face="bold"), 
            axis.text.y = element_text(size = 11, face = "bold"), 
            plot.title = element_text(size=16, face = "bold")) +
      xlab("2010 Mortality Rate") +
      ylab("State") +
      ggtitle(input$cause)
  })

})