library(shiny)
library(ggplot2)

m_df <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv", header = TRUE, stringsAsFactors = FALSE)

dr2010 <- subset(m_df, Year == 2010)

library(tidyr, warn.conflicts = FALSE, quietly=TRUE)
library(dplyr, warn.conflicts = FALSE, quietly=TRUE)

causes <- as.data.frame(unique(dr2010$ICD.Chapter), stringsAsFactors = FALSE)
colnames(causes) <- 'cause'
causes <- subset(causes, cause != "Pregnancy, childbirth and the puerperium")
nat_df <- data.frame(summarise(group_by(m_df, Year, ICD.Chapter), 
                               Deaths = sum(as.numeric(Deaths)),
                               Population = sum(as.numeric(Population)),
                               Crude.Rate = round(Deaths/Population * 100000, 3) ) )

for(i in 1999:2010) {
  yr_cl <- subset(nat_df, Year == i)
  maxpop <- max(yr_cl$Population)
  nat_df <- transform(nat_df, Population = ifelse(Year == i, maxpop, Population))
  nat_df$Crude.Rate <- round(nat_df$Deaths / nat_df$Population * 100000, 3)
}


nat_imp <- data.frame(Year = numeric(0), Cause = character(0), Change = numeric(0), stringsAsFactors = FALSE)

for (yr in 2000:2010) {
  for (i in 1:nrow(causes)) {
    old_rate <- subset(nat_df, ICD.Chapter == causes$cause[i] & Year == yr-1, select=c(Crude.Rate))
    colnames(old_rate) <- "Change"
    new_rate <- subset(nat_df, ICD.Chapter == causes$cause[i] & Year == yr, select=c(Crude.Rate))
    colnames(new_rate) <- "Change"
    if(nrow(old_rate) > 0 & nrow(new_rate) > 0) {
      nat_imp <- rbind(nat_imp, data.frame(Year = yr, Cause = causes$cause[i], 
                                           Change = round((new_rate - old_rate)/old_rate, 3)))
    } 
  } 
} 

nat_imp$Change = nat_imp$Change * 100
state_imp <- data.frame(Year = numeric(0), Change = numeric(0), stringsAsFactors = FALSE)

shinyServer(function(input, output) {

  natl_dat <- reactive({ natl_dat <- subset(nat_imp, Cause == input$cause) })
  output$MortImpPlot <- renderPlot({
    state_dat <- subset(m_df, ICD.Chapter == input$cause & State == input$state, 
                        select=c(Year, Crude.Rate))

    for (yr in 2000:2010) {
      old_rate <- subset(state_dat, Year == yr - 1, select=c(Crude.Rate))
      colnames(old_rate) <- "Change"
      
      new_rate <- subset(state_dat, Year == yr, select=c(Crude.Rate))
      colnames(new_rate) <- "Change"
      
      if(nrow(old_rate) > 0 & nrow(new_rate) > 0) {
        state_imp <- rbind(state_imp, data.frame(Year = yr, 
                                                 Change = round((new_rate - old_rate)/old_rate, 4)))
      } 
      
    } 

    state_imp$Change = state_imp$Change * 100
    plot_dat <- data.frame("State_Rate" = state_imp$Change, "National_Avg" = natl_dat()$Change)
    x = seq(2000, 2010, by = 1)
    
    ggplot(plot_dat, aes(x = x) ) + 
      geom_smooth(aes(y = State_Rate, col = "State_Rate"), se = FALSE, lty = 1) +
      geom_smooth(aes(y = National_Avg, col = "National_Avg"), se = FALSE, lty = 1) +
      geom_hline(yintercept=0, linetype="dashed") +
      
      theme(panel.grid.major.y = element_blank(), axis.title=element_text(size=15,face="bold"), 
            axis.text.y = element_text(size = 13, face = "bold"), 
            plot.title = element_text(size=16, face = "bold"),
            axis.text.x = element_text(size = 13, face = "bold")) +
      
      scale_x_continuous(breaks= seq(2000, 2010, by=1), expand = c(0, 0.2)) +
      ylab(label = "Average % Change in Mortality Rate") +
      xlab(label = "Year") +
      scale_colour_manual(name = "Legend", values=c("State_Rate" = "blue", "National_Avg" = "red")) +
      ggtitle(input$cause)
    
  })


  
})
