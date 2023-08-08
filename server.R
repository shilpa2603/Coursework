# Load libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly) 

# Read in data
data <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-DV0151EN-SkillsNetwork/labs/module_4/starter_code/adult.csv"

download.file(data, destfile = "adult.csv")
adult <- read_csv("adult.csv")
# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Define server logic
shinyServer(function(input, output) {
  
  df_country <- reactive({
    adult %>% filter(native_country == input$country)
  })
  
  # TASK 5: Create logic to plot histogram or boxplot
  output$p1 <- renderPlot({
    data <- read.csv("adult.csv")
    if (input$graph_type == "histogram") {
      # Histogram
      ggplot(df_country(), aes_string(x = input$continuous_variable)) +
        geom_histogram(bins = 30) +  # histogram geom
        labs(y = "Number of People", title = paste("Trend of ", input$continuous_variable))  +  # labels
        facet_wrap(~prediction)    # facet by prediction
    }
    else {
      # Boxplot
      ggplot(df_country(), aes_string(y = input$continuous_variable)) +
        geom_boxplot() +  # boxplot geom
        coord_flip() +  # flip coordinates
        labs(y = "Number of People", title = paste("Trend of ", input$continuous_variable)) +  # labels
        facet_wrap(~prediction)    # facet by prediction
    }
    
  })
  
  # TASK 6: Create logic to plot faceted bar chart or stacked bar chart
  output$p2 <- renderPlot({
    # Bar chart
    p <- ggplot(df_country(), aes_string(x = input$categorical_variable)) +
      ylab= "number of people" +  # labels
        theme(axis.text.x= element_text(angle= 45),
              legend.position= "left")    # modify theme to change text angle and legend position
    
    if (input$is_stacked == TRUE) {
      p + geom_bar(fill="prediction")  # add bar geom and use prediction as fill
    }
    else{
      p + 
        geom_bar(fill= input$categorical_variable) + # add bar geom and use input$categorical_variables as fill 
        facet_wrap(vars(prediction))   # facet by prediction
    }
  })
  
})
