#Shiny App for Body Fat Prediction with a Linear Regression Model

library(shiny)
library(ggplot2)
library(caret)


bodyfat_data <- read.csv("bodyfat.csv")

# Create age groups
bodyfat_data$age_groups <- cut(bodyfat_data$Age, 
                               breaks = c(20, 39, 46, 56, Inf), 
                               labels = c("20-39", "40-46", "47-55", "56+"), 
                               right = FALSE)

# Split data to train and test sets
set.seed(102)
splitIndex <- createDataPartition(bodyfat_data$BodyFat, p = 0.8, list = FALSE, times = 1)
train_data <- bodyfat_data[splitIndex, ]
test_data <- bodyfat_data[-splitIndex, ]

# Train the models
model1 <- lm(BodyFat ~ Abdomen + Chest + Density, data = train_data)
model2 <- lm(BodyFat ~ Abdomen + Chest, data = train_data)

# UI
ui <- fluidPage(
  titlePanel("BodyFat Prediction for Age Groups"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "age_group",
        label = "Select Age Group:",
        choices = c("All Ages", "20-39", "40-46", "47-55", "56+"),
        selected = "All Ages"
      )
    ),
    mainPanel(
      fluidRow(
        column(6, 
               plotOutput("predictionPlot1"),
               textOutput("metricsModel1")
        ), 
        column(6, 
               plotOutput("predictionPlot2"),
               textOutput("metricsModel2")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  #function to subset data
  get_test_data <- function() {
    if (input$age_group == "All Ages") {
      return(test_data)
    }
    subset(test_data, age_groups == input$age_group)
  }
  
  # Plot for Model 1
  output$predictionPlot1 <- renderPlot({
    subset_test_data <- get_test_data()
    
    if (nrow(subset_test_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data for this selection", size = 6, hjust = 0.5) +
               theme_void())
    }
    
    predictions <- predict(model1, newdata = subset_test_data)
    
    ggplot(subset_test_data, aes(x = predictions, y = BodyFat)) +
      geom_point() +
      geom_smooth(method = "lm", color = "red") +
      labs(
        title = paste("Model 1: Actual vs Predicted BodyFat (", input$age_group, ")"),
        x = "Predicted BodyFat",
        y = "Actual BodyFat"
      ) +
      theme_minimal()
  })
  
  # Model 1: RMSEP and R^2
  output$metricsModel1 <- renderText({
    subset_test_data <- get_test_data()
    
#    if (nrow(subset_test_data) == 0) {
#      return("No data available for this selection.")
#    }
    
    predictions <- predict(model1, newdata = subset_test_data)
    PRESS <- sum((subset_test_data$BodyFat - predictions)^2)
    RMSEP <- sqrt(PRESS / nrow(subset_test_data))
    SST <- sum((subset_test_data$BodyFat - mean(subset_test_data$BodyFat))^2)
    R2 <- 1 - (PRESS / SST)
    
    paste0("Model 1 Metrics:\nRMSEP = ", round(RMSEP, 2), "\nR² = ", round(R2, 2))
  })
  
  # Plot for Model 2
  output$predictionPlot2 <- renderPlot({
    subset_test_data <- get_test_data()
    
    if (nrow(subset_test_data) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data for this selection", size = 6, hjust = 0.5) +
               theme_void())
    }
    
    predictions <- predict(model2, newdata = subset_test_data)
    
    ggplot(subset_test_data, aes(x = predictions, y = BodyFat)) +
      geom_point() +
      geom_smooth(method = "lm", color = "blue") +
      labs(
        title = paste("Model 2: Actual vs Predicted BodyFat (", input$age_group, ")"),
        x = "Predicted BodyFat",
        y = "Actual BodyFat"
      ) +
      theme_minimal()
  })
  
  # Model 2: RMSEP and R^2
  output$metricsModel2 <- renderText({
    subset_test_data <- get_test_data()
    
    if (nrow(subset_test_data) == 0) {
      return("No data available for this selection.")
    }
    
    predictions <- predict(model2, newdata = subset_test_data)
    PRESS <- sum((subset_test_data$BodyFat - predictions)^2)
    RMSEP <- sqrt(PRESS / nrow(subset_test_data))
    SST <- sum((subset_test_data$BodyFat - mean(subset_test_data$BodyFat))^2)
    R2 <- 1 - (PRESS / SST)
    
    paste0("Model 2 Metrics:\nRMSEP = ", round(RMSEP, 2), "\nR² = ", round(R2, 2))
  })
}

shinyApp(ui = ui, server = server)

