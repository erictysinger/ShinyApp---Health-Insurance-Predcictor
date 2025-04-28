# Install and load necessary packages
library(shiny)
library(dplyr)
library(ggplot2)
library(reticulate)

# Source the Python script which contains the random forest model and predict_insurance function.
# Ensure "model_rf.py" is in your working directory or specify its full path.
source_python("/Users/michaelwhitfield/school-work/mgmt-389/model_rf.py")

# Define UI for the app
ui <- fluidPage(
  titlePanel("Health Insurance Charge Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age:", value = 30, min = 18, max = 100),
      numericInput("bmi", "BMI:", value = 25, min = 10, max = 50),
      numericInput("children", "Number of Children:", value = 0, min = 0, max = 10),
      selectInput("smoker", "Smoker:", choices = c("yes", "no")),
      selectInput("region", "Region:", choices = c("northeast", "northwest", "southeast", "southwest")),
      # New inputs to match the training features
      selectInput("sex", "Sex:", choices = c("male", "female")),
      selectInput("risk_level", "Risk Level:", choices = c("low", "medium", "high")),
      actionButton("predict", "Predict Charges")
    ),
    mainPanel(
      htmlOutput("prediction"),
      fluidRow(
        column(6, plotOutput("chargesPlot")),
        column(6, plotOutput("agePlot"))
      ),
      fluidRow(
        column(6, plotOutput("smokerPlot")),
        column(6, plotOutput("bmiPlot"))
      ),
      fluidRow(
        column(12, plotOutput("childrenPlot"))
      )
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  
  # Load the original data for plotting (if needed)
  data <- read.csv("/Users/michaelwhitfield/school-work/mgmt-389/insurance-4-model.csv")
  data$sex <- as.factor(data$sex)
  data$smoker <- as.factor(data$smoker)
  data$region <- as.factor(data$region)
  
  observeEvent(input$predict, {
    # Convert risk_level from text to numeric (0,1,2)
    risk_level_num <- switch(input$risk_level,
                             "low" = 0,
                             "medium" = 1,
                             "high" = 2)
    
    # Construct new_data with all required columns, using the numeric risk_level
    new_data <- data.frame(
      age = input$age,
      bmi = input$bmi,
      children = input$children,
      smoker = input$smoker,
      region = input$region,
      sex = input$sex,
      risk_level = risk_level_num
    )
    
    # Call the imported Python function predict_insurance with all parameters.
    # Ensure your Python function's signature matches these inputs.
    prediction <- predict_insurance(input$age, input$bmi, input$children,
                                    input$smoker, input$region, input$sex, risk_level_num)
    
    output$prediction <- renderUI({
      HTML(paste("<h2><b>Predicted Health Insurance Charges: $", round(prediction, 2), "</b></h2>"))
    })
    
    # Continue with plotting code (unchanged) for demonstration
    average_charges_by_region <- data %>%
      group_by(region) %>%
      summarize(average_charges = mean(charges, na.rm = TRUE))
    
    output$chargesPlot <- renderPlot({
      ggplot(average_charges_by_region, aes(x = region, y = average_charges)) +
        geom_bar(stat = "identity", fill = "grey") +
        theme_minimal() +
        labs(title = "Average Charges by Region",
             x = "Region",
             y = "Average Charges") +
        geom_bar(data = subset(average_charges_by_region, region == input$region),
                 aes(x = region, y = average_charges),
                 fill = "red", stat = "identity") +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold"),
          axis.text = element_text(size = 12)
        )
    })
    
    average_charges_by_age <- data %>%
      group_by(age) %>%
      summarize(average_charges = mean(charges, na.rm = TRUE))
    
    output$agePlot <- renderPlot({
      ggplot(average_charges_by_age, aes(x = age, y = average_charges)) +
        geom_bar(stat = "identity", fill = "grey") +
        theme_minimal() +
        labs(title = "Average Charges by Age",
             x = "Age",
             y = "Average Charges") +
        geom_bar(data = subset(average_charges_by_age, age == input$age),
                 aes(x = age, y = average_charges),
                 fill = "red", stat = "identity") +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold"),
          axis.text = element_text(size = 12)
        )
    })
    
    average_charges_by_smoker <- data %>%
      group_by(smoker) %>%
      summarize(average_charges = mean(charges, na.rm = TRUE))
    
    output$smokerPlot <- renderPlot({
      ggplot(average_charges_by_smoker, aes(x = smoker, y = average_charges)) +
        geom_bar(stat = "identity", fill = "grey") +
        theme_classic() +
        labs(title = "Average Charges by Smoker Category",
             x = "Smoker",
             y = "Average Charges") +
        geom_bar(data = subset(average_charges_by_smoker, smoker == input$smoker),
                 aes(x = smoker, y = average_charges),
                 fill = "red", stat = "identity") +
        theme(
          plot.title = element_text(size = 20, face = "bold", color = "black"),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold"),
          axis.text = element_text(size = 12, color = "black"),
          legend.position = "none"
        ) +
        geom_text(aes(label = round(average_charges, 2)), vjust = -0.5, size = 5, color = "black") +
        annotate("text", x = 1, y = max(average_charges_by_smoker$average_charges) * 1.1,
                 label = "Smokers pay more!", color = "red", size = 6, fontface = "bold")
    })
    
    data <- data %>%
      mutate(bmi_bin = cut(bmi, breaks = seq(0, max(bmi, na.rm = TRUE), by = 5), right = FALSE))
    
    average_charges_by_bmi_bin <- data %>%
      group_by(bmi_bin) %>%
      summarize(average_charges = mean(charges, na.rm = TRUE)) %>%
      filter(!is.na(bmi_bin))
    
    output$bmiPlot <- renderPlot({
      ggplot(average_charges_by_bmi_bin, aes(x = bmi_bin, y = average_charges)) +
        geom_bar(stat = "identity", fill = "grey") +
        theme_minimal() +
        labs(title = "Average Charges by BMI Bin",
             x = "BMI Bin",
             y = "Average Charges") +
        geom_bar(data = subset(average_charges_by_bmi_bin,
                               bmi_bin == cut(input$bmi, breaks = seq(0, max(data$bmi, na.rm = TRUE), by = 5), right = FALSE)),
                 aes(x = bmi_bin, y = average_charges),
                 fill = "red", stat = "identity") +
        theme(
          plot.title = element_text(size = 20, face = "bold"),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold"),
          axis.text = element_text(size = 12)
        )
    })
    
    average_charges_by_children <- data %>%
      group_by(children) %>%
      summarize(average_charges = mean(charges, na.rm = TRUE))
    
    output$childrenPlot <- renderPlot({
      ggplot(average_charges_by_children, aes(x = as.factor(children), y = average_charges, fill = as.factor(children))) +
        geom_bar(stat = "identity", fill = "grey") +
        theme_classic() +
        labs(title = "Average Charges by Number of Children",
             x = "Number of Children",
             y = "Average Charges") +
        geom_bar(data = subset(average_charges_by_children, children == input$children),
                 aes(x = as.factor(children), y = average_charges),
                 fill = "red", stat = "identity") +
        geom_text(aes(label = round(average_charges, 2)), vjust = -0.5, size = 5,
                  color = "black", position = position_stack(vjust = 0.5)) +
        theme(
          plot.title = element_text(size = 20, face = "bold", color = "black"),
          axis.title.x = element_text(size = 15, face = "bold"),
          axis.title.y = element_text(size = 15, face = "bold"),
          axis.text = element_text(size = 12, color = "black")
        )
    })
  })
}

# Call the shiny app
shinyApp(ui = ui, server = server)