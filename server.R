library(shiny)
library(brochure)
library(ggplot2)
library(tidyr)
library(dplyr)

#Server code of the app
server <- function(input, output, session) {
  
  #Error function in case of non-numeric values
  displayError <- function()
  {
    plot.new()
    text(0.5, 0.5, "Error! Non-numeric values cannot be displayed.", cex = 1.5)
  }
  
  # Returns selected dataset
  selectedData <- reactive({
    switch(input$dataset,
           "swiss" = as.data.frame(swiss),
           "state.x77" = as.data.frame(state.x77) %>% 
             rename("Life_Exp" = 4, "HS_Grad" = 6),
           "LakeHuron" = data.frame(row.names = c(1875:1972), Depth = LakeHuron),
           "Titanic" = as.data.frame(Titanic))
  })
  
  # Returns selected variable
  selectedVariable <- reactive({
    input$variable
  })
  
  # UI for variable selection
  output$variableSelect <- renderUI({
    data <- selectedData()
    if (is.data.frame(data)) {
      selectInput("variable", "Choose variable:", choices = names(data), multiple = FALSE)
    }
  })
  
  # Output the selected dataset as a table
  output$table <- renderTable({
    selectedData()
  })
  
  output$hist <- renderPlot({
    data <- selectedData()
    var <- selectedVariable()
    bins <- input$bins
    if (!is.null(var) && is.numeric(data[[var]])) {
      ggplot(data, aes_string(x = var)) +
        geom_histogram(bins = bins, fill = "blue", color = "black") +
        labs(title = paste("Histogram \n[", var, "]"), x = var, y = "Frequency")
    } else {
      displayError()
    }
  })
  
  # Output the selected dataset as a box plot using ggplot2
  output$box <- renderPlot({
    data <- selectedData()
    var <- selectedVariable()
    if (input$allVar) {
      if (all(sapply(data, is.numeric))) {
        data_long <- data %>%
          pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
        ggplot(data_long, aes(x = variable, y = value)) +
          geom_boxplot(fill = "blue", color = "black") +
          labs(title = "Box Plot \n[all variables]", x = "Variable", y = "Value")
      } else {
        displayError()
      }
    } else {
      if (!is.null(var) && is.numeric(data[[var]])) {
        ggplot(data, aes_string(y = var)) +
          geom_boxplot(fill = "blue", color = "black") +
          labs(title = paste("Box Plot \n[", var, "]"), y = var)
      } else {
        displayError()
      }
    }
  })
  
  # Output the selected dataset as a Q-Q plot using ggplot2
  output$qq <- renderPlot({
    data <- selectedData()
    var <- selectedVariable()
    if (!is.null(var) && is.numeric(data[[var]])) {
      ggplot(data, aes_string(sample = var)) +
        geom_qq() +
        geom_qq_line(color = "red") +
        labs(title = paste("Q-Q Plot \n[", var, "]"))
    } else {
      displayError()
    }
  })
  
  # Output the selected dataset as an ECDF plot using ggplot2
  output$ecdf <- renderPlot({
    data <- selectedData()
    var <- selectedVariable()
    if (!is.null(var) && is.numeric(data[[var]])) {
      ggplot(data, aes_string(x = var)) +
        stat_ecdf(geom = "step", color = "blue") +
        labs(title = paste("ECDF Plot \n[", var, "]"), x = var, y = "ECDF")
    } else {
      displayError()
    }
  })
}
