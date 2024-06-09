library(shiny)
library(brochure)
library(ggplot2)
library(tidyr)
library(dplyr)
library(MASS)
library(GGally)

#Server code of the app
server <- function(input, output, session) {
  
  #Function to generate panel
  panel.hist <- function(x, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
  }
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
  }
  
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
           "Titanic" = as.data.frame(Titanic),
           "PimaIndians" = as.data.frame(Pima.tr),
           "PipettingTraining" = data.frame(
             before = c(1.36, 1.37, 1.29, 1.22, 1.38, 1.31, 1.40, 1.39, 1.30, 1.37), 
             after = c(1.29, 1.25, 1.20, 1.26, 1.25, 1.23, 1.26, 1.31, 1.24, 1.31)),
           "BacterialCulture" = data.frame(
             grow = c(0.146842, 0.156757, 0.091255, 0.063720, 0.148471, -0.045436, 0.150407,
                      0.077905, 0.077267, 0.026454, 0.090700, 0.245384, 0.129650, 0.141617,
                      0.039957, 0.165351, 0.029091, 0.073473, 0.189657, 0.123897)),
             "Salary" = data.frame(
               company1 = c(4218.874, 2323.970, 4104.761, 3172.519, 3058.287, 2386.729,
                            4405.709, 2665.709, 5326.124, 2993.015, 5152.121, 3164.876,
                            2703.269, 3837.005, 2927.137, 2847.995, 3087.938, 3063.339,
                            4697.341, 5602.379, 2992.996, 5052.060, 4095.423, 1668.059, 6268.097),
               company2 = c(1888.252, 2429.395, 2062.037, 1932.138, 1788.335, 2119.263,
                            2185.819, 2173.098, 2391.626, 1576.546, 1871.540, 2405.640, 
                            2470.771, 1879.237, 2181.048, 2272.962, 2174.767, 1729.053,
                            1119.993, 2325.788, 2112.610, 2847.006, 1124.272, 5320.000, 4785.000))
    )
  })
  
  # Update the choices for scatter plot variable selection based on the selected dataset
  observe({
    data <- selectedData()
    updateSelectInput(session, "scatter_x", choices = names(data), selected = names(data)[1])
    updateSelectInput(session, "scatter_y", choices = names(data), selected = names(data)[2])
  })
  
  # Returns selected variable - set default to variable 1
  selectedVariable <- reactive({
    data <- selectedData()
    if (!is.null(input$variable) && length(input$variable) > 0) {
      input$variable
    } else {
      names(data)[1]
    }
  })
  
  # UI for variable selection single
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
        labs(title = paste("Histogram \n[", var, "]"), x = var, y = "Frequency") + 
        theme(axis.text = element_text(size = 15), 
              axis.ticks.length=unit(.25, "cm"),
              axis.title=element_text(size=15,face="bold"),
              plot.title = element_text(face="bold", size=20))
    } else {
      displayError()
    }
  }, height = 700, width = 700)
  
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
          labs(title = "Box Plot \n[all variables]", x = "Variable", y = "Value") + 
          theme(axis.text = element_text(size = 15), 
                axis.ticks.length=unit(.25, "cm"),
                axis.title=element_text(size=15,face="bold"),
                plot.title = element_text(face="bold", size=20))
      } else {
        displayError()
      }
    } else {
      if (!is.null(var) && is.numeric(data[[var]])) {
        ggplot(data, aes_string(y = var)) +
          geom_boxplot(fill = "blue", color = "black") +
          labs(title = paste("Box Plot \n[", var, "]"), y = var) + 
          theme(axis.text = element_text(size = 15), 
                axis.ticks.length=unit(.25, "cm"),
                axis.title=element_text(size=15,face="bold"),
                plot.title = element_text(face="bold", size=20))
      } else {
        displayError()
      }
    }
  }, height = 700, width = 700)
  
  # Output the selected dataset as a Q-Q plot using ggplot2
  output$qq <- renderPlot({
    data <- selectedData()
    var <- selectedVariable()
    if (!is.null(var) && is.numeric(data[[var]])) {
      ggplot(data, aes_string(sample = var)) +
        geom_qq() +
        geom_qq_line(color = "red") +
        labs(title = paste("Q-Q Plot \n[", var, "]")) + 
        theme(axis.text = element_text(size = 15), 
              axis.ticks.length=unit(.25, "cm"),
              axis.title=element_text(size=15,face="bold"),
              plot.title = element_text(face="bold", size=20))
    } else {
      displayError()
    }
  }, height = 700, width = 700)
  
  # Output the selected dataset as an ECDF plot using ggplot2
  output$ecdf <- renderPlot({
    data <- selectedData()
    var <- selectedVariable()
    if (!is.null(var) && is.numeric(data[[var]])) {
      ggplot(data, aes_string(x = var)) +
        stat_ecdf(geom = "step", color = "blue") +
        labs(title = paste("ECDF Plot \n[", var, "]"), x = var, y = "ECDF") + 
        theme(axis.text = element_text(size = 15), 
              axis.ticks.length=unit(.25, "cm"),
              axis.title=element_text(size=15,face="bold"),
              plot.title = element_text(face="bold", size=20))
    } else {
      displayError()
    }
  }, height = 700, width = 700)
  
  # Output the selected dataset as a Scatter Plot Matrix
  output$scatter_matrix <- renderPlot({
    data <- selectedData()
    if (all(sapply(data, is.numeric))) {
      pairs(data, lower.panel = panel.smooth, upper.panel = panel.cor, 
            diag.panel = panel.hist,las=1) 
    } else {
      displayError()
    }
  }, height = 700, width = 700)

  # Output the selected dataset as a Scatter Plot
  output$scatter <- renderPlot({
    data <- selectedData()
    x_var <- input$scatter_x
    y_var <- input$scatter_y
    if (!is.null(x_var) && !is.null(y_var) && is.numeric(data[[x_var]]) && is.numeric(data[[y_var]])) {
      if (input$log_transform) {
        data <- data %>%
          mutate(across(c(x_var, y_var), log, .names = "log_{col}"))
        ggplot(data, aes_string(x = paste0("log_", x_var), y = paste0("log_", y_var))) +
          geom_point() +
          geom_smooth(method = "lm", col = "red") +
          labs(title = paste("Scatter Plot \n[Log(", x_var, ") vs Log(", y_var, ")]"), 
               x = paste("Log(", x_var, ")"), y = paste("Log(", y_var, ")")) + 
          theme(axis.text = element_text(size = 15), 
                axis.ticks.length=unit(.25, "cm"),
                axis.title=element_text(size=15,face="bold"),
                plot.title = element_text(face="bold", size=20))
      } else {
        ggplot(data, aes_string(x = x_var, y = y_var)) +
          geom_point() +
          geom_smooth(method = "lm", col = "red") +
          labs(title = paste("Scatter Plot \n[", x_var, " vs ", y_var, "]"), x = x_var, y = y_var) + 
          theme(axis.text = element_text(size = 15), 
                axis.ticks.length=unit(.25, "cm"),
                axis.title=element_text(size=15,face="bold"),
                plot.title = element_text(face="bold", size=20))
      }
    } else {
      displayError()
    }
  }, height = 700, width = 700)
}
