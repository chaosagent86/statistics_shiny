library(shiny)

# UI code for the app
ui <- fluidPage(
  # App title
  titlePanel("Statistik_UE - Visualisation"),
  
  # Sidebar panel --> data/variable selection
  sidebarLayout(
    # -------------- SIDEBAR PANEL --------------------------
    sidebarPanel(
      # Dropdown menu for selecting dataset
      selectInput("dataset", "Choose dataset:",
                  choices = c("swiss", "state.x77", "UN", "LakeHuron", 
                              "Titanic", "PimaIndians", "PipettingTraining", "BacterialCulture", "Salary")),
      
      # Dropdown menu for display type selection
      #am Ende Data Table wieder an den Anfang stellen
      selectInput("display", "Choose display type:",
                  choices = c("Data Table","Histogram", "Box Plot", 
                              "Q-Q Plot", "ECDF Plot", "Scatter Plot Matrix", "Scatter Plot", "Mosaic Plot")),
      
      #Dropdown Menu for Mosaic Plot
      conditionalPanel(
        condition = "input.display == 'Mosaic Plot'",
        selectInput("MosaicPlotColumns", "Choose Columns to show:\n(multiple select possible)",
                    choices = c("Class", "Sex", "Age", "Survived"),
                    multiple=TRUE,
                    selected=c("Class", "Sex", "Age", "Survived")
                    )
      ),
      
      
      
      # Conditional panel for variable selection single
      conditionalPanel(
        condition = "input.display != 'Data Table' && 
        input.display != 'Scatter Plot Matrix' && 
        input.display != 'Scatter Plot' && 
        input.display != 'Mosaic Plot' && 
        input.display != 'Residuals'",
        uiOutput("variableSelect")
      ),
      
      # Conditional panel for scatter plot variable selection
      conditionalPanel(
        condition = "input.display == 'Scatter Plot'",
        selectInput("scatter_x", "Choose X variable:", choices = NULL),
        selectInput("scatter_y", "Choose Y variable:", choices = NULL),
        checkboxInput("log_transform", "Log transform variables", value = FALSE)
      ),
      
      # Conditional panel for box plot
      conditionalPanel(
        condition = "input.display == 'Box Plot'",
        checkboxInput("allVar", "Display all variables", value = TRUE)
      ),
      
      # Conditional panel for histogram
      conditionalPanel(
        condition = "input.display == 'Histogram'",
        sliderInput("bins", "Number of bins:", min = 5, max = 30, value = 10)
      ),
      conditionalPanel(
        condition = "input.display == 'Residuals'",
        
        
      ),
    ),
    
    # -------------- MAIN PANEL --------------------------
    
    # Main panel --> output display
    mainPanel(
      
      tabsetPanel(
        # ---------- DATA TABLE PANEL ----------------------
        tabPanel('Data Table and Plots',
          conditionalPanel(
            condition = "input.display == 'Data Table'",
             DT::DTOutput("table")
             #tableOutput("table")
          ),
          conditionalPanel(
            condition = "input.display == 'Histogram'",
            plotOutput("hist")
          ),
          conditionalPanel(
            condition = "input.display == 'Box Plot'",
            plotOutput("box")
          ),
          conditionalPanel(
            condition = "input.display == 'Q-Q Plot'",
            plotOutput("qq")
          ),
          conditionalPanel(
            condition = "input.display == 'ECDF Plot'",
            plotOutput("ecdf")
          ),
          conditionalPanel(
            condition = "input.display == 'Scatter Plot Matrix'",
            plotOutput("scatter_matrix")
          ),
          conditionalPanel(
            condition = "input.display == 'Scatter Plot'",
            plotOutput("scatter")
          ),
          conditionalPanel(
            condition = "input.display == 'Mosaic Plot'",
            plotOutput("mosaic")
          ),
        ), # End of second panel
        # ---------- RESIDUALS PANEL ----------------------
        tabPanel('Residuals',
                 selectInput("residuals_x", "Choose X variable:", choices = NULL),
                 checkboxInput("res_log_transform_x", "Log transform X-variable?", value = FALSE),
                 
                 selectInput("residuals_y", "Choose Y variable:", choices = NULL),
                 checkboxInput("res_log_transform_y", "Log transform Y-variable?", value = FALSE),
                 
                 plotOutput("residuals")
        ), #End of Panel Residuals
        tabPanel('Summary',
                 verbatimTextOutput("summaryOutput")
                 )
      ) # End of Tabset Panel
    ) # End of Main Panel 
  ) # End of Sidebar Layout
) # End of Fluid Page
