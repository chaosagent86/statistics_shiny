library(shiny)

# UI code for the app
ui <- fluidPage(
  # App title
  titlePanel("Statistik_UE - Visualisation"),
  
  # Sidebar panel --> data/variable selection
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for selecting dataset
      selectInput("dataset", "Choose dataset:",
                  choices = c("Titanic", "state.x77", "UN", "LakeHuron", "swiss", "PimaIndians", "PipettingTraining", "BacterialCulture", "Salary")),
      
      # Dropdown menu for display type selection
      #am Ende Data Table wieder an den Anfang stellen
      selectInput("display", "Choose display type:",
                  choices = c("Mosaic Plot", "Histogram", "Box Plot", "Q-Q Plot", "ECDF Plot", "Scatter Plot Matrix", "Scatter Plot", "Data Table")),
      
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
        condition = "input.display != 'Data Table' && input.display != 'Scatter Plot Matrix' && input.display != 'Scatter Plot' && input.display != 'Mosaic Plot'",
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
      )
    ),
    
    # Main panel --> output display
    mainPanel(
      conditionalPanel(
        condition = "input.display == 'Data Table'",
        tableOutput("table")
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
      )
    )
  )
)
