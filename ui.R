library(shiny)

# UI code for the app
ui <- fluidPage(
  # App title
  titlePanel("Statistik #2 - Explorative Datenanalse"),
  
  # Sidebar panel --> data/variable selection
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for selecting dataset
      selectInput("dataset", "Choose dataset:",
                  choices = c("swiss", "state.x77", "LakeHuron", "Titanic")),
      
      # Dropdown menu for display type selection
      selectInput("display", "Choose display type:",
                  choices = c("Data Table", "Histogram", "Box Plot", "Q-Q Plot", "ECDF Plot")),
      
      # Conditional panel for variable selection
      conditionalPanel(
        condition = "input.display != 'Data Table'",
        uiOutput("variableSelect")
      ),
      
      # Conditional panel for box plot
      conditionalPanel(
        condition = "input.display == 'Box Plot'",
        checkboxInput("allVar", "Display all variables", value = TRUE)
      ),
      
      # Conditional panel for histogram
      conditionalPanel(
        condition = "input.display == 'Histogram'",
        sliderInput("bins", "Number of bins:", min = 5, max = 20, value = 10)
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
      )
    )
  )
)
