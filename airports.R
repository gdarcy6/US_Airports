library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)

# Read airport data
data <- read.csv("airports.csv")

# Create an INFO column
data <- data %>% mutate(INFO = paste0(AIRPORT, " | ", CITY, ", ", STATE))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = div(icon("plane"), " US Airport Map"), titleWidth = 280),
  dashboardSidebar(
    selectizeInput("inputState", "Select state:", multiple = TRUE, choices = c("Select All", sort(unique(data$STATE))), options = list(placeholder = "Select states")),
    actionButton("clearSelection", "Clear Selection"),
    br(),
    tags$span("Total Airports Selected: ", textOutput("totalAirports"))
  ),
  dashboardBody(
    leafletOutput("mapOutput"),
    tableOutput("stateTable"),  # Table to display selected states and airport counts
    tags$script(
      HTML(
        '
        $(document).on("shiny:connected", function() {
          $("#clearSelection").click(function() {
            var inputState = $("#inputState")[0].selectize;
            inputState.clear();
          });
        });
        '
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    if ("Select All" %in% input$inputState) {
      data
    } else if (length(input$inputState) > 0) {
      data[data$STATE %in% input$inputState, ]
    } else {
      data[0,]  # Empty data frame when no states are selected
    }
  })
  
  # Create a data frame with selected states and airport counts
  selected_states <- reactive({
    if ("Select All" %in% input$inputState) {
      data %>%
        group_by(STATE) %>%
        summarize(No.OfAirports = n())
    } else if (!is.null(input$inputState)) {
      data %>%
        filter(STATE %in% input$inputState) %>%
        group_by(STATE) %>%
        summarize(No.OfAirports = n())
    } else {
      data.frame()  # Empty data frame when no states are selected
    }
  })
  
  output$mapOutput <- renderLeaflet({
    usaLat <- 36.5588659
    usaLon <- -107.6660877
    usaZoom <- 3
    
    airportIcon <- makeIcon(
      iconUrl = "airplane.png",
      iconWidth = 16, iconHeight = 16
    )
    
    map <- leaflet() %>%
      setView(lat = usaLat, lng = usaLon, zoom = usaZoom) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
    
    if (nrow(filtered_data()) > 0) {
      map <- map %>%
        addMarkers(
          ~LONGITUDE, ~LATITUDE,
          data = filtered_data(),
          icon = airportIcon,
          label = ~INFO,
          popup = ~INFO
        )
    }
    
    map
  })
  
  
  
  output$stateTable <- renderTable({
    selected_states()
  })
  
  # Update the total airports count dynamically
  output$totalAirports <- renderText({
    selected_data <- filtered_data()
    if (nrow(selected_data) > 0) {
      paste(nrow(selected_data), "airports selected")
    } else {
      "No airports selected"
    }
  })
}

shinyApp(ui = ui, server = server)


