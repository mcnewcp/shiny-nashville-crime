### this app displays Nashville crime data
###
###
###             Created by:         Coy McNew               2020-03-07
###             Last edited by:     Coy McNew               2020-03-07

library(shiny)
library(shinyWidgets)
library(leaflet)
library(lubridate)
library(dplyr)

#load data
data_list <- readRDS('data.RDS')
mapDF <- data_list$mapDF
blankDF <- data_list$blankDF
rm(data_list)

###static settings
#dates
min_date <- min(mapDF$incident_occurred, na.rm = TRUE)
max_date <- max(mapDF$incident_occurred, na.rm = TRUE)
default_range <- c(
    max_date - days(2),
    max_date
)

ui <- bootstrapPage(
    title = "Nashville_Crime",
    tags$style(
        type = "text/css", "html, body {width:100%;height:100%}"
    ),
    
    leafletOutput("cluster_map", width = "100%", height = "100%"),
    absolutePanel(
        top = 10, left = 75,
        tags$h2("Nashville Crime Map", style = "color: #989898;"),
        tags$h4("Make selections at right. Zoom, click, and hover for more information", style = "color: #989898;"),
        tags$a("Data Source", href="https://data.nashville.gov"),
        tags$br(),
        actionButton(inputId = "refresh", label = "Refresh Data")
    ),
    
    absolutePanel(
        draggable = TRUE, top = 10, right = 10,
        width = 330, height = "auto",
        dateRangeInput(
            "date", "Date Range Selection:",
            start = default_range[1], end = default_range[2],
            min = min_date, max = Sys.Date()
        ),
        pickerInput(
            "incident_status", "Incident Status:",
            choices = levels(mapDF$incident_status), multiple = TRUE
        ),
        sliderInput(
            "victim_number", "Number of Victims:",
            min = min(mapDF$victim_number, na.rm = TRUE), max = max(mapDF$victim_number, na.rm = TRUE),
            value = c(min(mapDF$victim_number, na.rm = TRUE), max = max(mapDF$victim_number, na.rm = TRUE))
        ),
        selectizeInput(
            "offense_description", "Offense Description:",
            choices = levels(mapDF$offense_description), multiple = TRUE
        ),
        pickerInput(
            "location_description", "Location Description:",
            choices = levels(mapDF$location_description), multiple = TRUE
        )              
    )
)

server <- function(input, output) {
    #generate static map portion
    output$cluster_map <- renderLeaflet({

        # draw map static portion
        leaflet(mapDF) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            #set view to center of nashville and set zoom level
            setView(-86.725, 36.175, 12)
            
        
    })
    
    #filter df based on inputs
    filterDF <- reactive({
        mapDF %>%
            #date range
            filter(
                incident_occurred >= input$date[1] & incident_occurred < (input$date[2] + days(1))
            ) %>%
            #filter data
            {if (length(input$offense_description) > 0) filter(., offense_description %in% input$offense_description) else .} %>%
            {if (length(input$location_description) > 0) filter(., location_description %in% input$location_description) else .} %>%
            {if (length(input$incident_status) > 0) filter(., incident_status %in% input$incident_status) else .} %>%
            filter(victim_number >= input$victim_number[1] & victim_number <= input$victim_number[2]) %>%
            #generate days_ago column
            mutate(days_ago = difftime(
                Sys.Date(), incident_occurred, units ="days") %>% as.numeric()
            ) %>%
            #generate date column
            mutate(date = date(incident_occurred))

        
    })
    
    #refresh data from API
    refreshDF <- eventReactive(input$refresh, {
        read.socrata("https://data.nashville.gov/resource/sie3-y9k4.csv", app_token = read_tsv(app_token.txt))
    })
    
    #map color function
    map_col <- reactive({
        colorNumeric("viridis", domain = c(0, max(filterDF()$days_ago, na.rm = TRUE)))
    })
    
    #change points on filtered data
    observe({
        leafletProxy("cluster_map", data = filterDF()) %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            removeImage(layerId = "legend") %>%
            
            addCircleMarkers(
                lng = ~longitude, lat = ~latitude,
                clusterOptions = markerClusterOptions(),
                color = ~map_col()(days_ago),
                label = ~lapply(paste0(
                    "Date of Occurrence: ",      incident_occurred,
                    "<br>Address: ",             address,
                    "<br>Offense Description: ", offense_description, 
                    "<br>Weapon Description: ",  weapon_description,
                    "<br>Number of Victims: ",   victim_number,
                    "<br>Victim Description: ",  victim_description,
                    "<br>Victim Gender: ",       victim_gender,
                    "<br>Victim Race: ",         victim_race,
                    "<br>Victim Ethnicity: ",    victim_ethnicity
                ), HTML)
            ) %>%
            addLegend(
                "bottomright", pal = map_col(), values = ~days_ago,
                layerId = "legend",
                title = "How many days ago did the<br> crime occur?"
            )
    })
    # output$debug <- renderPrint({
    #     # input$date[1]
    #     # input$offense_group
    #     # input$investigation_status
    #     # is.null(input$victim_description)
    #     # unique(filterDF()$victim_description)
    #     glimpse(filterDF())
    #     
    #     
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
