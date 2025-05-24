library(dplyr)
library(htmltools)
library(leaflet)
library(maps)
library(sf)
library(shiny)
library(munsell)

# The code here prevents messages from popping up in the Shiny app
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(leaflet)))
suppressWarnings(suppressMessages(library(maps)))
suppressWarnings(suppressMessages(library(sf)))

# Note 1: make variable names in format: first word lowercase, other words uppercase
# Example: firstVariable, secondVariable, yetAnotherVariable

# Note 2: This section of code modifies the data

dataFile <- read.csv("UpdatedOCSData3-7.csv")

server <- function(input, output) {
  
  activePrograms <- read.csv("OCS24-25ActivePrograms.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # Fix Grinnell in London's name
  activePrograms$Program.Name <- ifelse(activePrograms$Program.Name == " *Grinnell-in-London", "Grinnell in London", activePrograms$Program.Name)
  
  # Reading in the country information
  worldMap <- read_sf("ne_50m_admin_0_countries.shp")
  
  # Create a dataframe of countries and the number of students who have visited them,
  # but only for active programs
  countryCounts <- dataFile %>%
    filter(ProgramName %in% activePrograms$Program.Name) %>%
    group_by(CountryName)
  
  # Changing country names to match the names in worldMap:
  countryCounts$CountryName[countryCounts$CountryName == "Czech Republic"] <- "Czechia"
  countryCounts$CountryName[countryCounts$CountryName == "French Polynesia"] <- "Fr. Polynesia"
  countryCounts$CountryName[countryCounts$CountryName == "Turks and Caicos Islands"] <- "Turks and Caicos Is."
  countryCounts$CountryName[countryCounts$CountryName == "U.S. Virgin Islands"] <- "U.S. Virgin Is."
  countryCounts$CountryName[countryCounts$CountryName == "United States"] <- "United States of America"
  
  # Need to change NAME to CountryName so that the geographical data matches the dataset
  colnames(worldMap)[colnames(worldMap) == "NAME"] <- "CountryName"
  
  # Creating the map
  output$map <- renderLeaflet({
    
    
    # Allows for the map to be filtered by Academic Year
    filteredCounts <- subset(countryCounts, StartYear >= input$yearRange[1] & StartYear <= input$yearRange[2])
    
    # Gets the counts of trips per country
    filteredCounts <- filteredCounts %>% summarise(TripCount = n(), ProgramNames = list(ProgramName))
    
    # Putting the TripCounts into worldMap. 
    
    worldMap <- merge(worldMap, filteredCounts, by = 'CountryName', all.x = TRUE)
    
    # Set all other countries to 0 trips
    worldMap$TripCount <- replace(worldMap$TripCount, is.na(worldMap$TripCount), 0)
    
    
    # Creating a color palette for the map
    paletteNum <- colorNumeric("Reds", domain = worldMap$TripCount)
    
    # When a country is clicked on, makes a popup box that shows its name,
    # how many visits it's had, and links to its active programs.
    popupText <- lapply(seq(nrow(worldMap)), function(i) {
      uniquePrograms <- unique(worldMap$ProgramNames[[i]])
      programLinks <- paste("<a href='", activePrograms$TD.link[activePrograms$Program.Name %in% uniquePrograms], "'>", uniquePrograms, "</a>", collapse = "<br/>")
      paste(
        "Country: ", worldMap$CountryName[i], "<br/>",
        "Visits: ", worldMap$TripCount[i], "<br/>",
        "Programs: ", paste(programLinks, collapse = "<br/>"),
        sep = ""
      )
    }) %>%
      lapply(htmltools::HTML)
    
    
    
    # Sets how far you can zoom so you don't go past the borders
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      # Sets the map
      addProviderTiles("CartoDB.Voyager") %>%
      # Sets the starting viewpoint
      setView(lng = 0, lat = 30, zoom = 2) %>%
      # Sets the limits of scrolling
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      # Adds the actual chloropleth map for each country based on how many visits
      addPolygons(data = worldMap, fillColor = ~paletteNum(worldMap$TripCount), weight = 1,
                  color = 'black', smoothFactor = .3, fillOpacity = .75,
                  popup = popupText) %>%
      # Adds a legend
      addLegend(pal = paletteNum, values = worldMap$TripCount,
                title = '<small>Number of students<br>who have visited</small>',
                position = 'bottomleft')
  })
  
  
}

# UI
ui <- fluidPage(
  titlePanel("Student Travel Map"),
  fluidRow(
    column(width = 12,
           leafletOutput("map", height = "500px", width = "1000px")
    )
  ),
  fluidRow(
    column(width = 12,
           sliderInput("yearRange", "Select Year Range:",
                       min = min(dataFile$StartYear), max = max(dataFile$StartYear),
                       value = c(min(dataFile$StartYear), max(dataFile$StartYear)), step = 1)
    )
  )
)

# Note: if you get a scrollbar in the middle of the page, increase the height or width
shinyApp(ui = ui, server = server, options = list(width = 1000, height = 800))