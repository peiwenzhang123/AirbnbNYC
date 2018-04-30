# Libraries
library(data.table)
library(dplyr)
library(DT)
library(geojsonio)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(plotly)
library(shiny)
library(shinythemes)
library(sp)
library(stringr)

# Data importing and light wrangling
airbnb_data <- fread("./stat041s18_airbnb/merged_airbnb.csv") %>%
  arrange(neighbourhood_cleansed)

nyc_names<- fread("./stat041s18_airbnb/Data/neighbourhoods.csv")
nyc_neighbour <- geojsonio::geojson_read("./stat041s18_airbnb/Data/neighbourhoods.geojson", what = "sp")

merged_plot2 <- fread("./stat041s18_airbnb/merged_plot2.csv", stringsAsFactors = FALSE,
                      colClasses = c("character", "character", "character", "integer", "character", "character", "character", "integer")) %>% 
  select(-c(listing_id, V1))

# Grouping neighbourhoods by boroughs
Manhattan_nh <- merged_plot2 %>%
  select(neighbourhood_cleansed, neighbourhood_group_cleansed) %>%
  filter(neighbourhood_group_cleansed == "Manhattan")
Manhattan_nh <- as.list(sort(unique(Manhattan_nh$neighbourhood_cleansed)))

Bronx_nh <- merged_plot2 %>%
  select(neighbourhood_cleansed, neighbourhood_group_cleansed) %>%
  filter(neighbourhood_group_cleansed == "Bronx")
Bronx_nh <- as.list(sort(unique(Bronx_nh$neighbourhood_cleansed)))

StatenIsland_nh <- merged_plot2 %>%
  select(neighbourhood_cleansed, neighbourhood_group_cleansed) %>%
  filter(neighbourhood_group_cleansed == "Staten Island")
StatenIsland_nh <- as.list(sort(unique(StatenIsland_nh$neighbourhood_cleansed)))

Queens_nh <- merged_plot2 %>%
  select(neighbourhood_cleansed, neighbourhood_group_cleansed) %>%
  filter(neighbourhood_group_cleansed == "Queens")
Queens_nh <- as.list(sort(unique(Queens_nh$neighbourhood_cleansed)))

Brooklyn_nh <- merged_plot2 %>%
  select(neighbourhood_cleansed, neighbourhood_group_cleansed) %>%
  filter(neighbourhood_group_cleansed == "Brooklyn")
Brooklyn_nh <- as.list(sort(unique(Brooklyn_nh$neighbourhood_cleansed)))

# Main app
shinyApp(
  
  # UI side
  ui = tagList(
    navbarPage(
      theme = shinytheme("cosmo"),
      title = "Airbnb in NYC",
      tabPanel("About", 
               sidebarLayout(
                 sidebarPanel(
                   p("As both of us (Peiwen and Istvan) are moving to New York City next year and are uncertain about which area to live in, 
                      we decided to use this project as an opportunity to explore the differences in NYC neigbourhoods. 
                      We were able to obtain structured data from Airbnb, which we understand is priced differently 
                      from long-term rentals. Nevertheless, to inform our decisions, we used the data in relative terms by comparing 
                      prices of different types of listings across neighbourhoods."),
                   br(),
                   p("Our data comprises of more than 17 million observations: the nightly rates and availability for all 
                      48852 Airbnb listings across the five boroughs of NYC, for the next 365 days (starting from 
                      March 4th 2018). Before getting into the nitty-gritty, on this tab we present an overview of the 
                      NYC Airbnb market. You can see how the listings vary by borough and accommodation type, followed by
                      a general picture of the interquartile range of prices, also listed by borough."),
                   br(),
                   p("After familiarising yourself with the overall market, in the 'Availability Map' section you 
                      can use our interactive map to find out how many Airbnb listings are available in a given future time frame and 
                      in a given price range. Under 'Price Fluctuation Comparison', you will find a detailed, neighbourhood level 
                      comparison of how prices vary over the upcoming year, and how prices of individual neighbourhoods compare to the boroughs' average."),
                   br(),
                   p("We hope that you will enjoy using this tool as much we did making it, and that it provides you with some useful 
                      insights into the housing market in New York City. Come visit us in the Big Apple!")
                  ),
                 
                 mainPanel(
                   tabsetPanel(type = "tabs",
                               tabPanel("Introductory Data", plotOutput("plot1"), plotOutput("plot2")),
                               tabPanel("Data Source",
                                        br(),
                                        p("We downloaded all our wonderful data from ",
                                          shiny::a("Inside Airbnb", href = "http://insideairbnb.com/index.html"),
                                          ", 'an independent, non-commercial set of tools and data that allows you to explore 
                                          how Airbnb is really being used in cities around the world'. Kudos to the team for 
                                          making its work public and adding data to the debate on the sharing economy."),
                                        br(),
                                        p("After downloading the data, we wrangled it slightly to fit our purpose, all of 
                                          which can be reproduced with the ", 
                                          shiny::a("following code", href = "https://github.com/icsel/AirbnbNewYork/blob/master/InitialWrangling.R"),
                                          " .")
                                          )
                   )
                 )
              )
           ),
      
      tabPanel("Availability Map",
               sidebarPanel(
                 helpText(tags$div("Pick a price range and a future time frame (the next 30/ 60/ 90/ 365 days) that you are interested in.",
                          "Then, hover over the map to see the number of days an average listing is available for in that time frame and the number of listings consulted.", 
                          tags$br(), 
                          tags$br(), 
                          "Please note that ", strong("the fill gradient is based on the number of days available "), "- if a lot of the map's area seems blank, try decreasing the price range!"
                    )
                 ),
                 
                 # Allow user to select a price range
                 sliderInput(inputId = "range", 
                             label = h4("Price Range: "),
                             min = min(airbnb_data$price, na.rm=T), 
                             max = max(airbnb_data$price, na.rm=T),
                             value = c(100, 600)),
                 
                 # Allow user to select an upcoming period
                 selectInput(inputId = "date",
                             label = h4("Availability in the next: "), 
                             choices = list("30 days" = "availability_30", 
                                            "60 days" = "availability_60", 
                                            "90 days" = "availability_90", 
                                            "365 days" = "availability_365"), 
                             selected = "30 days")
               ),
               
               mainPanel(
                 # Output map
                 leafletOutput("mymap")
               )
      ),
      
      tabPanel("Price Fluctuation Comparison",
               sidebarPanel(
                 helpText(tags$div("With this widget you can explore how, in a chosen neighbourhood(s), the nightly prices of listings vary over the next year.",
                                   "The data was pulled on 4 March 2018, so it is accurate as of then.",
                                   tags$br(),
                                   tags$br(),
                                   "Only neighbourhoods with at least 1 listing are included, organized by borough, and within each borough arranged alphabetically.", 
                                   "After selecting a neighbourhood(s), the app will draw a graph that compares the chosen neighbourhood to the borough where it is located.",
                                   "It is possible to select neighbourhoods from multiple boroughs, but be wary, the graph gets 'messy' quickly!"
                                   )
                          ),

                 # Allow user to select location
                 selectInput(
                   inputId = "location",
                   label = h4("Neighbourhood(s) to compare:"),
                   choices = list("Bronx" = Bronx_nh, "Brooklyn" = Brooklyn_nh, "Manhattan" = Manhattan_nh, "Queens" = Queens_nh, "Staten Island" = StatenIsland_nh), 
                   selected = NULL,
                   multiple = TRUE,
                   selectize = TRUE),
                 
                 # Allow user to select number of people
                 sliderInput(
                   inputId = "accommodate", 
                   label = h4("Accomodation for how many people:"),
                   min = min(merged_plot2$accommodates), max = max(merged_plot2$accommodates), #select this number
                   value = c(1, 2),
                   round = TRUE,
                   step = 1),
                 
                 # Allow user to select type of listing
                 checkboxGroupInput(
                   inputId = "type",
                   label = h4("Type of accommodation:"),
                   choices = as.list(unique(merged_plot2$room_type)),
                   selected = "Private room")
                 ),
               
               mainPanel(
                 # Output graph
                 plotlyOutput(
                   outputId = "calendar",
                   height = "650px"))
      )
    )
  ),
  
  # Server side
  server = function(input, output){

    # Create graphs for tab 1
    
    # First static graph
    palette1 <- c("#5BC8AC", "#E6D72A", "#F18D9E")
    
    output$plot1 <- renderPlot({
      ggplot(airbnb_data, aes(x = neighbourhood_group_cleansed)) + 
        geom_bar(aes(fill = room_type)) +
        labs(title = "Listing Type by Borough", x = "Borough", y = "Number of Listings") +
        scale_fill_manual(values = palette1, name = "Listing Type") +
        theme_light()
      })
    
    # Second static graph
    palette2 <- c("#ffa8f0", "#E6D72A", "#8accb3", "#f66767", "#8c4df9")
    
    output$plot2 <- renderPlot({
    ggplot(airbnb_data, aes(x = neighbourhood_group_cleansed , y = price)) + 
      geom_boxplot(col = palette2) +
      coord_flip() +
      labs(title = "Average Nightly Rent by Borough", x = "Borough", y = "Average Nightly Rent ($)") +
      theme_light()
    })

    # Data wrangling for tab 2
      # Create reactive dataset
      airbnb_new <- reactive({
        airbnb_data %>%
          filter(price >= input$range[1], 
                 price <= input$range[2]) %>%
          group_by(neighbourhood_cleansed) %>%
          summarise(room = n(), average = round(mean(get(input$date))))
      })
      
      # Join with full neighbourhoods list to eliminate NA's
      joined <- reactive({
        left_join(nyc_names, airbnb_new(), by = c("neighbourhood" = "neighbourhood_cleansed")) %>%
        arrange(neighbourhood) %>%
        mutate(room = ifelse(is.na(room), 0, room)) %>%
        mutate(average = ifelse(is.na(average), 0, average))
      })
        
      # Merge dataset with geojson map info
      merged_map <- reactive({merge(nyc_neighbour, 
                                    as.data.frame(joined()), 
                                    by = c("neighbourhood"), 
                                    duplicateGeoms = TRUE)
      })
      
      # Create interactive map for tab 2
      pal <- colorNumeric("YlGnBu", domain = NULL)
      
      output$mymap <- renderLeaflet({
        leaflet(merged_map()) %>%
          setView(-74, 40.7, 10) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(fillColor = ~pal(merged_map()$average),
                      weight = 2,
                      opacity = 1,
                      color = "white",
                      dashArray = "1",
                      smoothFactor = 0.3,
                      fillOpacity = 2, 
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#FC94AF",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE), 
                      label = ~paste(neighbourhood, ": ", room, " listing(s) available, on ", average, " day(s) on average"),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", 
                                     padding = "2px 5px"),
                        textsize = "15px",
                        direction = "bottom")) %>%
          addLegend(pal = pal, values = ~airbnb_new()$average, opacity = 0.7, 
                    title = NULL, position = "topleft")
      })
    
    # Create graph for tab 3
    output$calendar <- renderPlotly({
      
      # Require some inputs
      req(input$location)
      req(input$type)
      
      # Filter for relevant neighbourhoods
      plot_calendar <- merged_plot2 %>%
        filter(neighbourhood_cleansed %in% input$location) %>%
        filter(accommodates >= input$accommodate[1],
               accommodates <= input$accommodate[2]) %>%
        filter(room_type %in% input$type)
      
      # Summarise neighbourhood statistics
      plot_neighbourhoods <- plot_calendar %>%
        group_by(date, neighbourhood_cleansed)  %>% 
        filter(!is.na(price)) %>%
        summarise(price = mean(price, na.rm = TRUE), count = n())
      
      # Find relevant boroughs
      plot_boroughs_used <- as.list(unique(plot_calendar$neighbourhood_group_cleansed))
      
      # Validate dataset is not empty, if so, notify user
      shiny::validate(need(length(plot_boroughs_used) > 0, "No listing matches the selected criteria. Please change your input."))
      
      # Summarize borough statistic
      plot_boroughs <- merged_plot2 %>%
        filter(neighbourhood_group_cleansed %in% plot_boroughs_used) %>%
        filter(accommodates >= input$accommodate[1],
               accommodates <= input$accommodate[2]) %>%
        filter(room_type %in% input$type) %>% 
        group_by(date, neighbourhood_group_cleansed) %>%
        filter(!is.na(price)) %>%
        summarise(price = mean(price, na.rm = TRUE), count = n())
      
      # Plot both borough and neighbourhood on a graph
      calendar_normal <- ggplot(data = plot_neighbourhoods, aes(x = as.POSIXct(date), y = price, group = 1, col = neighbourhood_cleansed, 
                                                                text = paste('Rent ($):', as.integer(price), '<br>Date: ', as.Date(date), '<br>Listings Available: ', count))) +
        geom_point(alpha = 0.5, size = 1) +
        geom_path(lineend = "butt", linejoin = "round", linemitre = 1, alpha = 0.8,
                  data = plot_boroughs, aes(col = neighbourhood_group_cleansed)) +
        ggtitle("Comparison of Airbnb Prices in New York City Neighbourhoods") +
        labs(color = "Neighbourhood", x = "Date", y = "Average Nightly Rate ($)") +
        theme_minimal() +
        scale_colour_brewer(palette = "Dark2")
      
      # Validate graph has data in it
      shiny::validate(need(nrow(calendar_normal$data) > 0, "No listing matches the selected criteria. Please change your input."))
      
      # Convert ggplot2 into plotly
      graph_plotly <- ggplotly(calendar_normal, tooltip = c("text"))
      style(graph_plotly) %>% 
        layout(legend = list(orientation = "h", y = -0.1, x = 0.5, xanchor = 'center'))
      })
    }
  )
