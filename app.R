library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(shinydashboard)
library(lubridate)


# Database connection
path_to_db <- "C:/DBs/chinook.db"  
db <- dbConnect(RSQLite::SQLite(), path_to_db)

genres <- dbGetQuery(db, "SELECT GenreId, Name FROM genres")
media_types <- dbGetQuery(db, "SELECT MediaTypeId, Name FROM media_types")
tracks <- dbGetQuery(db, "SELECT * FROM tracks")
customers <- dbGetQuery(db, "SELECT * FROM customers")
invoices <- dbGetQuery(db, "SELECT * FROM invoices")

tracks <- tracks %>%
  mutate(Seconds = Milliseconds / 1000)

slider_max <- 1700

# Shiny app
shinyApp(
  ui = fluidPage(
    titlePanel("Chinook Database Dashboard"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "genreInput", 
          "Select Genre:", 
          choices = genres$Name, 
          selected = genres$Name[1], 
          multiple = TRUE
        ),
        checkboxGroupInput(
          "mediaTypeInput", 
          "Select Media Type:", 
          choices = media_types$Name, 
          selected = media_types$Name[1]
        ),
        sliderInput(
          "lengthInput", 
          "Track Length (Seconds):", 
          min = 0, 
          max = slider_max, 
          value = c(0, slider_max)
        ),
        numericInput("clusters", "Number of Clusters:", value = 3, min = 2, max = 10),
        actionButton("showAll", "Show All Genres and Media Types", class = "btn-primary")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Overview",
                   fluidRow(
                     column(6, valueBoxOutput("totalRevenue")),
                     column(6, valueBoxOutput("totalCountries"))
                   ),
                   fluidRow(
                     column(6, valueBoxOutput("totalCustomers")),
                     column(6, valueBoxOutput("mostSoldGenre"))
                   ),
                   fluidRow(
                     column(12, tableOutput("topAlbums"))
                   )
          ),
          tabPanel("Dashboard",
                   fluidRow(
                     column(8, plotOutput("barChart")),
                     column(4, leafletOutput("mapPlot"))
                   ),
                   fluidRow(
                     column(12, dataTableOutput("dataTable"))
                   )),
          tabPanel("Clustering", plotOutput("clusterPlot"), dataTableOutput("clusterTable"))
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    filteredData <- reactive({
      if (input$showAll %% 2 == 1) {
        return(tracks)
      } else {
        tracks %>%
          filter(
            GenreId %in% genres$GenreId[genres$Name %in% input$genreInput],
            MediaTypeId %in% media_types$MediaTypeId[media_types$Name %in% input$mediaTypeInput],
            Seconds >= input$lengthInput[1],
            Seconds <= input$lengthInput[2]
          )
      }
    })
    
    output$totalRevenue <- renderValueBox({
      total_revenue <- sum(invoices$Total, na.rm = TRUE)
      valueBox(formatC(total_revenue, format = "f", digits = 2), "Total Revenue", icon = icon("dollar"), color = "green")
    })
    
    output$totalCountries <- renderValueBox({
      total_countries <- customers %>% select(Country) %>% distinct() %>% nrow()
      valueBox(total_countries, "Countries Selling To", icon = icon("globe"), color = "blue")
    })
    
    output$totalCustomers <- renderValueBox({
      total_customers <- nrow(customers)
      valueBox(total_customers, "Total Customers", icon = icon("users"), color = "purple")
    })
    
    output$mostSoldGenre <- renderValueBox({
      most_sold <- tracks %>%
        inner_join(invoices, by = c("TrackId" = "InvoiceId")) %>%
        group_by(GenreId) %>%
        summarise(TotalSales = sum(Total), .groups = "drop") %>%
        arrange(desc(TotalSales)) %>%
        slice(1) %>%
        inner_join(genres, by = c("GenreId" = "GenreId"))
      valueBox(most_sold$Name, "Most Sold Genre", icon = icon("music"), color = "red")
    })
    
    output$topAlbums <- renderTable({
      top_albums <- tracks %>%
        inner_join(invoices, by = c("TrackId" = "InvoiceId")) %>%
        group_by(AlbumId) %>%
        summarise(TotalSales = sum(Total), .groups = "drop") %>%
        arrange(desc(TotalSales)) %>%
        slice(1:5)
      top_albums %>%
        inner_join(dbGetQuery(db, "SELECT AlbumId, Title FROM albums"), by = "AlbumId") %>%
        select(Title, TotalSales)
    })
    
    output$barChart <- renderPlot({
      data <- filteredData() %>%
        inner_join(invoices, by = c("TrackId" = "InvoiceId")) %>%
        inner_join(customers, by = "CustomerId") %>%
        group_by(Country, GenreId) %>%
        summarise(TrackCount = n(), .groups = "drop") %>%
        inner_join(genres, by = c("GenreId" = "GenreId"))
      
      ggplot(data, aes(x = Country, y = TrackCount, fill = Name)) +
        geom_bar(stat = "identity") +
        labs(title = "Tracks by Country and Genre", 
             x = "Country", 
             y = "Number of Tracks", 
             fill = "Genre") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    })
    
    output$mapPlot <- renderLeaflet({
      # Apply the filters
      filtered_tracks <- filteredData() %>%
        inner_join(invoices, by = c("TrackId" = "InvoiceId")) %>%
        inner_join(customers, by = "CustomerId") %>%
        group_by(Country) %>%
        summarise(TotalRevenue = sum(Total), .groups = "drop")
      
      # Coordinates for countries
      country_coords <- data.frame(
        Country = c("USA", "Canada", "Brazil", "Germany", "France", "India", "Australia", "Japan", "Poland", "Czech Republic"),
        lat = c(37.0902, 56.1304, -14.2350, 51.1657, 46.6034, 20.5937, -25.2744, 36.2048, 50.2599, 49.8175),
        lon = c(-95.7129, -106.3468, -51.9253, 10.4515, 2.2137, 78.9629, 133.7751, 138.2529, 19.0216, 15.4730)
      )
      
      revenue_with_coords <- filtered_tracks %>%
        inner_join(country_coords, by = "Country")
      
      # Check if there is data
      if (nrow(revenue_with_coords) == 0) {
        return(leaflet() %>% addTiles() %>% addPopups(lng = 0, lat = 0, popup = "No data available"))
      }
      
      # Set the size of the circles based on profit
      max_revenue <- max(revenue_with_coords$TotalRevenue, na.rm = TRUE)
      revenue_with_coords <- revenue_with_coords %>%
        mutate(BubbleSize = (TotalRevenue / max_revenue) * 50)
      
      # Draw the map
      leaflet(revenue_with_coords) %>%
        addTiles() %>%
        addCircleMarkers(
          ~lon, ~lat,
          radius = ~BubbleSize,
          popup = ~paste("Country:", Country, "<br>Total Revenue:", round(TotalRevenue, 2)),
          color = "blue",
          fillOpacity = 0.7
        )
    })
    
    
    output$dataTable <- renderDataTable({
      datatable(filteredData(), options = list(pageLength = 10))
    })
    
    
    output$clusterPlot <- renderPlot({
      withProgress(message = "Performing clustering...", {
        revenue_by_country <- filteredData() %>%
          inner_join(invoices, by = c("TrackId" = "InvoiceId")) %>%
          inner_join(customers, by = "CustomerId") %>%
          group_by(Country) %>%
          summarise(TotalRevenue = sum(Total), .groups = "drop") %>%
          na.omit()
        
        if (nrow(revenue_by_country) > 1) {
          kmeans_result <- kmeans(revenue_by_country$TotalRevenue, centers = input$clusters)
          revenue_by_country$Cluster <- as.factor(kmeans_result$cluster)
          
          ggplot(revenue_by_country, aes(x = TotalRevenue, y = Country, color = Cluster)) +
            geom_point(size = 3) +
            labs(title = "K-means Clustering of Countries by Revenue\n For Best Results use k=3", 
                 x = "Total Revenue", 
                 y = "Country", 
                 color = "Cluster") +
            theme_minimal()
        } else {
          ggplot() +
            labs(title = "Not Enough Data for Clustering")
        }
      })
    })
    
    output$clusterTable <- renderDataTable({
      revenue_by_country <- filteredData() %>%
        inner_join(invoices, by = c("TrackId" = "InvoiceId")) %>%
        inner_join(customers, by = "CustomerId") %>%
        group_by(Country) %>%
        summarise(TotalRevenue = sum(Total), .groups = "drop") %>%
        na.omit()
      
      if (nrow(revenue_by_country) > 1) {
        kmeans_result <- kmeans(revenue_by_country$TotalRevenue, centers = input$clusters)
        revenue_by_country$Cluster <- as.factor(kmeans_result$cluster)
        datatable(
          revenue_by_country,
          options = list(
            pageLength = 10,
            dom = 'tB',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          ),
          extensions = 'Buttons',
          rownames = FALSE
        )
      } else {
        datatable(
          data.frame(Message = "Not Enough Data for Clustering"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      }
    })
    
    
  }
)
