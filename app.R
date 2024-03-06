# Loading relevant packages
library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
library(plotly)
library(leaflet)
library(sf)
library(fmsb)
library(knitr)

df <- read.csv("https://raw.githubusercontent.com/ishita-17/ishita-csc324-02/main/AI_index_db.csv")
world <- read_sf('/Users/ishi/csc324-labs/world_shape/ne_50m_admin_0_countries.shx')


## Set up the UI object
ui <- navbarPage(title= "AI Investment by 62 Countries",
                 inverse = T,    #  inverse the color of navigation bar
                 tabPanel("Bar Plot",
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          selectizeInput(inputId = "X_axis", label = "X-axis", choices = c("Cluster", "Income.group")), 
                                          selectInput(inputId = "check", label = "Y-Axis Scale", choices = c("Fill", "Stack"))), # option for user to decide Y-axis scale 
                                          
                                        mainPanel(
                                          plotOutput('graph'))
                          ), 
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          selectizeInput(inputId = "bubble_size", label = "Size", choices = c("Talent", "Operating.Environment",
                                                                                                              "Government.Strategy", "Commercial", "Total.score"))),
                                        
                                        mainPanel(
                                          plotlyOutput('bubble'))
                          ),
                 ),
                 tabPanel("World Map",
                              leafletOutput('map')
                 )
                                        
                          
                 
)

## Set up the server function
server <- function(input, output){
  
  df2 <- left_join(x = df, y = world, by = c("Country" = "NAME_EN"))
  # Convert MULTIPOLYGON geometries
  multipolygons <- lapply(df2$geometry, function(poly_list) st_multipolygon(poly_list))
  
  # Create sf object
  sf_data <- reactive({
    st_sf(data = df2, geometry = multipolygons)
  })
  col_vals <- df2$Total.score
  col_vals[is.infinite(col_vals)] <- NA   
  
  pal <- colorNumeric("magma", domain = col_vals)
  
  myLabels <- paste("<strong>", df2$Country, "</strong>", "<br/>", 
                    "Total Score:", prettyNum(df2$Total.score, big.mark = ","))  # Adds commas to pop
  myPopups <- paste("Political Regime: ", df2$Political.regime)
  
  
  # creating map using Leaflet
  output$map <- renderLeaflet({
    
    leaflet(sf_data()) %>% addTiles() %>% 
      addPolygons(fillColor = pal(col_vals),
                  weight = 1,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "grey",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = lapply(myLabels, htmltools::HTML),
                  popup = myPopups) %>%
      addLegend(pal = pal, values = col_vals,
                title = "Total Score", position = "bottomleft", na.label = "Missing")
    
  })
  
  
  data2 <- reactive(df %>% select(input$X_axis, Region)) # making data reactive
  
  output$graph <- renderPlot({
    
    new <- isolate(data2())
    # creating bar graph 
    ggplot(new, aes_string(x = input$X_axis, fill="Region")) +
      geom_bar(position=input$check) 
  })
  
  data3 <- reactive({
    df %>%
      #mutate(Research = round(Research, 2)) %>%
      #mutate(!!input$bubble_size := round(aes_string(input$bubble_size), 2)) %>%  
      #mutate(Development = round(Development, 2)) %>%
      arrange(desc(!!input$bubble_size)) %>%
      mutate(Country = factor(Country)) %>%
      mutate(text = paste0("Country: ", Country, "\n", input$bubble_size, ": ", aes_string(input$bubble_size), "\nResearch: ", Research, "\nDevelopment: ", Development, sep = "")) %>%
      select(Research, Development, Country, text, input$bubble_size)
  })
  
  output$bubble <- renderPlotly({
    new <- isolate(data3())
    p <- ggplot(data3(), aes_string(x = "Research", y = "Development", size = input$bubble_size, color = "Country", text = "text")) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(1.4, 19), name = input$bubble_size) +
      scale_color_viridis(discrete = TRUE, guide = FALSE, option = "C") +
      theme_ipsum() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  
  })
}

## Build and run
shinyApp(ui, server)

