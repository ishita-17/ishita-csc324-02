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
library(shinythemes)

# Reading relevant files 
df <- read.csv("https://raw.githubusercontent.com/ishita-17/ishita-csc324-02/main/AI_index_db.csv")
world <- read_sf('world_shape/ne_50m_admin_0_countries.shx')


## Set up the UI object
ui <- navbarPage(title= "AI Investment by 62 Countries",
                 theme=shinytheme("cyborg"),  # shiny theme
                 inverse = T,    #  inverse the color of navigation bar
                 tabPanel("Graphs",
                          # Bar Plot
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          selectizeInput(inputId = "X_axis", label = "X-axis", choices = c("Cluster", "Income.group", "Political.regime")),  # options for user to decide X-axis variable
                                          selectInput(inputId = "check", label = "Y-Axis Scale", choices = c("Fill", "Stack"))), # option for user to decide Y-axis scale 
                                          
                                        mainPanel(
                                          plotOutput('graph'))
                          ), 
                          # Bubble Plot
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          selectizeInput(inputId = "bubble_size", label = "Bubble Size", choices = c("Talent", "Operating.Environment",
                                                                                                              "Government.Strategy", "Commercial", "Total.score"))),  # options for user to change size of bubbles 
                                        mainPanel(
                                          plotlyOutput('bubble'))
                          ),
                          # Spider Plot
                          sidebarLayout(position = "left",
                                        sidebarPanel(
                                          selectizeInput(inputId = "country", label = "Country", choices = unique(df$Country))),   # options for user to pick 1 country
                                        
                                        mainPanel(
                                          plotOutput('spider'),    # Spider Plot
                                          tableOutput("table")),   # Table with numeric values
                          )
                 ),
                 tabPanel("World Map",
                          htmlOutput("body_text"),   # Map Instructions
                          leafletOutput('map'),      # Leaflet Map
                 )
)


## Set up the server function
server <- function(input, output){
  
  # output for Map Instructions
  output$body_text <- renderText({
    "<h3>Map Instructions</h3>
    <p>1. Hover over each shaded country to see the country name with the Total Score <br>
       2. Click on the country to see its Income Group</p>"
  })
  
  # setting up data frame for Leaflet Map
  df2 <- left_join(x = df, y = world, by = c("Country" = "NAME_EN"))
  # Convert MULTIPOLYGON geometries
  multipolygons <- lapply(df2$geometry, function(poly_list) st_multipolygon(poly_list))
  
  # Create sf object
  sf_data <- reactive({
    st_sf(data = df2, geometry = multipolygons)
  })
  
  # creating color palette for map
  col_vals <- df2$Total.score
  col_vals[is.infinite(col_vals)] <- NA   
  pal <- colorNumeric("magma", domain = col_vals)   
  
  # creating labels for each country (when hovered)
  myLabels <- paste("<strong>", df2$Country, "</strong>", "<br/>", 
                    "Total Score:", prettyNum(df2$Total.score, big.mark = ","))  # Adds commas to pop
  
  # creating popup label for each country (clicked)
  myPopups <- paste("Income Group: ", df2$Income.group)
  
  # creating title object
  tag.map.title <- tags$style(HTML("           
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(0,0,0);
    font-weight: bold;
    font-size: 28px;
  }"))
  title <- tags$div(
    tag.map.title, HTML("Interactive World Map")
  ) 
  
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
                title = "Total Score", position = "bottomleft", na.label = "Missing") %>%
      addControl(title, position = "topleft", className="map-title")
    
  })
  
  # creating reactive data frame for bar plot
  data2 <- reactive(df %>% select(input$X_axis, Region)) # making data reactive
  
  # creating bar plot
  output$graph <- renderPlot({
    
    new <- isolate(data2())
    # creating bar graph 
    ggplot(new, aes_string(x = input$X_axis, fill="Region")) +
      geom_bar(position=input$check) + 
      labs(x = input$X_axis, 
           y = "Frequency", 
           fill = "Region", 
           position = input$check, 
           title = "Bar Graph for Cluster with Different Variables") +
      theme(plot.title = element_text(size=27, face= "bold", colour= "black", hjust = 0.5),
            axis.title.x = element_text(size=14, face="bold", colour = "black"),    
            axis.title.y = element_text(size=14, face="bold", colour = "black"),
            axis.text = element_text(size=14, colour = "black"),
            legend.text = element_text(size=14, face="bold", colour = "black"),
            legend.title = element_text(size=18, face="bold", colour = "black"))
  })
  
  # creating reactive data frame for bubble plot
  data3 <- reactive({
    df %>%
      arrange(desc(!!input$bubble_size)) %>%
      mutate(Country = factor(Country)) %>%
      mutate(text = paste0("Country: ", Country, "\n", input$bubble_size, ": ", !!as.name(input$bubble_size), "\nResearch: ", Research, "\nDevelopment: ", Development, sep = "")) %>%
      select(Research, Development, Country, text, input$bubble_size)
  })
  
  # creating bubble plot
  output$bubble <- renderPlotly({
    new <- isolate(data3())
    p <- ggplot(data3(), aes_string(x = "Research", y = "Development", size = input$bubble_size, color = "Country", text = "text")) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(1.4, 19), name = input$bubble_size) +
      scale_color_viridis(discrete = TRUE, guide = FALSE, option = "C") +
      theme_ipsum() +
      theme(legend.position = "none") +
      labs(x = "Research", y = "Development", size = input$bubble_size, color = "Country", title = "Bubble Plot for Research and Development of 
           Each Country with Size of Bubbles as Different Variables") +
      theme(axis.title.x = element_text(size=14, face="bold", colour = "black"),    
            axis.title.y = element_text(size=14, face="bold", colour = "black"),
            axis.text = element_text(size=14, colour = "black"),
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text")   # converting static plot to interactive plot by using plotly
  })
  
  # creating reactive data frame for spider plot
  filteredData <- reactive({
    df3 <- df[, !names(df) %in% c("Political.regime", "Income.group", "Cluster", "Region")]
    subset(df3, Country == input$country)
  })
  
  # creating spider plot
  output$spider <- renderPlot({
    values <- as.data.frame(filteredData()[, -1])
    colnames(values) <- names(filteredData())[-1]
    values2 <- rbind(max(values), min(values), values)
    
    radarchart(values2, axistype = 1, 
               pcol = rgb(0.2, 0.5, 0.5, 0.9), pfcol = rgb(0.2, 0.5, 0.5, 0.5), plwd = 4, palcex = 20,
               cglcol = "darkgrey", cglty = 1, axislabcol = "black", caxislabels = seq(0, 20, 5), cglwd = 0.8, calcex = 1.5,
               vlcex = 1.2)
    
    title(main = "Spider Plot for Each Country for all Numeric Variables", cex.main = 2)   # Adjust the title font size
  })
  
  # creating table with the numeric values used in the spider plot
  output$table <- renderTable({
    table_data <- as.data.frame(filteredData()[, -1])
    table_data
  })
  
}

## Build and run shiny app
shinyApp(ui, server)

