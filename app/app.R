library(bslib)
library(echarts4r)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(leaflet)



data1 <- readRDS("data1.rds")
neighbourhoods <- readRDS("neighbourhoods.rds")


ui <- page_fluid(
  tags$head(
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Roboto:wght@300&display=swap"),
    tags$style(HTML("
                    body{
                    background-color: #FFFFFF;
                    font-family: 'Roboto';
                    color: #FF5A5F
                    }
                    .card-body{
                    color: #FF5A5F;
                    font-weight: bold;
                    }
             
                    "))
  ),
  h1("Airbnb Apartments in Buenos Aires City"),
  p(HTML(paste("Data obtained from ", "<a href = 'https://insideairbnb.com/get-the-data/'>", "here", "</a>", ". 29 January 2025"))),
  title = "Airbnb app",
  layout_columns(
    card(
         sliderInput(inputId = "price", 
                     label = "Price per Night (US$)",
                     value = c(20,40),
                     min = 16,
                     max = 137,
                     width = "100%")),
    card(sliderInput(inputId = "rating", 
                        label = "Rating",
                        value = c(4,5),
                        min = 1,
                        max = 5)),
    col_widths = c(6,6),
    row_heights = c(3)
  ),
  layout_columns(
    card(
      pickerInput(inputId = "barrio",
                     label = "Neighbourhood",
                     choices = neighbourhoods,
                  selected = neighbourhoods,
                     multiple = TRUE,
                  options = list("actions-box" = TRUE))
      ),
    card(echarts4rOutput(outputId = "plot")),
    col_widths = c(3,9)

  
  )

)



# Define server logic required to draw a histogram
server <- function(input, output) {

  my_scale <- function(x){
    scales::rescale(x, to = c(5, 20))
  }
  
  subseted <-reactive({
    req(input$barrio, input$price, input$rating)
    data1 %>% 
    filter(between(price, input$price[1], input$price[2]) &
             neighbourhood_cleansed %in% input$barrio &
             between(review_scores_rating, input$rating[1], input$rating[2]))})

    
  
  output$plot <-renderEcharts4r(
subseted() %>% 
    e_charts(longitude) %>% 
    e_leaflet(
      center = c(-58.4, -34.6),
      zoom = 12) %>% 
    e_leaflet_tile( template = "https://{s}.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png") %>% 
    e_scatter(latitude,
              size = price,
              coord_system = "leaflet",
              bind = listing_url,
              scale = my_scale) %>%
    e_add_unnested("rating", review_scores_rating) %>% 
    e_tooltip(formatter = htmlwidgets::JS("
      function(params){
      let rating = params.data.rating ? params.data.rating : 'N/A';
        return(`<div style='padding:5px;'>
        <a href = '${params.name}' target='_blank' style='font-weight:bold; color:#FF5A5F;'><strong>See in Airbnb</strong></a><br/>
        Price: US$ ${params.value[2]}<br/>
        Rating : ${rating}\u2B50 </div>`)
      }
    "), trigger = "item",
              enterable = TRUE) %>% 
    e_legend(show = FALSE) %>% 
  e_show_loading()
)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
