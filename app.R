library(shiny)
library(leaflet)
library(ggplot2)
library(ggmap)
library(data.table)
library(stringr)
library(rsconnect)




ui <- bootstrapPage(
  
  #map element 
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  # select category and country
  absolutePanel( bottom = 30, left = 20, align = "center",
                 h1(style = "font-family:Averia Sans Libre; font-weight: 700; color: #e4d89a;","A Pazionados"),
                 h4(
                   style = "font-family:Averia Sans Libre; font-weight: 400; color: #abaebf;","a collaboration between"),
                 h3(  
                   style = "margin-top: 10px; font-family:Averia Sans Libre; font-weight: 700; color: #abaebf;", 
                   a(style = "color: #abaebf", href = "www.andreaarzaba.com","Andrea Arzaba"), "and",
                   a(style = "color: #abaebf", href = "www.collinschwantes.com", "Collin Schwantes")
                 )
  ),
  absolutePanel(style = "background-color:rgba(0,0,8,.5); font-family:Averia Sans Libre; font-weight: 600;line-height: 2; color: #FFFFFF;",
                width = "300px", top = 10, right = 10,
                selectInput(selected = "All",inputId =  "category",label =  "Project Type",
                            choices = list("All" = "All", 
                                           "Art" = "Art", 
                                           "Community Engagement" = "Community Engagement",
                                           "Education" = "Education",
                                           "Media" = "Media",
                                           "Sports" = "Sports")),
                selectInput(selected = "All", "country", "Country",
                            choices = list("All" = "All",
                                           "Ecuador" = "ecuador",
                                           "El Salvador" = "el salvador",
                                           "Guatemala" = "guatemala",
                                           "Mexico" = "mexico",
                                           "Venezuela" = "venezuela")),
                h2(style = "font-family:Averia Sans Libre; font-weight: 700; color: #FFFFFF;", textOutput(outputId = "title")),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                imageOutput(outputId = "image", height = "75%"),
                h3(style = "font-family:Averia Sans Libre; font-weight: 400; color: #FFFFFF;", textOutput(outputId = "summary"))
  )               
)

server <- function(input, output) {
  df <- read.csv(file = "./Apazionados.csv")
  df <- cbind(df, geocode(as.character(df$City), output = "more")) 
  df$country <- as.factor(df$country)
  df$lon <- as.factor(df$lon)
  df$dest <- paste("./www/", df[,2], str_sub(df$Image, -4), sep = "")
  for(i in df$Image){
    download.file(url = as.character(i), destfile = df[df$Image == i, 25])
  }
  
  #INPUTS From categories
  #default should be all 
  points <- reactive ({ 
    if((input$category == "All") & (input$country == "All")) {df} else {
      if((input$category == "All") & (input$country != "All")) {df_subco <- df[df$country == input$country,]} else {
        if((input$category != "All") & (input$country == "All")) {df_subca <- df[df$Category == input$category,]} else { 
          df_sub <- df[df$Category == input$category,]
          df_sub <- df_sub[df$country == input$country,]}
      }   
    }
  })
  
  
  output$map <- renderLeaflet({
    leaflet(df) %>%  
      setView(lng = -86.257367, lat = 0, zoom = 3) %>% 
      addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") 
  })
  
  #markers
  observe({leafletProxy("map", data = points()) %>% clearMarkers() %>%
             addMarkers(lng = ~lon, lat = ~lat)
  })
  
  output$title <- renderText({
    click <- (input$map_marker_click) 
    long <- as.numeric(click[4])
    obj <- as.character(df[df$lon == long,2]) 
  })
  
  output$image  <- renderImage({ 
    click <- (input$map_marker_click) 
    long <- as.numeric(click[4])
    list(src = df[df$lon == long,25], width = 300)
  }, deleteFile = FALSE)
  
  output$summary <- renderText({
    click <- (input$map_marker_click) 
    long <- as.numeric(click[4])
    obj <- as.character(df[df$lon == long,4])
  })
  
}

shinyApp(ui, server)

deployApp('/Users/featherlite569/Documents/La Pazionado/La Pazionado')
