library(shiny)
library(leaflet)
library(ggplot2)
library(ggmap)
library(data.table)
library(stringr)
library(rsconnect)

df <- read.csv(file = "./apaz.csv")



ui <- bootstrapPage(
  
  #map element 
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  # select category and country
  absolutePanel( bottom = 30, left = 20, align = "center",
                 h1(style = "font-family:Averia Sans Libre; font-weight: 700; color: #FFFFFF;","APAZIONADOS"),
                 h4(style = "font-family:Averia Sans Libre; font-weight: 700; color: #FFFFFF;",
                    "Mapeamos proyectos de paz en Latinoamérica"),
                 h4(style = "font-family:Averia Sans Libre; font-weight: 700; color: #e4d89a;",
                    "Fase I: México")
  ),
  absolutePanel(style = "background-color:rgba(0,0,8,.5); font-family:Averia Sans Libre; font-weight: 600;line-height: 2; color: #FFFFFF;",
                width = "300px", top = 10, right = 10,
                #selectizeInput(inputId = "title", label = "Título", choices = Título.del.Proyecto, 
                #              multiple = T, options = list(maxItems = 5)),
                selectizeInput(selected = "Todos",inputId ="category",label =  "Categoría",
                               choices = category, multiple = T),
                selectInput(selected = "Todos",inputId = "country", label = "País",
                            choices = country),
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
  df <- read.csv(file = "./apaz1.csv")
  View(df)
  df$category <- strsplit(as.character(df$X.Cómo.categoriza.su.proyecto.), ", ")
  
  df$category <- strsplit(as.character(df$X.Cómo.categoriza.su.proyecto.), ", ")
  category <- unlist(df$category)
  category <- c(category, "Todos")
  names(category) <- category
  category
  
  country <- df$País
  country <- c(as.character(country), "Todos")
  names(country) <- country
  
  
  df$City <- paste(df$Ciudad,df$Estado.o.Provincia,df$País)
  df <- cbind(df, geocode(as.character(df$City), output = "more")) 

  df$country <- as.factor(df$country)
  df$lon <- as.factor(df$lon)
  df$dest <- paste("./www/", df[,2], str_sub(df$Foto, -4), sep = "")
  #match("dest", names(df))
 # for(i in df$Foto){
#    download.file(url = as.character(i), destfile = df[df$Foto == i, 43])
 # }

  #INPUTS From categories
  #default should be all 
  points <- reactive ({ 
    if((input$category == "Todos") & (input$country == "Todos")) {df} else {
      if((input$category == "Todos") & (input$country != "Todos")) {pais <- input$country
        df_subco <- df[df$País %in% pais,]} else {
        if((input$category != "Todos") & (input$country == "Todos")) { category <- input$category
        df_subca <- df[df$category %in% category,]} else { category <- input$category
          pais <- input$country
          df_sub <- df[df$category %in% category,]
          df_sub <- df_sub[df$País %in% pais,]}
      }   
    }
  })
  
  
  output$map <- renderLeaflet({
    leaflet(df) %>%  
      setView(lng = -99.257367, lat = 19.47, zoom = 5) %>% 
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
    list(src = df[df$lon == long,43], width = 300)
  }, deleteFile = FALSE)
  
  output$summary <- renderText({
    click <- (input$map_marker_click) 
    long <- as.numeric(click[4])
    obj <- as.character(df[df$lon == long,5])
  })
  
}

shinyApp(ui, server)

