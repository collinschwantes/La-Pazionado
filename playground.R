m = leaflet() %>% addTiles()

map <- leaflet(df) %>%  
    setView(lng = -86.257367, lat = 0, zoom = 2) %>% 
    addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>% 
    addMarkers(lng = df$lon, lat = df$lat, layerId = df$Title, popup = df$Title)

df$lon

addMarkers

df

a
b
a == "as" & b == "ss"

c <- if((a == "as") & (b != "as")) {df} else {"PRINT"}
c
is.data.frame(df)
df[df$Category == c,]

c <- "Country"
e <- "Category"
f <- "Country and Category "
g <- "Neither"

b <- "as"
a <- "as"

d <- if((a == "as") & (b == "as")) {
        g} else {
          if((a == "as") & (b != "as")) {c} else {
              if((a != "as") & (b == "as")) {e} else {f}
  }   
}

d

dt <- as.data.table(df)
long <- 98
title1 <- dt[lon %like% long]
str(dt)
title1$Title

df[,2]

