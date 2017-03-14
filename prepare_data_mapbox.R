#===========================================
# @Project: wland
# @Name: main
# @url: http://slow-data.com/wland
# @author: jprimav
# @date: 2017/02
#===========================================


## --- Libraries
library(RColorBrewer)
library(colorspace)
library(dplyr)
library(leaflet)
library(htmlwidgets)


## --- read data
load("C:/Users/pc/Documents/slowdata/post/wland/git/data/cafesPigneto.RData")


## --- Prepare popups
line1 <- rep("<b>Not available</b>", nrow(sp_torpignattara@data))
for(i in 1:nrow(sp_torpignattara@data)) {
  if(!is.na(sp_torpignattara@data$name[i])) line1[i] <- paste0("<b><a href='", sp_torpignattara@data$website[i], "' target='_blank' >", sp_torpignattara@data$name[i], "</a></b>")
}

line1 <- rep("<b>Not available</b>", nrow(sp_torpignattara@data))
for(i in 1:nrow(sp_torpignattara@data)) {
  if(!is.na(sp_torpignattara@data$name[i])) line1[i] <- ifelse(!is.na(sp_torpignattara@data$website[i]), 
                                                               paste0("<b><a href='", sp_torpignattara@data$website[i], "' target='_blank'  >", sp_torpignattara@data$name[i], "</a></b>"),
                                                               paste0("<b>", sp_torpignattara@data$name[i], "</b>"))
}


line2 <- rep("",nrow(sp_torpignattara@data))
for(i in 1:nrow(sp_torpignattara@data)) {
  if(!is.na(sp_torpignattara@data$description[i])) line2[i] <- paste0("<br/>",sp_torpignattara@data$description[i])
}
line3 <- rep("",nrow(sp_torpignattara@data))
for(i in 1:nrow(sp_torpignattara@data)) {
  if(!is.na(sp_torpignattara@data$addr[i])) line3[i] <- paste0("<br/>",sp_torpignattara@data$addr[i])
}
line4 <- rep("",nrow(sp_torpignattara@data))
for(i in 1:nrow(sp_torpignattara@data)) {
  if(!is.na(sp_torpignattara@data$opening_hours[i])) line4[i] <- paste0("<br/>",sp_torpignattara@data$opening_hours[i])
}

popups <- rep(NA, nrow(sp_torpignattara@data))
for(i in 1:nrow(sp_torpignattara@data)) {
  popups[i] <- paste0(line1[i], line2[i], line3[i], line4[i])
  }

sp_torpignattara@data$popups <- popups



## --- Exports for MapBox

# curated points in GeoJSON
str(sp_torpignattara)
class(sp_torpignattara)
bar_torpigna_curated <- geojson_json(sp_torpignattara)
# geojson_write(input = bar_torpigna_curated, file = "C:/Users/pc/Documents/slowdata/post/wland/git/data/wland_pigneto_25022017_curated.geojson")

# Torpignattara polygon
# writeOGR(zu, "C:/Users/pc/Documents/slowdata/post/wland/git/shape/4mapbox", layer="ZU_latlong", driver="ESRI Shapefile")
# writeOGR(zu_torpignattara, "C:/Users/pc/Documents/slowdata/post/wland/git/shape/4mapbox", layer="ZU_Torpigna_latlon", driver="ESRI Shapefile")