## --- Libraries
library(rgdal)
library(maptools)
library(PBSmapping)
library(dplyr)
library(rgeos)
library(geojsonio)



## --- Get data

# OSM nodes obtained with Overpass turbo (http://overpass-turbo.eu/) query:
# bbox used to include all "Torpignattara" (neighborhood in Rome) (http://mapparoma.blogspot.it/p/zone-urbanistiche.html)
# Roughly being [12.512956,41.877646,12.551193,41.899626] (retrieved using http://boundingbox.klokantech.com/)


## --- query for OverpassTurbo (last run 25/02/2017)

# [out:json][timeout:25];

# // gather results
# (
#   node
#   ["amenity"~"bar|cafe|pub|coworking_space|library|arts_centre|university|community_centre|cinema|nightclub|social_centre|internet_cafe|fast_food|restaurant|ice_cream"]   
#   ["internet_access"~"wlan|yes|public|wifi|wlan;terminal|terminal;wlan|wlan;wired"]

#   ({{bbox}});
#   node
#   ["office"="coworking"]
#   ["internet_access"~"wlan|yes|public|wifi|wlan;terminal|terminal;wlan|wlan;wired"]
#   ({{bbox}});
# ); 

# // print results
# out body;
# >;
# out skel qt; 
# ---------------------------------------------------------


## --- Read data

# read GEOJson
dat <- readOGR(dsn = "C:/Users/pc/Documents/slowdata/post/wland/git/data/wland_pigneto_25022017.geojson",
               layer = "OGRGeoJSON")

# Read SHAPEFILE with borders of 'zone urbanistiche' (neighborhoods)
zu <- readOGR(dsn = "C:/Users/pc/Documents/slowdata/post/wland/git/shape", layer = "ZU_COD")

# change CRS for zu shape file to coicide with OSM data
crslonglat = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
zu <- spTransform(zu, CRS=crslonglat)
plot(zu)
plot(dat, pch = 16, cex = .5, col = "red", add = TRUE)

# find polygon of interest
pos_torpignattara <- which(zu@data[,"ZU"]=="Torpignattara")
zu_torpignattara <- zu[pos_torpignattara,]
zu_outside <- zu[-pos_torpignattara,]
zu_outside <- gUnionCascaded(zu_outside)


## --- clean data
# get polygons from shapes
zu_poly <- SpatialPolygons2PolySet(zu)
zu_poly_torpignattara <- SpatialPolygons2PolySet(zu_torpignattara) # get polygons from shapes

# test which points belong to Torpignattara polygon
pip <- point.in.polygon(dat@coords[,1], dat@coords[,2], zu_poly[zu_poly$PID==pos_torpignattara,"X"], zu_poly[zu_poly$PID==pos_torpignattara,"Y"])
nodes_torpignattara <- which(pip == 1)

# keep only nodes belonging to Torpignattara polygon
sp_torpignattara <- dat[nodes_torpignattara,]
str(sp_torpignattara@data)

# drop never-occurring levels explicitly
for(i in 1:ncol(sp_torpignattara@data)) sp_torpignattara@data[,i] <- droplevels(sp_torpignattara@data[,i])
str(sp_torpignattara@data)

# keep only nodes suitable for working/studying based on survey
sp_torpignattara@data[,c("name","amenity")]
ok <- c(1, 3:5, 10:11, 15, 19:21, 23, 25:26) # manual selection
sp_torpignattara <- dat[ok,]
sp_torpignattara@data$name
for(i in 1:ncol(sp_torpignattara@data)) sp_torpignattara@data[,i] <- droplevels(sp_torpignattara@data[,i])
str(sp_torpignattara@data)
sp_torpignattara@data$name



## --- Quick data exploration

# visual
plot(zu)
plot(sp_torpignattara, pch = 16, cex = .5, col = "red", add = TRUE)
plot(zu_torpignattara)
plot(sp_torpignattara, pch = 16, cex = .5, col = "red", add = TRUE)

# tabular
any(is.na(sp_torpignattara@data$amenity)) # all places are amenities
sp_torpignattara@data %>%
  group_by(amenity) %>%
  summarise(
    n = length(X.id)) %>%
  arrange(desc(n))


## --- Determine centroid of target poly "Torpignattara"
c <- gCentroid(zu_torpignattara)@coords


## --- Classify amenities 
sp_torpignattara@data %>%
  select(amenity, name)

cat <- c("Cafe/Bar",
         "Bistrot/Fast-Food",
         "Cafe/Bar",
         "Cafe/Bar",
         "Cafe/Bar",
         "Cafe/Bar",
         "Bistrot/Fast-Food",
         "Bistrot/Fast-Food",
         "Cafe/Bar",
         "Wine-Bar",
         "Cafe/Bar",
         "Pub",
         "Wine-Bar")

sp_torpignattara@data$cat <- cat
sp_torpignattara@data %>%
  select(name, cat)



## --- Clean features

# names
sp_torpignattara@data$name
levels(sp_torpignattara@data$name)
levels(sp_torpignattara@data$name)[2] <- "Caffe Sospeso"
levels(sp_torpignattara@data$name)[5] <- "Gastro Cibo & Caffe"
levels(sp_torpignattara@data$name)[6] <- "Hop Corner"
levels(sp_torpignattara@data$name)[8] <- "Lo Yeti"
levels(sp_torpignattara@data$name)[11] <- "Violetta Caffe & Cucina"
levels(sp_torpignattara@data$name)
any(is.na(sp_torpignattara@data$name))

sp_torpignattara@data %>%
  group_by(cat) %>%
  summarise(
    n = length(X.id)) %>%
  arrange(desc(n))


# descriptions are ok?
sp_torpignattara@data$description
any(is.na(sp_torpignattara@data$description))

# wheelchair are ok?
sp_torpignattara@data$wheelchair
any(is.na(sp_torpignattara@data$wheelchair))

# addresses are ok?
sp_torpignattara@data %>%
  select(name, addr.street, addr.housenumber)
sp_torpignattara@data$addr.street
sp_torpignattara@data$addr.housenumber

addr <- rep(NA, nrow(sp_torpignattara@data))
for(i in 1:nrow(sp_torpignattara@data)) {
  if(!is.na(sp_torpignattara@data$addr.street[i]) & !is.na(sp_torpignattara@data$addr.housenumber[i]))
    addr[i] <- paste(sp_torpignattara@data$addr.street[i], sp_torpignattara@data$addr.housenumber[i])
}
addr
sp_torpignattara@data$addr <- addr
sp_torpignattara@data$addr

# website are ok?
sp_torpignattara@data$website

# opening_hours are ok?
sp_torpignattara@data[,c("name","opening_hours")]

# save.image("C:/Users/pc/Documents/slowdata/post/wland/git/data/cafesPigneto.RData")
