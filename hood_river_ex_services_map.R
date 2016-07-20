require(transportr)
require(dplyr)
require(tidyr)
require(XLConnect)
require(readr)
require(readxl)
require(sp)
require(ggmap)
require(ggrepel)
require(lubridate)
require(extrafont)
require(rgeos)
require(scales)
require(rgdal)
require(leaflet)
require(rvest)
require(tigris)

setwd("G:/Current/HOOD_RIVER_TMP_2016.0270")

fetchFeed("Columbia Area Transit","Data/Transit")
fetchFeed("Mt. Hood Express","Data/Transit")
fetchFeed("Sandy Area Metro","Data/Transit")
fetchFeed("Skamania","Data/Transit")
fetchFeed("Cascades East Transit","Data/Transit")

cat_routes = exportRouteShape("Data/Transit/feeds/Columbia Area Transit")
cat_stops = exportStopShape("Data/Transit/feeds/Columbia Area Transit")

hood_routes = exportRouteShape("Data/Transit/feeds/Mt. Hood Express")
hood_stops = exportStopShape("Data/Transit/feeds/Mt. Hood Express")

sam_routes = exportRouteShape("Data/Transit/feeds/Sandy Area Metro")
sam_stops = exportStopShape("Data/Transit/feeds/Sandy Area Metro")

wet_routes = exportRouteShape("Data/Transit/feeds/Skamania County Public Transit (Gorge WET Bus)")
wet_stops = exportStopShape("Data/Transit/feeds/Skamania County Public Transit (Gorge WET Bus)")

cet_routes = exportRouteShape("Data/Transit/feeds/Cascades East Transit")
cet_stops = exportStopShape("Data/Transit/feeds/Cascades East Transit")


sub_shapes = cat_routes@data %>% group_by(route_id) %>% summarise(shape_id = shape_id[1])
cat_routes = cat_routes[cat_routes$shape_id %in% sub_shapes$shape_id,]
cat_routes@data = data.frame(
  cat_routes@data %>%
    mutate(html = paste0("<b>Short Name: </b>",route_short_name,"</br>",
                         "<b>Long Name: </b>",route_long_name,"</br>"))
)

# sub_shapes = cet_routes@data %>% group_by(route_id) %>% summarise(shape_id = shape_id[1])
# cet_routes = cet_routes[cet_routes$shape_id %in% sub_shapes$shape_id,]
cet_routes@data = data.frame(
  cet_routes@data %>%
    mutate(html = paste0("<b>Short Name: </b>",route_short_name,"</br>",
                         "<b>Long Name: </b>",route_long_name,"</br>"))
)

# sub_shapes = hood_routes@data %>% group_by(route_id) %>% summarise(shape_id = shape_id[1])
# hood_routes = hood_routes[hood_routes$shape_id %in% sub_shapes$shape_id,]
hood_routes@data = data.frame(
  hood_routes@data %>%
    mutate(html = paste0("<b>Short Name: </b>",route_short_name,"</br>",
                         "<b>Long Name: </b>",route_long_name,"</br>"))
)

# sub_shapes = sam_routes@data %>% group_by(route_id) %>% summarise(shape_id = shape_id[1])
# sam_routes = sam_routes[sam_routes$shape_id %in% sub_shapes$shape_id,]
sam_routes@data = data.frame(
  sam_routes@data %>%
    mutate(html = paste0("<b>Short Name: </b>",route_short_name,"</br>",
                         "<b>Long Name: </b>",route_long_name,"</br>"))
)

# sub_shapes = wet_routes@data %>% group_by(route_id) %>% summarise(shape_id = shape_id[1])
# wet_routes = wet_routes[wet_routes$shape_id %in% sub_shapes$shape_id,]
wet_routes@data = data.frame(
  wet_routes@data %>%
    mutate(html = paste0("<b>Short Name: </b>",route_short_name,"</br>",
                         "<b>Long Name: </b>",route_long_name,"</br>"))
)

or_counties = counties(state="Oregon")
wa_counties = counties(state="Washington")
sub_or_counties = or_counties[or_counties$NAME %in%
                                c("Clackamas","Multnomah","Wasco","Hood River","Sherman"),]
sub_wa_counties = wa_counties[wa_counties$NAME %in% c("Klickitat","Clark","Skamania"),]


route_list = c(paste0("CAT-",cat_routes$route_long_name),
               #paste0("CET-",cet_routes$route_long_name),
               paste0("SAM-",sam_routes$route_long_name),
               #paste0("MHX-",hood_routes$route_long_name),
               paste0("WET-",wet_routes$route_long_name))

cat_routes$route_tag = paste0("CAT-",cat_routes$route_long_name)
#cet_routes$route_tag = paste0("CET-",cet_routes$route_long_name)
sam_routes$route_tag = paste0("SAM-",sam_routes$route_long_name)
#hood_routes$route_tag = paste0("MHX-",hood_routes$route_long_name)
wet_routes$route_tag = paste0("WET-",wet_routes$route_long_name)

leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=sub_wa_counties,group = "Washington Counties",fillColor = "blue",color="black") %>%
  addPolygons(data=sub_or_counties,group = "Oregon Counties",fillColor = "green",color="black") %>%
  addPolylines(data=cat_routes,color=~route_color, group =~route_tag, popup =~html) %>%
  #addPolylines(data=hood_routes,color=~route_color, group =~route_tag, popup =~html) %>%
  addPolylines(data=sam_routes,color=~route_color, group =~route_tag, popup =~html) %>%
  addPolylines(data=wet_routes,color=~route_color, group =~route_tag, popup =~html) %>%
  addLayersControl(overlayGroups = c("Washington Counties","Oregon Counties",
                                     unique(route_list))) %>%
  hideGroup(c("Washington Counties","Oregon Counties"))

