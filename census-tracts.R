#Createing a leaflet map for all census tracts in US
library(leaflet)
library(tidycensus)
library(sf)


options(tigris_use_cache = TRUE)
key <- read.delim(file="private/Census Api Key.txt",header=FALSE)
Sys.setenv(census_api_key=key[[1]])

acs.poverty <- c("B17020_001E")

zip.poverty <- get_acs(geography="zcta",
                                variables=acs.poverty,
                                geometry=TRUE)
zip.poverty <- zip.poverty %>% 
  st_transform(crs = "+init=epsg:4326") 

testset <- head(zip.poverty)



make.map <- function(c,h,a,d,o){
  
  #c: Data Set
  #h: Data Set With Quantitative Column
  #a: Label
  #d: Color Bins
  #o: Destination
  # 
  labels <- a
  bins <- d
  pal <- colorBin("GnBu", domain=h , bins = bins)

  m <- leaflet(c,padding = 0,width=1000,height=1000,
                 sizingPolicy=leafletSizingPolicy(browser.fill=TRUE)
                               )%>%
    addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels) %>%

    addPolygons(
      fillColor = ~pal(h),
      weight= 0.5,
      smoothFactor = 0,
      opacity = 1,
      color = "black",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "black",
        fillOpacity = 0.9,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
       direction = "auto"
        )
      ) %>%
    addLegend(pal = pal, values = ~h, opacity = 0.9, title = NULL,
              position = "bottomright"
              )
  mapview::mapshot(m,vheight=100,vwidth=100,file="map.pdf")
  m
}

make.map(c=testset,
         h=testset$estimate,
         a=testset$GEOID,
         d=c(0,10000,20000,30000,40000,50000,60000,70000,80000,Inf)
         
)


