library(googleVis)
library(tidyverse)
library(webshot)
library(magick)
library(pdftools)

# https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html

# how to use gvismap: http://www.rdocumentation.org/packages/googleVis/functions/gvisMap
# good doc for setting options

# https://developers.google.com/chart/interactive/docs/gallery/map#Configuration_Options

# gvismap source code: https://code.google.com/p/google-motion-charts-with-r/source/browse/trunk/R/gvisMap.R?r=358


# setwd
setwd("C:/Users/Stephen/Desktop/R/googlevis")

AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(AndrewMap)

# example 2
map <- data.frame(location = c("56 talmadge hill road, prospect, ct 06712", 
                               "22 skyline drive, prospect, ct 06712", "p o box 2939 montgomery al 36105"), tip = c("home", "neighbor", "po box"))

# using lat lon instead of full address, you must combine lat long, separated by colon
# eg "10.8:-35.5", see below for example
# head(Andrew)

gc <- geocode("56 talmadge hill road, prospect, ct 06712")
gc2 <- geocode("22 skyline drive, prospect, ct 06712")
gc_df <- bind_rows(gc, gc2)
gc_df$lat_long <- str_c(gc_df$lat, gc_df$lon, sep = ":")

map <- data.frame(gc_df$lat_long, tip = c("home", "neighbor"))
names(map)[1] <- "location"

# example 3
test <- read.csv("zip.csv")
test$state <- "TX"
test$country <- "united states"

test2 <- test
test2$address1 <- str_c(test2$city, test2$state, test2$zip, sep = ", ")
test2$address2 <- str_c(test2$state, test2$zip, sep = ", ")
test2$address3 <- str_c(test2$state, test2$zip, test2$country, sep = ", ")
test2$tip <- test2$address1
test2 <- test2[1:30, ]
map <- data.frame(location = test2$address1, tip = test2$tip)

# example 4 - geo-coding multiple addresses for faster plotting, using test 2 from above 
address <- data.frame(address = c("56 talmadge hill road, prospect, ct 06712", "22 skyline drive, prospect, ct 06712"))

for(i in 1:nrow(test2)){
        gc <- geocode(test2$address1[i])
        test2$lat[i] <- gc[2]
        test2$lon[i] <- gc[1]
        #test2$lat_long[i] <- str_c(test2$lat[i], test2$lon[i], sep = ":")
}

distinct <- distinct(select(test2, lat, lon))
unlist_lat <- unlist(test2$lat)
unlist_lon <- unlist(test2$lon)
test2$location <- str_c(unlist_lat, unlist_lon, sep = ":")

map <- data.frame(test2$lat_lon, test2$address1)
names(map) <- c("location", "tip")

map_plot <- gvisMap(map, "location" , "tip", 
                options=list(showTip=TRUE, 
                     showLine=TRUE, 
                     enableScrollWheel=TRUE,
                     mapType='normal', 
                     useMapTypeControl=TRUE))

plot(map_plot)


# online example

AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(AndrewMap)


# example of changing icons
# http://finzi.psych.upenn.edu/library/googleVis/html/gvisMap.html

## Example with address, here UK post-code and some html code in tooltip

df <- data.frame(Postcode=c("EC3M 7HA", "EC2P 2EJ"),
                 Tip=c("<a href='http://www.lloyds.com'>Lloyd's</a>",
                       "<a href='http://www.guildhall.cityoflondon.gov.uk/'>Guildhall</a>"))

M2 <- gvisMap(df, "Postcode", "Tip",
              options=list(showTip=TRUE, mapType='normal',
                           enableScrollWheel=TRUE))

plot(M2)

## Change mapping icons
M3 <- gvisMap(df, "Postcode", "Tip",
              options=list(showTip=TRUE, mapType='normal',
                           enableScrollWheel=TRUE,
                           icons=paste0("{",
                                        "'default': {'normal': 'http://icons.iconarchive.com/",
                                        "icons/icons-land/vista-map-markers/48/",
                                        "Map-Marker-Ball-Azure-icon.png',\n",
                                        "'selected': 'http://icons.iconarchive.com/",
                                        "icons/icons-land/vista-map-markers/48/",
                                        "Map-Marker-Ball-Right-Azure-icon.png'",
                                        "}}")))

plot(M3)

# my example w changing icons and colors
# working directory to shiny/opcs folder
# pins don't show up on govt internet, but do on mobile internet

# google maps icons http://kml4earth.appspot.com/icons.html#pushpin

# more markers https://mapicons.mapsmarker.com/
# with an api to create your own marker - insert text = ? and color = ?
# for colors, use hexadecimal color code http://www.w3schools.com/tags/ref_colorpicker.asp
# http://thydzik.com/thydzikGoogleMap/markerlink.php?text=1&color=5680FC

datafile <- read.csv("data/datafile_20150827.csv")
small <- datafile[1:3, 35:38]
small$Marker <- c("blue", "green", "yellow")
small

map <- gvisMap(small, "lat_lon", "address",
              options=list(showTip=TRUE, mapType='normal',
                           enableScrollWheel=TRUE,
                           icons = str_c("{", "'default': {'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=03%7c5680FC%7c000000&.png%3f',\n",
                                        "'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=03%7c5680FC%7c000000&.png%3f'",
                                        "}}")))

plot(map)

# example specifying the colors 
# http://www.rdocumentation.org/packages/googleVis/functions/gvisMap
map <- gvisMap(small, "lat_lon", "address",
               options=list(showTip=TRUE, mapType='normal',
                            enableScrollWheel=TRUE,
                            colors="['#cbb69d', '#603913', '#c69c6e']"))

plot(map)

# example with small from above using marker png saved to hard drive
# doesn't seem like it can be done?? no documentation for it on google's site
map <- gvisMap(small, "lat_lon", "address",
               options=list(showTip=TRUE, mapType='normal',
                            enableScrollWheel=TRUE,
                            icons = str_c("{",
                                          "'default': {'normal': '/H:/R/shiny/opcs/www/1990.png',\n",
                                          "'selected': '/H:/R/shiny/opcs/www/1990.png'",
                                          "}}")))

map <- gvisMap(small, "lat_lon", "address",
               options=list(colors="['#cbb69d', '#603913', '#c69c6e']"))

plot(map)

# example with small from above using multiple marker colors
map <- gvisMap(small, "lat_lon", "address",
               options=list(showTip=TRUE, mapType='normal',
                            enableScrollWheel=TRUE,
                            icons = str_c("{",
                                "'blue': {'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=B%7c5680FC%7c000000&.png%3f',\n",
                                "'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=B%7c5680FC%7c000000&.png%3f'",
                                "},", "'green': {'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=G%7c66FF33%7c000000&.png%3f',\n",
                                "'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=G%7c66FF33%7c000000&.png%3f'", "},",
                                "'yellow': {'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=Y%7cFFFF00%7c000000&.png%3f',\n",
                                "'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=Y%7cFFFF00%7c000000&.png%3f'", "}}")))

plot(map)

kstring <- "{'blue': {
                'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=B%7c5680FC%7c000000&.png',
                'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=B%7c5680FC%7c000000&.png'
                },
             'green': {
                'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=G%7c66FF33%7c000000&.png',
                'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=G%7c66FF33%7c000000&.png'
                },
             'yellow': {
                'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=Y%7cFFFF00%7c000000&.png',
                'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=Y%7cFFFF00%7c000000&.png'
                }
             }"


kmap <- gvisMap(small, "lat_lon", "address", "Marker",
               options=list(showTip=TRUE, mapType='normal',
                            enableScrollWheel=TRUE,
                            icons=kstring))

library(jsonlite)
json_small <- toJSON(small, pretty = TRUE)

#plot(kmap)
kmap[0]



###############################################################################
###############################################################################
################################################################################


# sankey plot

# extensive list of customization options in google docs
# https://developers.google.com/chart/interactive/docs/gallery/sankey
# https://stackoverflow.com/questions/29371977/how-to-change-node-and-link-colors-in-r-googlevis-sankey-chart

# create data
data <- tibble(from = c(rep("A", 3), rep("B", 3)), to = c(rep(c("X", "Y", "Z"),2)), weight = c(5, 7, 6, 2, 9, 4))
data


##########################


# create sankey plot from tutorial
Sankey <- gvisSankey(data, from="from", to="to", weight="weight",
                     options=list(
                             sankey="{link: {color: { fill: '#d799ae' } },
                            node: { color: { fill: '#a61d4c' },
                            label: { color: '#871b47' } }}"))
plot(Sankey)


#############################


# create sankey plot with some customization
sankey <- gvisSankey(data = data, from = "from", to = "to", weight = "weight",
                     options = list(width = 500, height = 300,
                                sankey = "{link: {color: {fill: '#00ff00', 
                                                stroke: '#bfbfbf', strokeWidth: 1, 
                                                fillOpacity: 0.1}},
                            node: {color: {fill: '#cc33ff'}, labelPadding: 40, width: 30, nodePadding: 80,
                                    label: {color: '#ff3300', fontName: 'Times-Roman', fontSize: 14,
                                        bold: true, italic: true} }}"))
plot(sankey)


############################


# sankey with manual link/node colors
sankey <- gvisSankey(data = data, from = "from", to = "to", weight = "weight",
                  options = list(width = 500, height = 300,
                        sankey = "{link: {color: {stroke: '#bfbfbf', strokeWidth: 1, fillOpacity: 0.1}, 
                                        colorMode: 'source', colors: ['#996633', '#6600cc']},
                                node: {labelPadding: 40, width: 30, nodePadding: 80,
                                        colors: ['#00cc99', '#00cc00', '#009900', '#99cc00', '#009999'],
                                        label: {color: '#ff3300', fontName: 'Times-Roman', fontSize: 14,
                                                bold: true, italic: true} }}"))
plot(sankey)


#################################################################
#################################################################


# sankey with multiple x-axis sets

# create data
data_2 <- tibble(from = c(rep("A", 3), rep("B", 3), rep("X", 3), rep("Y", 3), rep("Z", 3)), 
                 to = c(rep(c("X", "Y", "Z"), 2), rep(c("M", "N", "O"), 3)), 
                 weight = c(5, 7, 6, 2, 9, 4, 10, 3, 5, 8, 1, 26, 49, 104, 55))
data_2

# create sankey plot for data_2 with some customization 
sankey <- gvisSankey(data = data_2, from = "from", to = "to", weight = "weight",
                     options = list(width = 500, height = 300,
                                    sankey = "{link: {color: {fill: '#00ff00', 
                                                stroke: '#bfbfbf', strokeWidth: 1, 
                                                fillOpacity: 0.1}},
                            node: {color: {fill: '#cc33ff'}, labelPadding: 40, width: 30, nodePadding: 80,
                                    label: {color: '#ff3300', fontName: 'Times-Roman', fontSize: 14,
                                        bold: true, italic: true} }}"))
plot(sankey)


#################################################################


# create sankey plot for data_2 with manual colors 8/5
sankey <- gvisSankey(data = data_2, from = "from", to = "to", weight = "weight",
                     options = list(width = 500, height = 300,
                                    sankey = "{link: {color: {stroke: '#bfbfbf', strokeWidth: 1, fillOpacity: 0.1}, 
                                        colorMode: 'source', 
                                        colors: ['#996633', '#6600cc', '#ffff00', '#0099ff', '#ff9933']},
                                    node: {labelPadding: 40, width: 30, nodePadding: 80,
                                        colors: ['#00cc99', '#00cc00', '#009900', '#99cc00', '#009999',
                                                        '#ff0066', '#ff33cc', '#9966ff'],
                                        label: {color: '#ff3300', fontName: 'Times-Roman', fontSize: 14,
                                                bold: true, italic: true} }}"))
plot(sankey)


##################################################################


# save sankey plot

# get html_file
html_file <- plot(sankey)
html_file
str(html_file)

# use webshot to pull plot from html and save as pdf
# see save_flextable.R, which wraps these steps into a function, 
# could be easily updated to make save_gvis function
# note the plot is incredibly small though, so would need to manually zoom in with magick

# get id_selector for sankey chart
id_selector <- str_split(string = html_file, pattern = "/") %>% enframe() %>% unnest() %>% 
        tail(1) %>% mutate(value = str_c("#", str_replace(string = value, pattern = ".html", replacement = ""))) %>%
        pull(value)
id_selector

# note that oddly the larger the zoom value, the smaller the chart
webshot(url = html_file, zoom = .3, delay = 1, file = "sankey_plot.pdf", selector = id_selector)

# load pdf
sankey_pdf <- pdf_render_page(pdf = "sankey_plot.pdf", page = 1, dpi = 300)

# convert pdf to magick image
sankey_image <- image_read(sankey_pdf)

# crop image to remove footnotes
image_info <- as_tibble(image_info(sankey_image))
sankey_image <- image_crop(image = sankey_image, 
           geometry = geometry_area(width = image_info %>% pull(width), 
                                    height = image_info %>% pull(height) - 75, 
                                    x_off = 0, y_off = 0))
sankey_image

# save image as png
image_write(image = sankey_image, path = "sankey_image.png", format = 'png', density = '300x300')










