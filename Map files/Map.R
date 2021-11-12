if (!require(c("tidyverse","sf","tmap","tigris","rappdirs","shinyjs","RColorBrewer","raster","biscale","cowplot","rgeos","maps","cartography"))) 
  install.packages(c("tidyverse","sf","tmap","tigris","rappdirs","shinyjs","RColorBrewer","raster","biscale","cowplot","rgeos","maps","cartography"))

library(tidyverse)
library(sf)
library(tmap)
library(tigris)
library(rappdirs)
library(shinyjs)
library(RColorBrewer)
library(raster)
library(biscale)
library(cowplot)
library(rgeos)
library(maps)
library(cartography)

# load data set, poverty data and shape files with city boundaries
df <- read.csv(file = "PropertyStats.csv", header = TRUE) #16680
df$NormalizedBaselineConsumption <- df$BaselineConsumption/df$size
# treated
treatDF <- subset(df, df$Group ==1) #549

povDF <- read_csv('pov.csv')
names(povDF)[1] <- "GEOID"
povDF$GEOID <- as.factor(povDF$GEOID)
table(povDF$GEOID)

tr_albany <- st_read("Intersect_of_tracts_and_CityBoundary___CityBoundaries.shp") %>% 
  st_as_sf() %>% st_transform(crs = 4326)
op_zones <- st_read('Federal_Opportunity_Zones.shp') %>% 
  st_as_sf() %>% st_transform(crs = 4326)

op_zones.clipped <- op_zones %>%
  st_join(tr_albany) 

# CSV TO SF using coordinates
df.sf <- df %>% st_as_sf(coords = c("Longitude", "Latitude"), 
                         dim = "XY", 
                         crs = 4326)

treatDF.sf  <- treatDF %>% st_as_sf(coords = c("Longitude", "Latitude"), 
                                    dim = "XY", 
                                    crs = 4326)

# Grouping data by tract groups in Albany
df.in.tr <- tr_albany %>%
  st_join(df.sf) 

# Taking average of all numeric columns in each block group
df.in.tr.sumr <- df.in.tr %>%
  group_by(GEOID) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

# Join poverty data with Albany census tract shapefile 
biv.sf <- merge(df.in.tr.sumr,povDF,by='GEOID')
biv.sf <- biv.sf[!is.na(biv.sf$NormalizedBaselineConsumption),]

# Creating classes
data <- bi_class(biv.sf, x = Poverty_csv_Percent_below_pover, y = NormalizedBaselineConsumption, style= 'quantile', dim=3)

# Mapping
map <- ggplot() + 
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  geom_point(data = treatDF, aes(x = Longitude, y = Latitude),fill='black',col="grey58",pch=21,size=3) +
  bi_scale_fill(pal = 'GrPink', dim = 3) + 
  labs(title = "Housing Participants' Energy Consumption and Poverty Level") +
  bi_theme(axis.title.x=element_blank(), axis.title.y=element_blank())

legend <- bi_legend(pal = 'GrPink', 
                    dim = 3, 
                    xlab = 'Higher % Poverty', 
                    ylab = 'Higher Energy Consumption',
                    size=7) 


plot <- ggdraw() + draw_plot(map,0,0,1,1)+
  draw_plot(legend,0.7,0.1,0.2,0.2)

plot

hatchedLayer(op_zones.clipped,"right2left", density = 2, lwd = 0.3, col = "grey70")
plot.new()
hatch_leg <- legendHatched(cex = 1.5,
                           values.cex = 0.8,
                           title.txt = "Legend",
                           categ = c("Opportunity Zone"),
                           patterns = c("right2left"), density = 2,
                           col = "grey70",
                           ptrn.bg = "white")
