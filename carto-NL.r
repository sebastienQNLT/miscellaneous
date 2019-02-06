library(ggplot2)
library(grid)
library(rworldmap)
library(dplyr)
options(stringsAsFactors = FALSE)

#lecture data ----
trad<-read.csv2(sep=";",file="https://raw.githubusercontent.com/sebastienQNLT/miscellaneous/master/Trad_Pays.csv",header=FALSE)
names(trad)<-c("PAYS","PAYS_LONG")
data<-read.csv2(file="https://raw.githubusercontent.com/sebastienQNLT/miscellaneous/master/NAYAS_PROPRE.csv",sep=";")
data.agg<-data %>% group_by(COUNTRY_3) %>% 
  summarize(ACT_PHY_3=mean(ACT_PHY_3 )) %>% 
  left_join(trad,by=c("COUNTRY_3"="PAYS"))
            
# Get the world map ----
worldMap <- getMap()

# Member States of the European Union ----
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)


# Extract longitude and latitude border's coordinates of members states of E.U. ----
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})
europeCoords <- do.call("rbind", europeCoords)

# Add data for each member ----
europeCoords$ACT_PHY_3 <- data.agg$ACT_PHY_3[match(europeCoords$region,data.agg$PAYS_LONG)]


# Plot the map
P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = ACT_PHY_3),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71))
P <- P + scale_fill_gradient(name = "ACT_PHY_3", low = "#FFFF00FF", high = "#FF0000FF", na.value = "grey50")
(P <- P + theme(panel.grid.minor = element_line(colour = NA),
  panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
)

