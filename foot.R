library(sf)
library(tmap)
library(writexl)
map<-st_read("data/foot/res_equipements_2017.shp")
sel<-map %>% filter(depcode %in% c("75","92","94","93", "78","77","91","95"))
sel$ter<-as.factor(sel$naturesolli=="Gazon naturel")
levels(sel$ter)<-c("red","green")
sel$ter<-as.character(sel$ter)
plot(sel$geometry, col=sel$ter, pch=20 )

tmap_mode("view")
fig<-tm_shape(sel) +
       tm_tiles("Esri.WorldImagery")+
         tm_dots(col="ter")
fig

write_xlsx(sel, "data/foot/foot_IDF.xlsx")
st_write(sel, "data/foot/foot_IDF.shp")
