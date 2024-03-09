library(data.table)
library(dplyr)
library(sf)
library(tmap)
library(mapsf)


map<-st_read("data/foot/georef-france-commune.geojson")
paris<-map %>% filter(com_name_upper =="PARIS")
map$dis<-as.numeric(st_distance(map,paris))
summary(map$dis)
map2<-map %>% filter(dis < 12000) %>% st_as_sf()
plot(map2$geometry)



x<-fread("data/foot/licence2017.csv",encoding = "UTF-8")


foot <-x %>% filter(fed_2017==111 ,region=="Île-de-France")  %>%
            filter(substr(code_commune,1,2) %in% c("75","92","93","94") | 
                     code_commune %in% c("95018","91027","91479","91432","91589","91326","91687")) %>%
        select(IDCOM=code_commune,nom=libelle,foot=l_2017, foot_h=l_h_2017, foot_f=l_f_2017, 
               pop = pop_2016,pop_h=poph_2016 ,pop_f=popf_2016) %>%
       mutate(IDCOM = as.character(IDCOM),
              foot_ind=100*(foot/pop)/(sum(foot)/sum(pop)), 
              foot_f_ind=100*(foot_f/pop_f)/(sum(foot_f)/sum(pop_f)),
              foot_h_ind=100*(foot_h/pop_h)/(sum(foot_h)/sum(pop_h)),
              foot_fh_ind=100*(foot_f/foot_h)/(sum(foot_f)/sum(foot_h)))

map<-st_read("data/foot/GrandParisMunicipalities.geojson")
map<-st_make_valid(map)
map<-left_join(map,foot)
#tmap_options(check.and.fix = T)
mf_map(map, type="choro",var="foot_ind", breaks=c(0,25,50, 75, 100, 125, 150, 175, 200, 225, 250))

plot(log(map$REVENUS),map$foot_ind)

st_write(map,"data/foot/GdParis.geojson")


