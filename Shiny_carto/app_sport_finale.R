library(shiny)
library(tidyverse)
library(sf)
library(mapsf)
library(RColorBrewer)


don <- readRDS("sport/licpop_idf_2018.RDS") %>% mutate(pop=as.numeric(pop))

# selection de la zone d'étude et des sports
sel <- don %>% filter(substr(code_com,1,2) %in% c("75","92","93","94")) %>%
  mutate(sport = nom_fed)

# Groupement par commune
spo <- sel %>% group_by(code_com,sport,sexe) %>%
  summarise(spo=sum(nblic)) 
tot<-sel %>% group_by(code_com) %>%
  summarise(tot=sum(nblic)) 
tab<-left_join(spo,tot) %>% mutate(pct=100*spo/tot) %>% as.data.frame()

# Fonds de carte
map<-readRDS("sport/map_com_idf.RDS") %>% filter(code_dept %in% c("75","92","93","94")) 

mapdep<-map %>% group_by(code_dept) %>% summarise() %>% st_as_sf()

## Extraction des noms de sports
vars<-unique(tab$sport)
short<-vars
short<-gsub("FF d'","",short)
short<-gsub("FF des ","",short)
short<-gsub("FF de la ","",short)
short<-gsub("FF de ","",short)
short<-gsub("FF du ","",short)
short<-gsub("F nationale du ","",short)
short<-gsub("Fédération nautique de ","",short)
short<-gsub("Fédération française des ","",short)
short<-gsub("Union française des \u009cuvres laïques d'éducation physique (UFOLEP)","UFOLEP",short)
vars<-as.list(vars)
names(vars)<-short
vars<-vars[order(names(vars))]


# Définition UI et Server de l'application Shiny
ui <- fluidPage(
    # Titre de l'application
  titlePanel("Les sports dans le Grand Paris en 2017-2018"),
    
    # Définition du Widget - ici un slider en vue de construire un histogramme
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "variable",
                        label = "Choix de la fédération sportive",
                        choices = vars,
                        selected = "FF de football"
            ),
            
            selectInput(inputId = "Sexe",
                        label = "Choix du genre",
                        choices = c("Homme" = "Homme",
                                    "Femme" = "Femme"),
                        selected = "Homme"
            ),
            
            
            sliderInput(inputId = "classes",
                        label = "Nombres de classes",
                        min = 1,
                        max = 10,
                        value = 5),
            
            selectInput(inputId = "methode",
                        label = "Type de classes",
                        choices = c("Effectifs égaux" = "quantile",
                                    "Amplitudes égales" = "equal",
                                    "Jenks" = "jenks"),
                        selected = "quantile"),
            
            selectInput(inputId = "palette",
                        label = "Couleurs",
                        choices = c("Oranges" = "Oranges",
                                    "Bleus" = "Blues",
                                    "Verts" = "Greens",
                                    "Rouges" = "Reds",
                                    "Gris" = "Greys",
                                    "Spectral"= "Spectral"),
                        selected = "Oranges"),
            
            
        ),
        

        
        
        # Graphe montré à l'utilisateur
        mainPanel(
          plotOutput("mapPlot"),
          plotOutput("histPlot")
        )
    )
)

server <- function(input, 
                   output) {
    output$histPlot <- renderPlot({
        
      com<-tab %>% filter(sport==input$variable, sexe==input$Sexe)
    #   com<-tab %>% filter(sport=="FF de football")
       x<-com$pct
       mybreaks<-mf_get_breaks(x, nbreaks= input$classes, breaks=input$methode)
       mypalette<-brewer.pal(name = input$palette,n = input$classes)
       hist(x, 
            breaks=mybreaks,
            probability=TRUE,
            col=mypalette,
            xlab= "% des licences",
            ylab = "Densité de probabilité",
            main= paste("Part des licenciés par commune"),
            sub = "Source : Ministère de la jeunesse et des sports")
       mybw<-2*sd(x,na.rm=T)/input$classes
       lines(density(x,bw=mybw,na.rm=T),col="red",lwd=2)
    })
    
    output$mapPlot <-renderPlot({
      com<-tab %>% filter(sport==input$variable, sexe==input$Sexe)
      mapcom<-merge(map,com,by.x="insee_com",by.y="code_com",all.x=T,all.y=F)
      x<-mapcom$pct
      mybreaks<-mf_get_breaks(x, nbreaks= input$classes, breaks=input$methode)
      mypalette<-brewer.pal(name = input$palette,n = input$classes)
      # Initiate a base map
      mf_init(x = mapcom,theme = "candy")
      # Plot a shadow
      mf_shadow(mapcom, add = TRUE)
      # plot municipalities 
      mf_map(mapcom, 
             leg_title = "% des licences",
             leg_pos = "left",
             var = "pct",
             type = "choro",
             breaks = mybreaks,
             pal=mypalette,
             col_na = "gray90",
             lwd=0.5,
             border="white",
             add=TRUE
             )
      mf_map(mapdep, 
             type="base",
             col = NA,
             border="black",
             add=TRUE,
             lwd=2)
      
      mf_map(mapcom, 
             leg_title = "nb. de licences",
             var = "spo",
             type = "prop",
             col="black",
             border="white",
             inches=0.1,
             leg_pos = "topleft",
             add=TRUE)
      mf_layout(title = input$variable, 
                credits = "Source: Ministère de la Jeunesse et des Sports, saison 2017-2018",
                frame=T, arrow=F)
    })
    
}

shinyApp(ui = ui, server = server)