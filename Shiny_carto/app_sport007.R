library(shiny)
library(tidyverse)
library(sf)
library(mapsf)
library(RColorBrewer)

don <- readRDS("sport/licpop_idf_2018.RDS") %>% mutate(pop=as.numeric(pop))

# selection de la zone d'étude et des sports
sel <- don %>% filter(substr(code_com,1,2) %in% c("75","92","93","94")) %>%
  mutate(sport= case_when(code_fed=="111"~ "Football",
                          code_fed=="123"~ "Tennis",
                          code_fed=="132"~ "Golf",
                          code_fed=="119"~ "Natation",
                          code_fed=="117"~ "Judo",    
                          code_fed=="113"~ "Gymnastique",
                          code_fed=="219"~ "Danse",  
                          code_fed=="109"~ "Equitation",
                          code_fed=="115"~ "Handball", 
                          code_fed=="133"~ "Rugby",
                          code_fed=="101"~ "Athlétisme", 
                          TRUE ~ "Other"))


# Groupement par commune
spo <- sel %>% group_by(code_com,sport,sexe) %>%
  summarise(spo=sum(nblic)) 
tot<-sel %>% group_by(code_com) %>%
  summarise(tot=sum(nblic)) 
tab<-left_join(spo,tot) %>% mutate(pct=100*spo/tot) %>% as.data.frame()

# Fonds de carte
map<-readRDS("sport/map_com_idf.RDS") 


# Définition UI et Server de l'application Shiny
ui <- fluidPage(
    # Titre de l'application
  titlePanel("Les sports dans le Grand Paris en 2017-2018"),
    
    # Définition du Widget - ici un slider en vue de construire un histogramme
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "variable",
                      label = "Choix de l'indicateur",
                      choices = c("Football" = "Football",
                                  "Golf" = "Golf",
                                  "Tennis" = "Tennis",
                                  "Natation" = "Natation",
                                  "Judo"="Judo",
                                  "Gymnastique" = "Gymnastique",
                                  "Danse" = "Danse",
                                  "Equitation" = "Equitation",
                                  "Handball" = "Handball",
                                  "Rugby" = "Rugby",
                                  "Athlétisme" = "Athlétisme"
                      ),
                      selected = "Football"
          ),                 
            
            selectInput(inputId = "Sexe",
                        label = "Sexe",
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
          plotOutput("mapPlot",height = "400px"),
          plotOutput("histPlot", height = "300px")
        )
    )
)

server <- function(input, 
                   output) {
    output$histPlot <- renderPlot({
        
       com<-tab %>% filter(sport==input$variable, sexe==input$Sexe)
       x<-com$pct
       mybreaks<-mf_get_breaks(x, nbreaks= input$classes, breaks=input$methode)
       mypalette<-brewer.pal(name = input$palette,n = input$classes)
       hist(x, 
            breaks=mybreaks,
            probability=TRUE,
            col=mypalette,
            xlab= "% des licences",
            ylab = "Densité de probabilité",
            main= paste("Les sports dans le Grand Paris : ", input$variable),
            sub = "Source : Ministère de la jeunesse et des sports")
       mybw<-2*sd(x,na.rm=T)/input$classes
       lines(density(x,bw=mybw,na.rm=T),col="red",lwd=2)
    })
    
    output$mapPlot <-renderPlot({
      com<-tab %>% filter(sport==input$variable, sexe==input$Sexe)
      mapcom<-merge(map,com,by.x="insee_com",by.y="code_com")
      x<-mapcom$pct
      mybreaks<-mf_get_breaks(x, nbreaks= input$classes, breaks=input$methode)
      mypalette<-brewer.pal(name = input$palette,n = input$classes)
      mf_map(mapcom, 
             var = "pct",
             type = "choro",
             breaks = mybreaks,
             pal=mypalette)
    })
    
}

shinyApp(ui = ui, server = server)