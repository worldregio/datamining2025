library(shiny)
library(tidyverse)
library(sf)



# Chargement du tableau de données
don <- readRDS("sport/licpop_idf_2018.RDS") %>% mutate(pop=as.numeric(pop))

# selection de la zone d'étude et du sport
com <- don %>% filter(substr(code_com,1,2) %in% c("75","92","93","94")) %>%
         mutate(sport=code_fed==111,
                sportlic = sport*nblic) %>%
          group_by(code_com) %>%
             summarise(tot=sum(nblic),
                       spo=sum(sportlic)) %>%
          mutate(pct = 100*spo/tot)
         





ui <- fluidPage(
    # Titre de l'application
  titlePanel("Le Football dans le Grand Paris en 2017-2018"),
    
    # Définition du Widget - ici un slider en vue de construire un histogramme
    sidebarLayout(
        sidebarPanel(
   
            
            
            
            
            sliderInput(inputId = "Classes",
                        label = "Nombres de classes : ",
                        min = 1,
                        max = 10,
                        value = 5)
        ),
        mainPanel(
            plotOutput("histplot")
        )
    )
)

server <- function(input, 
                   output) {
    output$histplot<-renderPlot({hist(com$pct, breaks=input$Classes)})
   
}

shinyApp(ui = ui, server = server)
