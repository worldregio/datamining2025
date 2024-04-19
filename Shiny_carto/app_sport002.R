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


# Définition UI et Server de l'application Shiny
ui <- fluidPage(
    # Titre de l'application
    titlePanel("Le Football dans le Grand Paris en 2017-2018"),
    
    # Définition du Widget - ici un slider en vue de construire un histogramme
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "classes",
                        label = "Nombres de classes",
                        min = 1,
                        max = 10,
                        value = 5),
            
        ),
        
        
        # Graphe montré à l'utilisateur
        mainPanel(
            plotOutput("histPlot")
        )
    )
)

server <- function(input, 
                   output) {
    output$histPlot <- renderPlot({
        
       x<-com$pct
       mybreaks<-quantile(x,(0:input$classes)/input$classes,na.rm=T)
       hist(x, 
            breaks=mybreaks,
            probability=TRUE,
            col="lightyellow",
            xlab= "% des licences",
            ylab = "Densité de probabilité",
            main= "Le Football dans le Grand Paris",
            sub = "Source : Ministère de la Jeunesse et des Sports")
        mybw<-2*sd(x,na.rm=T)/input$classes
       lines(density(x,bw=mybw,na.rm=T),col="red",lwd=2)
    })
    
}

shinyApp(ui = ui, server = server)