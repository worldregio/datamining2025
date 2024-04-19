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
map<-st_transform(map, 2154)

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

## Extraction des classes d'âges
ages<-unique(don$age)
ages<-c("Total", ages)
ages<-as.list(ages)
names(ages)<-c("Total","0-4 ans", "5-9 ans","10-14 ans","15-19 ans", "20-29 ans","30-44 ans","45-59 ans", "60-74 ans", "75 ans et +")


## Extraction des classes de genre
sexes<-unique(don$sexe)
sexes<-c("Total", sexes)
sexes<-as.list(sexes)
sexes
names(sexes)<-sexes

# Définition UI et Server de l'application Shiny
ui <- fluidPage(
    # Titre de l'application
  titlePanel("Distribution par âge et par sexe des adhérents des fédérations sportives des communes de Paris et Petite Couronne en 2017-2018"),
    
    # Définition du Widget - ici un slider en vue de construire un histogramme
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "variable",
                        label = "Fédération sportive",
                        choices = vars,
                        selected = "FF de football"
            ),
            
            selectInput(inputId = "Sexe",
                        label = "Genre",
                        choices = sexes,
                        selected = "Total"
            ),
            
            selectInput(inputId = "Age",
                        label = "Age",
                        choices = ages,
                        selected = "Total"
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
            h5("Histogramme"),
            plotOutput("histPlot", height="200px")
            
            
        ),
        

        
        
        # Graphe montré à l'utilisateur
        mainPanel(
          plotOutput("mapPlot",height = "750px"),
      #    plotOutput("histPlot")
        )
    )
)

server <- function(input, 
                   output) {
  

    com <- reactive({
      df<-sel
      if(input$Sexe!="Total") {df<- sel %>% filter(sexe == input$Sexe)}
      if(input$Age!="Total") {df<- df %>% filter(age == input$Age)} 
      tot<-df %>% group_by(code_com) %>%
        summarise(tot=sum(nblic)) 
      spo <- df %>% filter(nom_fed==input$variable) %>% group_by(code_com) %>%
        summarise(spo=sum(nblic)) 
      com<-left_join(spo,tot) %>% mutate(pct=100*spo/tot) %>% as.data.frame()
      return(com)
    })
  
  
  
  
    output$histPlot <- renderPlot({
        
    #  com<-tab %>% filter(sport==input$variable, sexe==input$Sexe)
    #   com<-tab %>% filter(sport=="FF de football")
       x<-com()$pct
       mybreaks<-mf_get_breaks(x, nbreaks= input$classes, breaks=input$methode)
       mypalette<-brewer.pal(name = input$palette,n = input$classes)
       par(mar=c(4,4,2,0))
       hist(x, 
            breaks=mybreaks,
            probability=TRUE,
            col=mypalette,
            xlab= "% des licences",
            ylab = "prob",
            xlim = c(0, max(x)),
            main= NA,
            sub = input$choices)
       mybw<-2*sd(x,na.rm=T)/input$classes
       lines(density(x,bw=mybw,na.rm=T),col="red",lwd=2)
    })
    
    output$mapPlot <-renderPlot({
   #   com<-tab %>% filter(sport==input$variable, sexe==input$Sexe)
      mapcom<-merge(map,com(),by.x="insee_com",by.y="code_com",all.x=T,all.y=F)
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
             leg_pos = "topleft",
             leg_title_cex = 1,
             leg_val_cex = 1,
             leg_val_rnd = 1,
             leg_no_data = "Aucune licence",
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
 mapcom2 <- mapcom %>% filter(is.na(spo)==FALSE, spo>0)     
      mf_map(mapcom2, 
             leg_title = "nb. de licences",
             leg_title_cex = 1,
             leg_val_cex = 1,
             var = "spo",
             type = "prop",
             col="black",
             border="white",
             inches=0.1,
             leg_pos = "topright",
             add=TRUE)
      titre <-paste0(input$variable," : ", sum(com()$spo)," Licences (",round(100*sum(com()$spo)/sum(com()$tot),1),"%)")
      mf_layout(title = titre, 
                credits = "Source: Ministère de la Jeunesse et des Sports, saison 2017-2018",
                frame=T, arrow=F)
    })
    
  
    
}

shinyApp(ui = ui, server = server)