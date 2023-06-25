
library(shiny)
library(DT)
library(ggplot2)
library(quantmod)

movies <- read.csv("movies.csv")

ui <- fluidPage(
    tabsetPanel(tabPanel("Datos", 
                         titlePanel("Mejores Peliculas 1980 - 2020"),
                         fluidRow(column(DT::dataTableOutput("RawData"), 
                                         width = 12))
                        ),
                
                tabPanel("Categoria", 
                  titlePanel("Mejores peliculas por categoria"),
                  
                  sidebarLayout(
                  sidebarPanel("Seleccione la accion que desea consultar",
                                  selectInput(inputId = "var",
                                              label = 'Categoria',
                                              choices = c("Drama"= "Drama", "Aventura" = "Adventure", 
                                                           "Accion" = "Action", "Comedia"= "Comedy",
                                                           "Horror" = "Horror", "Biografia"= "Biography",
                                                           "Crimen" = "Crime", "Fantasia"= "Fantasy"), 
                                              multiple = FALSE, selected = "Drama" ),
                               
                               selectInput('tema', label = 'Tema', choices = c("Negro"="black", "Blanco"="white")),
                               selectInput('tipo', label = 'Tipo de grafico',
                                           choices = c("Barras"="bars", "Linea"="line"))
                                              
                              ),
                  
                    mainPanel("Grafica de mejores pelicula por categoria",
                                         h1('Grafico por puntuacion'),
                                         p('A continuacion se muestra la grafica'),
                                         
                                         plotOutput('grafico'))
                               )
                  
                              ),
                 
              tabPanel("Grafica",
                       sidebarLayout(
                           sidebarPanel("Seleccione las variables para graficar",
                                        selectInput(inputId = "varX",
                                                    label = "Seleccione la variable x:",
                                                    choices = list("genre", "year", 
                                                                "rating", "score",
                                                                "company")),
                                        
                                        selectInput(inputId = "varY",
                                                    label = "Seleccione la variable y:",
                                                    choices = list("genre", "year", 
                                                                   "rating", "score",
                                                                   "company")),
                                        
                                        ),
                           mainPanel(
                                     plotOutput('grafica'))
                           )) 
                       
    )

    ) #termina ui



server <- function(input, output) {

    output$RawData <- DT::renderDataTable(
       DT::datatable({
                   movies
                   },
                  options = list(lengthMenu = list(c(5, 10, 20), c('5', '10', '20')), papeLength = 10),
                  filter = "top", selection = 'multiple', style = 'bootstrap'))
                 
    
    output$grafico <- renderPlot({ 
        x <- movies %>% filter(movies$genre == input$var)
            ggplot(data = x, aes(year, score))
            
        })
    
    
    output$grafica <- renderPlot({
        mo <- movies[ ,c(input$varX, input$varY)]
        ggplot(data = mo, aes(x = mo[,1], y = mo[,2]))+
            geom_point()+
            labs(x = colnames(mo)[1], y = colnames(mo)[2])
             
    })
    
}


shinyApp(ui = ui, server = server)
