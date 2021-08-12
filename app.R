# Esta es una prueba de la libreria Shiny para ver como lo podemos 
# emplear en el ejercicio de la materia Recoleccion de Datos y Analisis
# Primario de la Informacion UNAB Ciencia de Datos 2020 1er Cuatrimestre

# Cargamos las librerias necesarias
library(shiny)
library(dplyr)
library(ggplot2)

# cargo el dataset
suaci_ori <- read.csv("sistema-unico-de-atencion-ciudadana-2020.csv")

# lo modificamos para que figure el mes como nueva columna
suaci <- mutate(suaci_ori, mes = substr(periodo, 5, 6))

# obtenemos los valores unicos de canal, categoria y barrio
# Y los ordenamos 
canal     <- arrange(distinct(suaci, canal), canal)
categoria <- arrange(distinct(suaci, categoria), categoria)
barrios   <- arrange(distinct(suaci, domicilio_barrio), domicilio_barrio)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SUACI"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            selectInput("canal", 
                        "Canal de Contacto", 
                        canal,
                        selected = NULL, 
                        multiple = FALSE),
            selectInput("categoria", 
                        "Categoria", 
                        categoria,
                        selected = NULL, 
                        multiple = FALSE),
            selectInput("barrio", 
                        "Barrio", 
                        barrios,
                        selected = NULL, 
                        multiple = FALSE),
        br(),
        br(),
        br(),
        br(),
        #img(src = "logo-unab-2.png", height = 70, width = 200),
        br(),
        "Agustin Hualde",br(), "Raul Marusca",br(), "Pablo Moreira",
        br(),
        span("R - RStudio - Shiny", style = "color:blue"),
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Histograma", plotOutput("distPlot")),
                tabPanel("Grafico de Torta", verbatimTextOutput("summary")),
                tabPanel("Tabla", tableOutput("table"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        flt_suaci <- filter(suaci,canal==input$canal, categoria==input$categoria, domicilio_barrio==input$barrio)
        grp_suaci <- group_by(flt_suaci, mes)
        smr_suaci <- summarise(grp_suaci, total=n())
        p <- ggplot(smr_suaci, aes(x=mes)) + 
            geom_bar(aes(x = mes, weight = total))
        print(p)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
