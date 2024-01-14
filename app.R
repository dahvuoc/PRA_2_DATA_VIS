
library(shinydashboard)
library(tidyverse)
library(readxl)
library(janitor)
library(hrbrthemes)
library(plotly)

setwd("/Users/dahv10/Desktop/Shiny Course/shiny_pra_uoc")

datos_credit <- read_excel("data.xlsx") %>% 
  clean_names() %>% 
  mutate(creditos_existentes = as.integer(creditos_existentes),
         balance_de_la_cuenta_cte = factor(balance_de_la_cuenta_cte, levels = c("no checking", "<0", "0<=X<200", ">=200"), ordered = TRUE),
         balance_de_ahorros = factor(balance_de_ahorros, levels = c("no known savings", "<100", "100<=X<500", "500<=X<1000", ">=1000"), ordered = TRUE))

clientes <- nrow(datos_credit)
clientes_buenos <- datos_credit %>% filter(cliente == "good") %>% nrow()
clientes_malos <- datos_credit %>% filter(cliente == "bad") %>% nrow()

header <- dashboardHeader()

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Create two `menuItem()`s, "Dashboard" and "Inputs"
    menuItem(text = "Dashboard",
             tabName = "dashboard"),
    menuItem(text = "Filtros",
             tabName = "filtros",
             selectInput(inputId = "sexo",
                         label = "Sexo",
                         choices = c("Todos", levels(as.factor(datos_credit$sexo)))),
             selectInput(inputId = "tiempo_residencia",
                         label = "Tiempo de Residencia",
                         choices = c("Todos", levels(as.factor(datos_credit$tiempo_de_residencia)))),
             selectInput(inputId = "numero_dependientes",
                         label = "Número de Dependientes",
                         choices = c("Todos", levels(as.factor(datos_credit$numero_de_dependientes)))),
             selectInput(inputId = "extranjero",
                         label = "Extranjeros?",
                         choices = c("Todos",levels(as.factor(datos_credit$extranjero)))),
             sliderInput(inputId = "edad",
                         label = "edad",
                         min = min(datos_credit$edad, na.rm = TRUE),
                         max = max(datos_credit$edad, na.rm = TRUE),
                         value = c(19, 75)),
             sliderInput(inputId = "monto",
                         label = "Monto del Crédito",
                         min = min(datos_credit$monto_del_credito, na.rm = TRUE),
                         max = max(datos_credit$monto_del_credito, na.rm = TRUE),
                         value = c(250, 18424))
             )
    )
  )

body <- dashboardBody(
  fluidRow(
    valueBox(clientes,
             "Total Clientes",
             icon(name = "person")),
    # Row 1, Column 2
    valueBox(clientes_buenos,
             "Total Clientes Buenos",
             icon(name = "circle-check")),
    # Row 1, Column 3
    valueBox(clientes_malos,
             "Total Clientes Malos",
             icon(name = "circle-xmark"))
    ),
  fluidRow(
    box(
      width = 6,
      title = "Histograma - Monto del Crédito | Historial de Crédito",
      plotlyOutput("plot1")),
    box(
      width = 6,
      title = "Gráfico de Columnas - Propósito | Sexo",
      plotlyOutput("plot2")),
    box(
      width = 6,
      title = "Violín - Edad | Tipo Cliente",
      plotlyOutput("plot3")),
    box(
      width = 6,
      title = "Relación: Antiguedad | Monto del Crédito",
      plotlyOutput("plot4"))
    ),
  fluidRow(
    box(
      width = 12,
      title = "4",
      DT::dataTableOutput("table")
      )
    )
  )
  

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- datos_credit
    data <- subset(data,
                   edad >= input$edad[1] & edad <= input$edad[2])
    
    data <- subset(data,
                   monto_del_credito >= input$monto[1] & monto_del_credito <= input$monto[2])
    
    if(input$sexo != "Todos") {
      data <- subset(data,
                     sexo %in% input$sexo
      )
    }
    
    if(input$tiempo_residencia != "Todos") {
      data <- subset(data,
                     tiempo_de_residencia == input$tiempo_residencia
      )
    }
    
    if(input$numero_dependientes != "Todos") {
      data <- subset(data,
                     numero_de_dependientes == input$numero_dependientes
      )
    }
    
    if(input$extranjero != "Todos") {
      data <- subset(data,
                     extranjero == input$extranjero
      )
    }
    
    data
  })
  
  output$table <- DT::renderDataTable({
    
    data <- filtered_data() %>% 
      select(extranjero, trabajo, residencia, creditos_existentes, cliente) %>% 
      rename(Extranjero = extranjero,
             Trabajo = trabajo,
             Residencia = residencia,
             Créditos = creditos_existentes,
             Cliente = cliente)
    data
  })
  
  output$plot1 <- renderPlotly({
    data <- filtered_data()
    
    ggplotly(ggplot(data, aes(x=monto_del_credito, fill = historial_de_credito)) +
      geom_histogram(alpha = 0.65) +
      facet_wrap(~historial_de_credito) +
      theme_ipsum() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1)))
  })
  
  output$plot2 <- renderPlotly({
    data <- filtered_data()
    
    grouped_data <- data %>% 
      group_by(sexo, proposito) %>% 
      summarise(registros = n()) %>% 
      arrange(sexo, desc(registros)) %>% 
      mutate(proposito = case_when(proposito %in% c("radio/tv", "domestic appliance") ~ "Electronics",
                                   proposito == "education" ~ "Education",
                                   proposito %in% c("furniture/equipment", "business") ~ "Business",
                                   proposito %in% c("new car", "used car") ~ "Car",
                                   proposito %in% c("repairs", "other", "retraining") ~ "Other"))
    
    ggplotly(ggplot(grouped_data, aes(x=proposito, y = registros, fill = proposito)) +
               geom_col(alpha = 0.65) +
               facet_wrap(~sexo) +
               theme_ipsum() +
               theme(legend.position = "none",
                     axis.text.x = element_text(angle = 45, hjust = 1)))
  })
  
  output$plot3 <- renderPlotly({
    data <- filtered_data()
    
    ggplotly(ggplot(data, aes(x=cliente, y = edad, fill = extranjero)) +
               geom_violin(alpha = 0.65) +
               facet_wrap(~extranjero) +
               theme_ipsum() +
               theme(legend.position = "none"))
  })
  
  output$plot4 <- renderPlotly({
    data <- filtered_data()
    
    ggplotly(ggplot(data, aes(x=antiguedad, y = monto_del_credito, fill = trabajo, size = compromisos_de_pagoa_a_plazos_percent)) +
               geom_point(alpha = 0.65) +
               theme_ipsum() +
               theme(legend.position = "none"))
  })
  
  output$card1 <- renderValueBox({
    data <- filtered_data()
    
    nrow(data)
    
  })
  
  }
  
shinyApp(ui, server)