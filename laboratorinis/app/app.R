library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(readr)
library(shiny)
library(shiny.fluent)
library(DT)
shinyjs::useShinyjs()
library(EpiCurve)

ui = dashboardPage(skin = "green",
                    dashboardHeader(title = "Duomenu valdymas"),
                    dashboardSidebar(selectizeInput(inputId = "imones_pavadinimas", label="Imones pavadinimas",
                                                    choices= NULL, selected= NULL), 
                                     tags$head(tags$style(HTML('.shiny-server-account { display: none; }')))),
                    dashboardBody(tabsetPanel(
                      tabPanel("Vidutinio atlyginimo darbuotoju grafikas", plotOutput("plot1"),  sidebarPanel(
                        selectInput(
                          inputId = "select_avgwage",
                          label = "Parinkti atlyginimo intervala",
                          choices = c(
                            "Visi atlyginimai" = "data1_tot",
                            "8000-11000 EUR" = "data1_rdt_8000-11000",
                            "12000-15000 EUR" = "data1_rdt_12000-15000",
                            "16000+ EUR" = "data1_rdt_16000"
                          ), 
                          selected = "All",
                          multiple = FALSE)),
                      ),
                      tabPanel("Duomenu lentele", tableOutput("table"), ),
                      tabPanel("Apdraustu darbuotoju grafikas", plotOutput("plot2")),
                      
                      fluidRow(
                        column(12, downloadButton("downloadDataFromTable", "Atsisiusti duomenu faila"))
                      ),
                      fluidRow(
                        column(12, DT::dataTableOutput("campaign_table", width = "100%"))
                      ),
                      uiOutput("userpanel")
                    ),
                    dashboardBody()
      )
   )


server = function(input, output, session){
  data = read_csv("../data/lab_sodra.csv")
  data1 = data %>%
    filter(ecoActCode == 522920) %>%
    mutate(month_value=as.integer(substr(month, 5 ,7)))
  
  updateSelectizeInput(session, "imones_pavadinimas", 
                       choices = data1$name, 
                       server = TRUE)
  
  output$table = renderTable(
    data1 %>%
      filter(name == input$imones_pavadinimas), digits = 0)
  
  output$data1_epicurve = renderPlot(
      ggplot(data1, wageinterval = input$select_avgwage)
  )
  
  #
  output$userpanel <- renderUI({
    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })
  
  output$plot1 = renderPlot(
    data1 %>%
      filter(name == input$imones_pavadinimas) %>%
      ggplot(aes(x = month_value, y = avgWage)) +
      scale_x_continuous("Month",breaks=1:12,limits=c(1,12)) +
      theme_light() +
      theme(axis.text.x = element_blank()) +
      geom_point() +
      geom_line(colour = 'blue') +
      labs(x = "Month", y = "Euros")
  )
  output$plot2 = renderPlot(
    data1 %>%
      filter(name == input$imones_pavadinimas) %>%
      ggplot(aes(x = month_value, y = numInsured)) +
      scale_x_continuous("Month",breaks=1:12,limits=c(1,12)) +
      theme_light() +
      geom_point() +
      geom_line(colour = "blue") +
      labs(x = "Month", y = "Count")
  )
  
}

shinyApp(ui, server)
