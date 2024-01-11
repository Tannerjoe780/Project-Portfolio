library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(readxl)
 group_grades <- read_excel("C:/Users/joeyt/Downloads/Group grades.xlsx")

 ui <- page_sidebar(
   sidebar = sidebar(
     varSelectInput("xvar", "X variable", group_grades, selected = "MathScore"),
     varSelectInput("yvar", "Y variable", group_grades, selected = "ReadingScore"),
     checkboxGroupInput(
       "ethnic_group", "Filter by Ethnic Group",
       choices = unique(group_grades$EthnicGroup),
       selected = unique(group_grades$EthnicGroup)
     ),
     hr(), # Add a horizontal rule
     checkboxInput("by_ethnic_group", "Show Ethnic Group", TRUE),
     checkboxInput("show_margins", "Show marginal plots", TRUE),
     checkboxInput("smooth", "Add smoother"),
   ),
   plotOutput("scatter")
 )
 
 server <- function(input, output, session) {
   subsetted <- reactive({
     req(input$ethnic_group)
     group_grades %>%
       filter(EthnicGroup %in% input$ethnic_group)
   })
   
   output$scatter <- renderPlot({
     p <- ggplot(subsetted(), aes_string(x = input$xvar, y = input$yvar)) +
       theme(legend.position = "bottom")
     
     if (input$by_ethnic_group) {
       p <- p + aes(color = EthnicGroup) + geom_point()
     }
     
     if (input$smooth) {
       p <- p + geom_smooth()
     }
     
     if (input$show_margins) {
       margin_type <- if (input$by_ethnic_group) "density" else "histogram"
       p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
                                size = 8, groupColour = input$by_ethnic_group, groupFill = input$by_ethnic_group)
     }
     
     p
   }, res = 100)
 }
 
 shinyApp(ui, server)