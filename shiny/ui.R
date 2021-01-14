library(shiny)

fluidPage(
  selectInput(inputId = "regija",
              label = "Izberi regijo",
              choices = unique(place_SLO$Regija), selected=unique(place_SLO$Regija)[1]),
  plotOutput("starosti")
)


  