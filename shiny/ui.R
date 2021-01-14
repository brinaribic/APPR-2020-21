library(shiny)

fluidPage(
  selectInput(inputId = "regija",
              label = "Izberi regijo",
              choices = unique(place_SLO$Regija), 
              selected=unique(place_SLO$Regija)[1]),
  radioButtons(inputId = "spol", 
              label = "Izberi spol",
              choices = unique(place_SLO$Spol), 
              selected = unique(place_SLO$Spol)[1]),
  plotOutput("starosti")
)


  