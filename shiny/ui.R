library(shiny)
library(markdown)

fluidPage(

sidebarPanel(
              selectInput(inputId = "drzava",
                          label = "Izberi državo",
                          choices = unique(place_Evropa$Drzava),
                          selected = unique(place_Evropa$Drzava)[13]),
              verbatimTextOutput("povzetek")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Tabela",
                    DT::dataTableOutput("table")),
               tabPanel("Plače in BDP",
                        plotOutput("place"),
                        plotOutput("bdp"))
                    )
           
           )
)