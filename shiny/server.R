library(shiny)

function(input, output) {
  output$starosti<- renderPlot({
     ggplot(place.starost %>%
                             filter(Regija == input$regija)) + 
      aes(x=Leto, y=Placa, fill=Starost) +
      geom_col(position = "dodge") +
      labs(title = "Plače po starosti v regiji") +
      ylab("Plača (v EUR)") + 
      scale_x_continuous(breaks=2008:2018) +
      theme_bw()
  })
}


