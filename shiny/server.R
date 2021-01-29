library(shiny)

function(input, output) {
   output$place <- renderPlot({
     ggplot(place_Evropa %>%
              filter(Drzava == input$drzava)
            ) +
       aes(x=Leto, y=Placa/1000) + 
       geom_point() +
       geom_line() +
       ylab("Plača * 1000 (v EUR)") +
       scale_x_continuous(breaks=2008:2018) +
       theme_bw()
   })
   output$bdp <- renderPlot({
      ggplot(bdp_Evropa %>%
                filter(Drzava == input$drzava)
      ) +
         aes(x=Leto, y=BDP/1000) + 
         geom_point() +
         geom_line() +
         ylab("BDP * 10000 (v mio EUR)") +
         scale_x_continuous(breaks=2008:2018) +
         theme_bw()
   })
   output$table <- DT::renderDataTable({
      DT::datatable(place.bdp %>% filter(Drzava == input$drzava),options = list(dom = 't'))
   })
   output$povzetek <- renderPrint({
      summary(place.bdp %>%
                 filter(Drzava == input$drzava) %>% .[-c(1,2)])
   })
}


