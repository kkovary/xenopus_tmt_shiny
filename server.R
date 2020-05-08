


shinyServer(function(input, output) {

    output$bet_mod_cor <- DT::renderDataTable({
        bet_mod_cor2 %>% 
            dplyr::select(-uniprot_a, -uniprot_b) %>% 
        DT::datatable(selection = 'single',
                      options = list(buttons = c('csv', 'excel'))
        )
    })
    
    output$bet_mod_heatmap <- renderPlot({
        mod_cor_heatmap(input$bet_mod_cor_rows_selected)
    })

})
