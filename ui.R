
ui <- navbarPage(
    theme = shinytheme("cerulean"),
    title = 'Module Correlation',
    mainPanel(tabPanel('Between Module Correlations',
                       dataTableOutput('bet_mod_cor'),
                       plotOutput("bet_mod_heatmap", 
                                  height = "1000px",
                                  width = "1000px")
    )
    )
)
