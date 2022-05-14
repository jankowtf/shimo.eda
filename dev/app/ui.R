my_ui <- fluidPage(
    shinyjs::useShinyjs(debug = TRUE),
    shinyWidgets::useShinydashboard(),
    # tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
    titlePanel(
        h1("Dev application", align = 'center'),
        windowTitle = "Dev application"
    ),
    mod_eda_freq_table_ui(id = "grouping"),
    mod_eda_select_ui(id = "test")
)

my_ui
