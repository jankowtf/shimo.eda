my_server <- function(input, output, session) {
    r_mtcars <- reactive({mtcars})

    mod_eda_freq_table_server(
        id = NULL,
        r_data = r_mtcars
    )
}

my_server
