my_server <- function(input, output, session) {
    r_mtcars <- reactive({mtcars})

    # --- Freq table ---
    mod_freq_table_server(id = "grouping", data = r_mtcars)

    # --- Select ---
    mod_select_server(id = "test", data = r_mtcars)
}

my_server
