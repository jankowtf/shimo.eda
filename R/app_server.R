#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # --- Data
    r_data <- reactive({mtcars})

    # --- Select ---
    mod_eda_select_server(r_data = r_data, verbose = TRUE)
    mod_eda_select_server(id = "select_outer", r_data = r_data, verbose = TRUE)

    # --- Freq table ---
    mod_eda_freq_table_server(id = "freq_table", r_data = r_data, verbose = TRUE)
    mod_eda_freq_table_server(id = "freq_table_outer", r_data = r_data, verbose = TRUE)
}
