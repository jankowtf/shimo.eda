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
    mod_eda_select_server(
        id = NULL,
        r_data = r_data
    )

    # --- Freq table ---
    mod_eda_freq_table_server(
        id = NULL,
        r_data = r_data
    )
}
