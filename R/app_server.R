#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # --- Data
    r_data <- reactive({mtcars})


    # --- Table: DT ---
    mod_table_dt_server(data = r_data, verbose = TRUE)
    mod_table_dt_server(id = "table_dt_box", data = r_data, verbose = FALSE)
    mod_foo_server(data = r_data, verbose = FALSE)

    # --- Select ---
    mod_eda_select_server(r_data = r_data, verbose = FALSE)
    mod_eda_select_server(id = "select_outer", r_data = r_data, verbose = FALSE)

    # --- Freq table ---
    mod_eda_freq_table_server(id = "freq_table", r_data = r_data, verbose = FALSE)
    mod_eda_freq_table_server(id = "freq_table_outer", r_data = r_data, verbose = FALSE)
    mod_eda_freq_table_server(id = "freq_table_trans",
        r_data = r_data,
        transform_fn = function(x) {
            x %>%
                dplyr::mutate(foo = "bar")
        },
        verbose = FALSE)
}
