#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # --- Data
    data <- reactive({mtcars})

    # --- Freq table ---
    mod_freq_table_server(id = "freq_table", data = data, verbose = FALSE)
    mod_freq_table_server(id = "freq_table_outer", data = data, verbose = FALSE)
    mod_freq_table_server(id = "freq_table_trans",
        data = data,
        transform_fn = function(x) {
            x %>%
                dplyr::mutate(foo = "bar")
        },
        verbose = FALSE
    )
    mod_freq_table_server(id = "freq_table_bundles",
        data = data,
        # dt_bundles_additional = list(
        #     dtf::dt_bundle_searchpanes()
        # ),
        dt.filter = dtf:::valid_dt_filter_values("top"),
        verbose = FALSE
    )
    mod_freq_table2_server(id = "freq_table_selectize", data = data)

    # --- Select ---
    mod_select_server(data = data, verbose = FALSE)
    mod_select_server(id = "select_outer", data = data, fn_select_ui = create_select_ui, verbose = FALSE)
    mod_select2_server(id = "selectize", data = data,
        fn_select_ui = create_selectize_ui, render_data = TRUE)

    # --- Table: DT ---
    mod_table_dt_server(data = data, verbose = TRUE)
    mod_table_dt_server(id = "table_dt_box", data = data, verbose = FALSE)
    mod_foo_server(data = data, verbose = FALSE)
    mod_table_dt_server(id = "table_dt_bundles", data = data,
        .bundles = list(
            dtf::dt_bundle_searchpanes()
        ),
        verbose = FALSE
    )
    mod_table_dt_server(id = "table_dt_filter", data = data,
        filter = dtf::valid_dt_filter_values("top"), server = TRUE, verbose = FALSE)
}
