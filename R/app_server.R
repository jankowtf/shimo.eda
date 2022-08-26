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
    # mod_freq_table2_server(id = "freq_table_selectize", data = data)
    # {
    #     data_grouped_1 <- mod_group_by_server(id = "freq_table_via_pipe_group_by", data = data,
    #         dt_ignore_null = FALSE)
    #     data_freq <- mod_freq_table3_server(id = "freq_table_via_pipe_freq_table",
    #         data = data_grouped_1, render_table = TRUE)
    # }
    mod_server_wrapper(id = "test",
        {
        data_grouped <- mod_group_by_server(id = "freq_table_via_pipe_group_by", data = data,
            dt_ignore_null = FALSE, verbose = FALSE)
        data_freq <- mod_freq_table3_server(id = "freq_table_via_pipe_freq_table",
            data = data_grouped, render_table = TRUE)
        }
    )
    mod_server_wrapper(id = "test2", {
        data_grouped <- mod_group_by2_server(id = "freq_table_via_pipe_selectize_group_by", data = data,
            dt_ignore_null = FALSE)
        data_freq <- mod_freq_table3_server(id = "freq_table_via_pipe_selectize_freq_table",
            data = data_grouped, render_table = TRUE)
    })

    # --- Select --
    mod_select_server(data = data, verbose = FALSE)
    mod_select_server(id = "select_outer", data = data, select_ui_fn = create_select_ui, verbose = FALSE)
    mod_select2_server(id = "selectize", data = data,
        select_ui_fn = create_selectize_ui, render_data = TRUE)

    mod_server_wrapper(id = "select_piped", {
        data_selected<- mod_select_server(id = "select_ui", data = data,
            render_table = FALSE)

        # dtf::mod_render_dt_server(
        #     id = NULL,
        #     output_id = "select_tbl",
        #     data = data_selected,
        #     scrollY = 300,
        #     left = 1,
        #     # .bundles = list(
        #     #     dt_bundle_buttons(),
        #     #     dt_bundle_internationalization()
        #     # )
        #     verbose = TRUE
        # )
        mod_dt_server(id = "select_tbl", data = data_selected, verbose = TRUE)
    })
    # --- Table: DT ---
    mod_table_dt_server(data = data, verbose = FALSE)
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
