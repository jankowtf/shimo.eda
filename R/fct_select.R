reactive_select <- function(
    id = NULL,
    data,
    event_id = "submit",
    input_ids,
    input_values,
    dt_bundle_buttons = dtf::dt_bundle_buttons_en,
    dt_bundle_internationalization = dtf::dt_bundle_internationalization_en,
    ignore_null = TRUE
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Transform
        shiny::eventReactive(input[[event_id]], {
            # browser()
            data <- data()

            group_by_ids <- input_ids()
            if (length(group_by_ids)) {
                cols <- input_values() %>% unname() %>% dplyr::syms()
                if (length(cols)) {

                    # data %>%
                    #     wrang::wr_freq_table(!!!cols)
                    data %>%
                        dplyr::select(!!!cols)
                } else {
                    data
                }
            } else {
                data
            }
        }, ignoreNULL = ignore_null)
    })
}
