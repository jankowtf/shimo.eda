#' Reactive select
#'
#' @param id
#' @param data
#' @param event_id
#' @param input_ids
#' @param input_values
#' @param dt_bundle_buttons
#' @param dt_bundle_internationalization
#' @param ignore_null
#'
#' @return
#' @export
#'
#' @examples
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
                    #     wrang::summa_freq_table(!!!cols)
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

reactive_group_by <- function(
    id = NULL,
    data,
    input_ids,
    input_values,
    event_id = "submit",
    ignore_null = TRUE
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        eventReactive(input[[event_id]], {
            data <- data()

            input_ids <- input_ids()
            if (length(input_ids)) {
                cols <- input_values() %>% unname() %>% dplyr::syms()
                if (length(cols)) {
                    data %>% dplyr::group_by(!!!cols)
                } else {
                    data
                }
            } else {
                data
            }
        }, ignoreNULL = ignore_null)
    })
}

reactive_freq_table_all_incl <- function(
    id = NULL,
    data,
    input_ids,
    input_values,
    event_id = "submit",
    col_n_abs = "col_n_abs",
    col_n_rel = "col_n_rel",
    sort = TRUE,
    ignore_null = TRUE
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        eventReactive(input[[event_id]], {
            data <- data()
            .col_n_abs <- input[[col_n_abs]]
            .col_n_rel <-  input[[col_n_rel]]

            input_ids <- input_ids()
            if (length(input_ids)) {
                cols <- input_values() %>% unname() %>% dplyr::syms()
                if (length(cols)) {
                    data %>%
                        wrang::summa_freq_table(
                            !!!cols,
                            .col_n_abs = rlang::eval_bare(.col_n_abs),
                            .col_n_rel = rlang::eval_tidy(.col_n_rel),
                            .sort = sort
                        )
                } else {
                    data
                }
            } else {
                # data
                cols <- data %>% names()
                data %>%
                    wrang::summa_freq_table(
                        !!!cols,
                        .col_n_abs = rlang::eval_bare(.col_n_abs),
                        .col_n_rel = rlang::eval_tidy(.col_n_rel),
                        .sort = sort
                    )
            }
        }, ignoreNULL = ignore_null)
    })
}

reactive_freq_table <- function(
    id = NULL,
    data,
    col_n_abs = "col_n_abs",
    col_n_rel = "col_n_rel",
    sort = TRUE,
    ignore_null = TRUE
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        reactive({
            data <- data()
            .col_n_abs <- input[[col_n_abs]]
            .col_n_rel <-  input[[col_n_rel]]

            data %>%
                wrang::summa_freq_table(
                    .col_n_abs = rlang::eval_bare(.col_n_abs),
                    .col_n_rel = rlang::eval_tidy(.col_n_rel),
                    .sort = sort
                )
        })
    })
}
