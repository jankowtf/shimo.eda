shiny_trace_ns_ui <- function(
    fn_name,
    id_inner,
    ns,
    verbose = FALSE
) {
    if (verbose) {
        logger::log_trace("Function: {fn_name}")
        logger::log_trace("ns: {ns(character())}")
        logger::log_trace("id_inner: {id_inner}")
        logger::log_trace("ns(id_inner): {ns(id_inner)}")
    }
}

shiny_trace_ns_server <- function(
    fn_name,
    id_inner,
    verbose = FALSE,
    .id = character()
) {
    shiny::moduleServer(.id, function(input, output, session) {
        ns <- session$ns

        observe({
            if (verbose) {
                logger::log_trace("Function: {fn_name}")
                logger::log_trace("ns: {ns(character())}")

                id_inner <- if (inherits(id_inner, "reactive")) {
                    id_inner()
                } else (
                    id_inner
                )

                id_inner %>% purrr::walk(~logger::log_trace("id_inner: {.x}"))
                id_inner %>% purrr::walk(~logger::log_trace("ns(id_inner): {ns(.x)}"))
                id_inner %>% purrr::walk(~input[[.x]] %>% logger::log_eval())
            }
        })
    })
}

#' Adjust font size (in percent)
#'
#' @param value [[character]] Text value
#' @param size [[integer]] Font siz in percent
#'
#' @return [shiny::div]
#' @export
#'
#' @examples
#' shiny_font_size_perc("foo")
#' shiny_font_size_perc("foo", 50)
shiny_font_size_perc <- function(
    value,
    size = 75
) {
    # shiny::moduleServer(.id, function(input, output, session) {
    #     ns <- session$ns

    shiny::div(value, style = "font-size:{size}%;" %>% stringr::str_glue())
    # })
}

rea_handle_data_input <- function(
    data = reactive(tibble::tibble()),
    data_fn = function() stop("Dummy data function"),
    event_id = "reload_data_train",
    verbose = FALSE,
    id = character()
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        if (!inherits(data, "reactive")) {
            data <- reactive(data)
        }

        is_empty <- reactive({
            !length(data())
        })

        reactive({
            if (is_empty()) {
                data_fn(
                    input_id = event_id,
                    verbose = verbose
                )()
            } else {
                data()
            }
        })
    })
}


# Render ------------------------------------------------------------------

render_ui <- function(
    id = NULL,
    fn,
    output_id
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output[[output_id]] <- renderUI({
            fn()
        })
    })
}

render_data_table <- function(
    id = NULL,
    data,
    output_id,
    scroll_y = 300,
    left = 1L,
    dt_bundle_buttons = dtf::dt_bundle_buttons_en,
    dt_bundle_internationalization = dtf::dt_bundle_internationalization_en
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        dtf::mod_render_dt_server(
            id = id,
            output_id = output_id,
            data = data,
            scrollY = scroll_y,
            left = left,
            .bundles = list(
                dt_bundle_buttons(),
                dt_bundle_internationalization()
            )
        )
    })
}


# Generic mod UI ----------------------------------------------------------

mod_ui_wrapper <- function(id, ..., env = environment()) {
    ns <- NS(id)

    # x <- rlang::quo(...)
    x <- substitute(...)
    tagList(rlang::eval_tidy(x, env = env))

    # tagList(...)
}

mod_server_wrapper <- function(
    id,
    ...
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        # browser()
        # rlang::eval_tidy(...)
        x <- rlang::quo(...)
        rlang::eval_tidy(x)
    })
}
