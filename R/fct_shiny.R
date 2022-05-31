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
