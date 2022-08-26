#' export UI Function
#'
#' @param id [[character]] Module ID
#' @param data_title
#' @param data_width
#' @param outer_box
#' @param outer_title
#' @param outer_width
#' @param select_ui
#' @param verbose [[logical]] Tracing infos yes/no
#'
#' @description A shiny Module.
#'
#' @importFrom shiny NS tagList
#' @export
mod_dt_ui <- function(
    id = "dt",
    # --- Select
    select_ui = tagList(),
    # --- Data
    data_title = "Data table",
    data_width = 12,
    # --- Outer
    outer_box = FALSE,
    outer_title = "Select columns",
    outer_width = 12,
    verbose = FALSE
) {
    ns <- NS(id)

    shiny_trace_ns_ui(
        fn_name = "mod_dt_ui",
        id_inner = "",
        ns = ns,
        verbose = verbose
    )

    ui <- tagList(
        if (length(select_ui)) {
            select_ui
        },
        fluidRow(
            shinydashboardPlus::box(
                width = data_width,
                title = data_title,
                collapsible = TRUE,
                # DT::DTOutput(ns("select_tbl"))
                dtf::mod_render_dt_ui(id = NULL,
                    output_id = ns("dt"), verbose = verbose)
            )
        )
    )

    if (outer_box) {
        fluidRow(
            shinydashboardPlus::box(
                title = tags$b(outer_title),
                width = outer_width,
                collapsible = TRUE,
                ui
            )
        )
    } else {
        ui
    }
}

#' export Server Function
#'
#' @param id [[character]] Module ID
#' @param data
#' @param output_id
#' @param dt_bundle_buttons [[function]] Seet [[dtf::dt_bundle_buttons]]
#' @param dt_bundle_internationalization [[function]] Seet [[dtf::dt_bundle_internationalization]]
#' @param scroll_y
#' @param left
#' @param verbose [[logical]] Tracing infos yes/no
#'
#' @export
mod_dt_server <- function(
    id = NULL,
    data,
    output_id = "dt",
    scroll_y = 300,
    left = 1L,
    dt_bundle_buttons = dtf::dt_bundle_buttons_en,
    dt_bundle_internationalization = dtf::dt_bundle_internationalization_en,
    verbose = FALSE
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        dtf::mod_render_dt_server(
            id = NULL,
            output_id = output_id,
            data = data,
            scrollY = scroll_y,
            left = left,
            .bundles = list(
                dt_bundle_buttons(),
                dt_bundle_internationalization()
            ),
            verbose = verbose
        )
    })
}
