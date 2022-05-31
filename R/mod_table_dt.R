# UI ----------------------------------------------------------------------

#' export UI Function
#'
#' @param id [[character]] Module ID
#' @param ns [[function]] Namespace function
#' @param output_id [[character]] Output ID
#' @param verbose [[logical]] Tracing infos yes/no
#'
#' @description A shiny Module.
#'
#' @importFrom shiny NS tagList
#' @export
mod_table_dt_ui <- function(
    id = "table_dt",
    ns = function() {},
    output_id = "dt",
    # --- Box
    box = FALSE,
    box_title = "Data table",
    box_width = 12,
    verbose = FALSE
) {
    ns_provided <- !missing(ns)

    if (ns_provided) {
        id <- ns(id)
    }

    ns <- NS(id)

    dt <- dtf::mod_render_dt_ui(
        # id = id,
        # output_id = if (ns_provided) {
        #     output_id
        # } else {
        #     ns(output_id)
        # },
        # Above works too (else part) but it's arguably cleaner to use 'ns' for
        # 'id' instead of 'output_id'. Doing it this way would duplicate the
        # 'tabble_dt' prefix
        id = ns("render"),
        output_id = output_id,
        verbose = verbose
    )

    if (box) {
        tagList(
            fluidRow(
                shinydashboardPlus::box(
                    title = tags$b(box_title),
                    width = box_width,
                    collapsible = TRUE,
                    dt
                )
            )
        )
    } else {
        tagList(dt)
    }
}

# Server ------------------------------------------------------------------

#' export Server Function
#'
#' @param id [[character]] Module ID
#' @param verbose [[logical]] Tracing infos yes/no
#' @param data
#' @param output_id
#' @param scrollY
#' @param left
#' @param right
#' @param trans_fn
#' @param rename_fn
#' @param .bundles
#' @param .rownames
#' @param .editable
#' @param .escape
#' @param ...
#'
#' @export
mod_table_dt_server <- function(
    id = "table_dt",
    # ns = function() {},
    output_id = "dt",
    data,
    # filter = c("none", "bottom", "top"),
    scrollY = 400,
    left = integer(),
    right = integer(),
    # selection = valid_dt_arg_selection("none"),
    trans_fn = identity,
    rename_fn = identity,
    .bundles = list(),
    .rownames = TRUE,
    .editable = FALSE,
    .escape = TRUE,
    verbose = FALSE,
    ...
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        dtf::mod_render_dt_server(
            # id = id,
            # Doing it this way would duplicate the 'tabble_dt' prefix. See
            # '*_ui()' function above
            id = "render",
            output_id = output_id,
            data = data,
            scrollY = scrollY,
            left = left,
            right = right,
            # selection = selection,
            trans_fn = trans_fn,
            rename_fn = rename_fn,
            .bundles = .bundles,
            .rownames = .rownames,
            .editable = .editable,
            .escape = .escape,
            verbose = verbose,
            ...
        )
    })
}

# Foo ---------------------------------------------------------------------

mod_foo_ui <- function(
    id = "foo",
    ns = function() {},
    verbose = FALSE
) {
    if (!missing(ns)) {
        id <- ns(id)
    }

    ns <- NS(id)

    # Desired: keeping the DEFAULT for 'id' in 'mod_table_dt_ui()' but nesting
    # it within 'ns' of 'mod_foo_ui()'
    # mod_table_dt_ui(
    #     ns = ns,
    #     verbose = verbose
    # )

    mod_table_dt_ui(
        id = ns("table_dt"),
        verbose = verbose
    )
}

mod_foo_server <- function(
    id = "foo",
    data,
    verbose = FALSE
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # mod_table_dt_server(
        #     # id = id,
        #     data = data
        # )

        mod_table_dt_server(
            id = ns("table_dt"),
            data = data
        )
    })
}
