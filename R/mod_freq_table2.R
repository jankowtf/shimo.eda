# UI ----------------------------------------------------------------------

#' export UI Function
#'
#' @param id [[character]] Module ID
#' @param ns [[function]] Namespace function
#' @param grouping_title
#' @param grouping_width
#' @param grouping_button_label
#' @param grouping_button_class
#' @param grouping_button_icon
#' @param grouping_button_width
#' @param freq.box_title
#' @param outer_box [[logical]] Wrap in outer box yes/no
#' @param outer_title
#' @param outer_width
#' @param freq.box_width
#' @param freq.box_title_width
#' @param freq.dropdown_title
#' @param freq.dropdown_width
#' @param freq.dropdown_col_n_abs_label
#' @param freq.dropdown_col_n_rel_label
#' @param freq.col_n_abs
#' @param freq.col_n_rel
#' @param verbose [[logical]] Tracing infos yes/no
#' @param grouping.button_submit_label
#' @param grouping.button_submit_class
#' @param grouping.button_submit_style
#' @param grouping.button_submit_icon
#' @param grouping.button_submit_width
#'
#' @description A shiny Module.
#'
#' @importFrom shiny NS tagList
#' @export
mod_freq_table2_ui <- function(
    id = "freq_table",
    ns = function() {},
    # --- Grouping
    grouping_title = "Group by",
    grouping_width = 12,
    grouping_button_label = "Add group-by statement",
    grouping_button_class = "btn-primary",
    # grouping_button_style = "background: #0c3992;color: #fffff;",
    grouping_button_icon = icon('plus'),
    grouping_button_width = 190,
    grouping.button_submit_label = "Submit",
    grouping.button_submit_class = "btn-success",
    grouping.button_submit_style = "color: #fff;",
    grouping.button_submit_icon = icon('check'),
    grouping.button_submit_width = 100,
    # --- Freq table
    freq.box_title = "Frequency table",
    freq.box_width = 12,
    freq.box_title_width= 200,
    freq.dropdown_title = "Column names",
    freq.dropdown_width= 150,
    freq.dropdown_col_n_abs_label = "Name for abs. count",
    freq.dropdown_col_n_rel_label = "Name for rel. count",
    freq.col_n_abs = "n_abs",
    freq.col_n_rel = "n_rel",
    # --- Outer
    outer_box = FALSE,
    outer_title = "Frequency table",
    outer_width = 12,
    verbose = FALSE
) {
    if (!missing(ns)) {
        id <- ns(id)
    }

    ns <- NS(id)

    shiny_trace_ns_ui(
        fn_name = "mod_freq_table_ui",
        id_inner = "foo",
        ns = ns,
        verbose = verbose
    )

    ui <- tagList(
        fluidRow(
            shinydashboardPlus::box(
                title = grouping_title,
                width = grouping_width,
                collapsible = TRUE,
                column(
                    width = 3,
                    uiOutput(ns("grouping_ui")),
                    actionButton(
                        ns("submit"),
                        label = grouping.button_submit_label,
                        class = grouping.button_submit_class,
                        # style = grouping.button_submit_style,
                        icon = grouping.button_submit_icon,
                        width = grouping.button_submit_width
                    )
                )
                # sidebar = shinydashboardPlus::boxSidebar(
                #     id = ns("boxsidebar"),
                #     width = 25,
                #     startOpen = TRUE
                #     # textInput(ns("col_n_abs"), label = NULL,
                #     #     value = freq.col_n_abs),
                #     # textInput(ns("col_n_rel"), label = NULL,
                #     #     value = freq.col_n_rel)
                # )
            )
        ),
        fluidRow(
            shinydashboardPlus::box(
                # title = freq.box_title,
                title = div(
                    div(
                        style = "display: inline-block;vertical-align:middle;width:{freq.box_title_width};" %>%
                            stringr::str_glue(),
                        h4(freq.box_title)
                    ),
                    shiny::div(
                        style = "display: inline-block;vertical-align:middle;width:{freq.dropdown_width}px;" %>%
                            stringr::str_glue(),
                        shinyWidgets::dropdownButton(
                            h4(freq.dropdown_title),
                            textInput(ns("col_n_abs"),
                                label = freq.dropdown_col_n_abs_label,
                                value = freq.col_n_abs),
                            textInput(ns("col_n_rel"),
                                label = freq.dropdown_col_n_rel_label,
                                value = freq.col_n_rel),
                            circle = TRUE,
                            size = "xs",
                            status = "danger",
                            icon = icon("gear", verfiy_fa = FALSE),
                            width = "150px",
                            tooltip = shinyWidgets::tooltipOptions(
                                title = "Click to see inputs")
                        )
                    )
                ),
                width = freq.box_width,
                collapsible = TRUE,

                vertical_space(1),
                DT::DTOutput(ns("grouping_tbl"))
            ),
        ),
        # sortable::sortable_js(ns("group_by_ui")),
        tags$script(src = "shimo.eda.js"),
        tags$script(paste0("mod_group_by_js('", ns(''), "')"))
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

# Server ------------------------------------------------------------------

#' export Server Function
#'
#' @param id [[character]] Module ID
#' @param data [[reactive]] Reactive function that serves data
#' @param dt_bundle_buttons [[function]] Seet [[dtf::dt_bundle_buttons]]
#' @param dt_bundle_internationalization [[function]] Seet [[dtf::dt_bundle_internationalization]]
#' @param verbose [[logical]] Tracing infos yes/no
#' @param dt_bundles_additional [[list]] Additional DT bundles. See
#'   [dtf::dt_bundle_dom] as an example
#' @param transform_fn
#' @param dt.filter [[character]] DT column filter settings. See package [dtf]
#' @param dt.scroll_y [[integer]] DT: pixels for vertical scrolling
#' @param dt.left [[integer]] DT: columns to fix from the left
#'
#' @export
mod_freq_table2_server <- function(
    id = "freq_table",
    data,
    dt_bundle_buttons = dtf::dt_bundle_buttons_en,
    dt_bundle_internationalization = dtf::dt_bundle_internationalization_en,
    dt_bundles_additional = list(),
    transform_fn = identity,
    dt.filter = dtf::valid_dt_filter_values(1),
    dt.scroll_y = 300,
    dt.left = 1L,
    verbose = FALSE,
    fn_group_by_ui = create_selectize_ui,
    input_id_prefix = "grouping_input"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # --- Create UI ---
        input_ids <- get_input_ids(input_id_prefix = "grouping_input", sort = TRUE)
        input_values <- get_input_values(input_ids = input_ids)

        shiny_trace_ns_server(
            fn_name = "mod_freq_table_server",
            id_inner = input_ids,
            verbose = verbose
        )

        fn_group_by_ui <- fn_group_by_ui(
            data = data,
            input_id_prefix = input_id_prefix,
            width = "100%"
        )

        render_ui(fn = fn_group_by_ui, output_id = "grouping_ui")

        # --- Freq table ---
        data_freq_table <- reactive_freq_table_all_incl(
            id = NULL,
            data = data,
            input_ids = input_ids,
            input_values = input_values,
            ignore_null = FALSE
        )

        # --- Render data table ---
        render_freq_table(
            id = NULL,
            data = data_freq_table,
            dt_bundle_buttons = dt_bundle_buttons,
            dt_bundle_internationalization = dt_bundle_internationalization,
            dt_bundles_additional = dt_bundles_additional,
            dt.filter = dt.filter,
            dt.scroll_y = dt.scroll_y,
            dt.left = dt.left,
            transform_fn = transform_fn,
            event_id = "submit"
        )
    })
}
