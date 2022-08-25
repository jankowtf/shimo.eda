#' export UI Function
#'
#' @param id [[character]] Module ID
#' @param ns [[function]] Namespace function
#' @param select_title
#' @param select_width
#' @param select_id
#' @param select_button_label
#' @param select_button_class
#' @param select_button_style
#' @param select_button_icon
#' @param select_button_width
#' @param data_title
#' @param data_width
#' @param outer_box
#' @param outer_title
#' @param outer_width
#' @param verbose [[logical]] Tracing infos yes/no
#' @param render_data [[logical]] Render data table yes/no
#'
#' @description A shiny Module.
#'
#' @importFrom shiny NS tagList
#' @export
mod_select2_ui <- function(
    id = "eda_select",
    ns = function() {},
    # --- Select
    select_title = "Column filter (none means 'all columns')",
    select_width = 12,
    select_id = "select_ui",
    select_button_label = "Submit",
    select_button_class = "btn-success",
    select_button_style = "color: #fff;",
    select_button_icon = icon('check'),
    select_button_width = 100,
    # --- Data
    render_data = FALSE,
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
        fn_name = "mod_select2_ui",
        id_inner = "foo",
        ns = ns,
        verbose = verbose
    )

    ui <- tagList(
        fluidRow(
            shinydashboardPlus::box(
                title = select_title,
                width = select_width,
                collapsible = TRUE,
                uiOutput(ns("select_ui")),
                # vertical_space(),
                actionButton(
                    ns("submit"),
                    label = select_button_label,
                    class = select_button_class,
                    style = select_button_style,
                    icon = select_button_icon,
                    width = select_button_width
                )
            )
        ),
        if (render_data) {
            fluidRow(
                shinydashboardPlus::box(
                    width = data_width,
                    title = data_title,
                    collapsible = TRUE,
                    DT::DTOutput(ns("select_tbl"))
                )
            )
        }
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
#' @param input_id_prefix
#' @param dt_bundle_buttons [[function]] Seet [[dtf::dt_bundle_buttons]]
#' @param dt_bundle_internationalization [[function]] Seet [[dtf::dt_bundle_internationalization]]
#' @param verbose [[logical]] Tracing infos yes/no
#' @param render_data [[logical]] Render data table yes/no
#'
#' @export
mod_select2_server <- function(
    id = "eda_select",
    data,
    input_id_prefix = "selectize_input",
    dt_bundle_buttons = dtf::dt_bundle_buttons_en,
    dt_bundle_internationalization = dtf::dt_bundle_internationalization_en,
    verbose = FALSE,
    fn_select_ui = create_selectize_ui,
    render_data = FALSE
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # --- Create select UI ----
        input_ids <- get_input_ids(input_id_prefix = input_id_prefix, sort = TRUE)
        input_values <- get_input_values(input_ids = input_ids)

        shiny_trace_ns_server(
            fn_name = "mod_select_server",
            id_inner = input_ids,
            verbose = verbose
        )

        fn_select_ui <- fn_select_ui(
            data = data,
            input_id_prefix = input_id_prefix
        )

        render_select_ui(id = NULL, fn_select_ui = fn_select_ui)

        data_selected <- reactive_select(
            id = NULL,
            data = data,
            input_ids = input_ids,
            input_values = input_values,
            event_id = "submit",
            ignore_null = FALSE
        )

        # observe({
        #     browser()
        #     data_selected()
        # })

        # --- Render data table ---
        if (render_data) {
            render_select_data_table2(
                id = NULL,
                data = data_selected,
                dt_bundle_buttons = dt_bundle_buttons,
                dt_bundle_internationalization = dt_bundle_internationalization
            )
        } else {
            data_selected
        }
    })
}

# Create inputs -----------------------------------------------------------

create_selectize_ui <- function(
    id = NULL,
    data,
    input_id_prefix = "selectize_input",
    width = "50%"
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        shiny::reactive({
            cols <- data() %>% names()

            input_id <- input_id_prefix

            shiny::selectizeInput(
                inputId = ns(input_id),
                label = NULL,
                choices = cols,
                multiple = TRUE,
                width = width
            )

        })
    })
}

# Render data table -------------------------------------------------------

#' Title
#'
#' @param id
#' @param data
#' @param input_ids
#' @param input_values
#' @param buttons_language
#'
#' @return
#'
#' @examples
render_select_data_table2 <- function(
    id = NULL,
    data,
    event_id = "submit",
    dt_bundle_buttons = dtf::dt_bundle_buttons_en,
    dt_bundle_internationalization = dtf::dt_bundle_internationalization_en
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        dtf::mod_render_dt_server(
            id = id,
            output_id = "select_tbl",
            data = data,
            scrollY = 300,
            left = 1,
            .bundles = list(
                dt_bundle_buttons(),
                dt_bundle_internationalization()
            )
        )
    })
}
