#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    shiny::tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        shinydashboardPlus::dashboardPage(
            # title = "Demo",
            skin = "red",
            options = list(
                sidebarExpandOnHover = TRUE
            ),
            header = shinydashboardPlus::dashboardHeader(
                title = "Demo",
                titleWidth = 100,
                fixed = TRUE,

                leftUi = htmltools::tagList(
                )
            ),
            sidebar = shinydashboardPlus::dashboardSidebar(
                id = "sidebar",
                width = 300,
                shinydashboard::sidebarMenu(
                    id = "tabs",

                    # --- Select ---
                    shinydashboard::menuItem(
                        "Select",
                        tabName = "select",
                        icon = icon("filter")
                    ),

                    # --- Freq table ---
                    shinydashboard::menuItem(
                        "Frequency table",
                        tabName = "freq_table",
                        icon = icon("table")
                    )
                )
            ),
            body = shinydashboard::dashboardBody(
                shinyjs::useShinyjs(),
                shinydashboard::tabItems(
                    # --- Select ---
                    shinydashboard::tabItem(
                        tabName = "select",
                        tabitem_vertical_space(2),
                        h3("Select"),
                        mod_eda_select_ui(id = NULL)
                    ),

                    # --- Freq table ---
                    shinydashboard::tabItem(
                        tabName = "freq_table",
                        br(),
                        br(),
                        h3("Frequency tables"),
                        mod_eda_freq_table_ui(id = NULL)
                    )
                )
            ),
            controlbar = shinydashboardPlus::dashboardControlbar()
        )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shimo.eda"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
