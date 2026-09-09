# Application UI --------------------------------------------------------
app_ui <- function() {
  shinydashboard::dashboardPage(
    header = build_header(),
    sidebar = build_sidebar(),
    body = build_body()
  )
}

