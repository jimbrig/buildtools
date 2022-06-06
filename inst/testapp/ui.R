fluidPage(
  fluidRow(
    column(
      6,
      h1("Test App"),
      br()
    ),
    column(
      6,
      br(),
      actionButton(
        "sign_out",
        "Sign Out",
        icon = icon("sign-out-alt"),
        class = "pull-right"
      )
    ),
    column(
      12,
      verbatimTextOutput("secure_content")
    )
  )
)
