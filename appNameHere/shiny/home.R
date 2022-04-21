home_page <- Page(
  title="オリジナルデータ",
  subtitle="Original Data",
  Grid(
    GridItem(
      Card(
          div(style = "height: 100%; overflow: auto;",
              DT::dataTableOutput("originTable"))
      )
    )
  )
)