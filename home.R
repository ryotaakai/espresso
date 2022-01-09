home_page <- Page(
  title="オリジナルデータ",
  subtitle="Original Data",
  Grid(
    GridItem(
      Card(
          title="Sales deals details",
          div(style = "height: 100%; overflow: auto;",
          uiOutput("table")),
      )
    )
  )
)
