dashboard_page <- Page(
  title="ダッシュボード",
  subtitle="Dashboard",
  Grid(
    GridItem(
      Card(
        div(style = "height: 100%; overflow: auto;",
            uiOutput("uiSavePlotItems"))
      )
    )
  )
)