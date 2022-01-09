makePanel <- function() {
  list(
    ui = div(
      reactOutput("reactPanel")
    ),
    server = function(input, output, session) {
      isPanelOpen <- reactiveVal(FALSE)
      output$reactPanel <- renderReact({
        Panel(
          headerText = "Data Robot",
          isOpen = isPanelOpen(),
          Stack(
            TextField(label = "エンドポイント"),
            TextField(label = "APIキー"),
            TextField(label = "プロジェクト名"),
            Text(variant = "medium", "ターゲット変数"),
            Dropdown(options = list()),
            Text(variant = "medium", "オートパイロットタイプ"),
            Dropdown(options = list("クイック","フルオート")),
            PrimaryButton("button2", text = "送信")
          ),
          onDismiss = JS("function() { Shiny.setInputValue('hidePanel', Math.random()); }")
        )
      })
      observeEvent(input$data_robot, isPanelOpen(TRUE))
      observeEvent(input$hidePanel, isPanelOpen(FALSE))
    }
  )
}

components$Panel <- makePanel()