makeModal <- function() {
  list(
    ui = tagList(
      reactOutput("modal"),
    ),
    server = function(input, output, session) {
      modalVisible <- reactiveVal(FALSE)
      observeEvent(input$mydata, modalVisible(TRUE))
      observeEvent(input$hideModal, modalVisible(FALSE))
      observeEvent(input$upload, modalVisible(TRUE))
      output$modal <- renderReact({
        Modal(isOpen = modalVisible(),
          Stack(tokens = list(padding = "15px", childrenGap = "10px"),
            div(style = list(display = "flex"),
              Text("テーブル処理", variant = "large"),
              div(style = list(flexGrow = 1)),
              IconButton.shinyInput("hideModal", iconProps = list(iconName = "Cancel")),
            ),
            div(
              p("テーブルUIを表示")
            )
          )
        )
      })
    }
  )
}

components$Modal <- makeModal()