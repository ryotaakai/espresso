header <- tagList(
  img(src = "estyle-logo.png", class = "logo"),
  div(shiny.fluent::Text(variant = "xLarge", "ESPRESSO"), class = "title"),
  div(
    shiny.fluent::CommandBarButton.shinyInput("upload", class="commandbutton", text="アップロード", iconProps = list("iconName" = "Upload"))
  ),
  CommandBarButton.shinyInput("download", class = "commandbutton", iconProps = list("iconName" = "Download")),
  downloadButton("download", class = "link", label = "ダウンロード", icon = NULL),
  div(
    CommandBarButton.shinyInput("share", class="commandbutton", text="共有", iconProps = list("iconName" = "Share"))
  ),
  div(
    CommandBarButton.shinyInput("data_robot", class="commandbutton", iconProps = list("iconName" = "Robot"))
  ),
  div(class="toggle",
    Toggle.shinyInput("view_toggle", class = "toggle", value = FALSE)
  )
)