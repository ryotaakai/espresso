header <- tagList(
  img(src = "estyle-logo.png", class = "logo"),
  div(Text(variant = "xLarge", "ESPRESSO"), class = "title"),
  div(
    CommandBarButton.shinyInput("add", class="commandbutton", text="新規プロジェクト", iconProps = list("iconName" = "Add")),
  ),
  div(
    CommandBarButton.shinyInput("project_open", class="commandbutton", text="プロジェクトを開く", iconProps = list("iconName" = "FabricOpenFolderHorizontal"))
  ),
  div(
    CommandBarButton.shinyInput("save", class="commandbutton", text="保存", iconProps = list("iconName" = "Save"))
  ),
  div(
    CommandBarButton.shinyInput("upload", class="commandbutton", text="アップロード", iconProps = list("iconName" = "Upload"))
  ),
  div(
    CommandBarButton.shinyInput("download", class="commandbutton", text="ダウンロード", iconProps = list("iconName" = "Download"))
  ),
  div(
    CommandBarButton.shinyInput("share", class="commandbutton", text="共有", iconProps = list("iconName" = "Share"))
  ),
  div(
    CommandBarButton.shinyInput("grid_view", class="commandbutton", iconProps = list("iconName" = "Tiles"))
  ),
  div(
    CommandBarButton.shinyInput("data_robot", class="commandbutton", iconProps = list("iconName" = "Robot"))
  )
)