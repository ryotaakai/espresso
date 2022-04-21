style <- list(margin = 10)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  div(
    Text(variant = "medium", nowrap = FALSE, "履歴"),
    uiOutput("uiIconHistory")
  ),
  Text(variant = "medium", nowrap = FALSE, "Rio's rights reserved")
)

HistoryItem <- function(inputKey) {
  switch(inputKey,
         "class_change" = list(FontIcon(iconName = "HandsFree", style=style)),
         "arrange_row" = list(FontIcon(iconName = "HandsFree", style=style)),
         "filter_row" = list(FontIcon(iconName = "HandsFree", style=style)),
         "select_columns" = list(FontIcon(iconName = "HandsFree", style=style)),
         "columnDelete" = list(FontIcon(iconName = "HandsFree", style=style)),
         "nameChange" = list(FontIcon(iconName = "HandsFree", style=style)),
         "dttm_change" = list(FontIcon(iconName = "Timer", style=style)),
         list(FontIcon(iconName = "WebAppBuilderModule", style=style))
  )
}

appendIcon <- function(inputKey, iconHistory, output) {
  iconHistory <- c(iconHistory, HistoryItem(inputKey))
  output$uiIconHistory <- renderUI({iconHistory()})
}