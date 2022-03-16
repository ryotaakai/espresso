Card <- function(..., title = NULL){
    Stack(class="card", tokens = list(padding=20, childrenGap=10),
        if (!is.null(title)) Text(title, variant = "large"),
        ...
    )
}

Grid <- function(...){
    div(
        class = "ms-Grid", dir = "ltr",
        style = "padding: 0px",
        ...
    )
}

GridItem <- function(..., class = "ms-sm12"){
  div(
    class = paste("ms-Grid-col", class),
    style = "padding: 10px",
    ...
  )
}

Page <- function (..., title, subtitle) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-28 ms-fontWeight-regular"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
      "margin: 14px;")
  ),
  ...)
}

