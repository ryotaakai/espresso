library(shiny)
library(shiny.fluent)
library(shiny.router)
library(magrittr)
library(plotly)
library(sass)
library(ggplot2)
library(glue)
library(leaflet)
library(dplyr)
library(tibble)
library(purrr)

source("utils.R")
source("navigation.R")
source("home.R")
source("header.R")
source("footer.R")
source("preprocessing.R")
source("modal.R")
source("panel.R")

shiny.react::enableReactDebugMode()

pages <- c(
  list(route("preprocess", preprocessing_page)), 
  list(route("home", home_page))
)
router <- lift(make_router)(pages)

layout <- div(class = "grid-container",
  div(class = "header", header),
  div(class = "sidenav", navigation),
  div(class = "main", id="drop-area", ondragover = "dragOver(event)", ondrop = "dropData(event)", router$ui),
  div(class = "footer", footer)

)

# Add shiny.router dependencies manually: they are not picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")

ui <- fluentPage(
  suppressDependencies("bootstrap"),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    shiny::tags$script(type = "text/javascript", src = shiny_router_js_src),
    shiny::tags$script(src="scroll.js"),
    shiny::tags$script(src = "drag.js")
  ),
  tags$script(HTML(
    "Fluent = jsmodule['@fluentui/react']
    Fluent.loadTheme({
      defaultFontStyle: { fontFamily: 'YuGothic, Meiryo, Osaka', fontWeight: 'regular' },
      palette: {
        themePrimary: '#c99000',
        themeLighterAlt: '#080600',
        themeLighter: '#201700',
        themeLight: '#3c2b00',
        themeTertiary: '#795700',
        themeSecondary: '#b17f00',
        themeDarkAlt: '#cf9a15',
        themeDark: '#d6a833',
        themeDarker: '#e1bd63',
        neutralLighterAlt: '#ffffff',
        neutralLighter: '#ffffff',
        neutralLight: '#ffffff',
        neutralQuaternaryAlt: '#ffffff',
        neutralQuaternary: '#ffffff',
        neutralTertiaryAlt: '#ffffff',
        neutralTertiary: '#b7a579',
        neutralSecondary: '#a08b58',
        neutralPrimaryAlt: '#88733c',
        neutralPrimary: '#2b1f00',
        neutralDark: '#5a4614',
        black: '#433207',
        white: '#ffffff'
    }})"
  )),
  shiny::tags$body(
    class = "ms-Fabric",
    dir = "ltr",
    layout
  )
)

sass(
  sass_file("style.scss"),
  output = "www/style.css"
)

server <- function(input, output, session) {
  # router-server
  router$server(input, output, session)

  purrr::map(components, function(component) {
    component$server(input, output, session)
  })
  
  filteredDeals <- reactive({
    fluentSalesDeals %>% filter(
      length(input$people) == 0 | rep_id %in% input$people,
      is_closed | input$includeOpen
    )
  })

  # drag and drop
  observeEvent(input$mydata, {
    name = names(input$mydata)
    csv_file = reactive(read.csv(text=input$mydata[[name]]))
    output$test_table <- renderUI({
      DetailsList(items = csv_file(), checkboxVisibility = 2)
    })
    output$tables <- renderUI({
      table_list <- lapply(1:len, function(i) {
        tableName <- names(input$mydata)[[i]]
        tableOutput(tableName)
      })
      do.call(tagList, table_list)
      #data_output <- read.csv(text=input$mydata[[name]])
      #tableOutput(head(data_output))
    })
  })
  
  # router-rest
  output$table <- renderUI({
    DetailsList(items = filteredDeals(), columns = columns, checkboxVisibility = 2)
  })
  output$original_table <- renderUI({
    DetailsList(items = filteredDeals(), columns = columns, checkboxVisibility = 2)
  })
}

shinyApp(ui, server)