components <- list()

makeTable <- function(){
    list(
        ui=Card(
            title="Sales deals details",
            div(style = "height: 500px; overflow: auto;",
            uiOutput("test_table")),
        ),
        server = function(input, output, session) {
            path <- reactiveValues(pth = NULL)
            file.choose2 <- function(...) {
                pathname <- NULL;
                tryCatch({
                pathname <- file.choose();
                }, error = function(ex) {
                })
                pathname;
            }
            observeEvent(input$upload,{
                path$pth <- file.choose2()
            })

            observeEvent(input$upload,{
                newvalue <- "B"
                updateNavbarPage(session, "navbar", newvalue)
            })
            data <- reactive({
                req(path$pth)
                df <- readr::read_csv(file = path$pth)
                return(df)
            })
            output$test_table <- renderUI({
                DetailsList(items = data())
            })
        }
    )
}

components$Table <- makeTable()

makePlot <- function() {
    list(
        ui=Card(title = "Deals count",
            plotlyOutput("plot")
        ),
        server = function(input, output, session) {
            output$plot <- renderPlotly({
                ggplot(data(), aes(x = Sepal.Length)) +
                    geom_bar() +
                    xlab("x") + 
                    ylab("y") +
                    theme_light()
            })
        }
    )
}

components$Plot <- makePlot()

# タブアイテム
setClickedId <- function(inputId) {
  JS(glue("item => Shiny.setInputValue('{inputId}', item.props.id)"))
}

makePivot <- function() {
    list(
        ui = tagList(
            Pivot(
                onLinkClick = setClickedId("currentTab"),
                PivotItem(id = "tab1", headerText = "型変換"),
                PivotItem(id = "tab2", headerText = "並べ替え"),
                PivotItem(id = "tab3", headerText = "行フィルター"),
                PivotItem(id = "tab4", headerText = "欠損値処理")
            ),
            textOutput("text")
        ),
        server = function(input, output, session) {
            output$text <- renderText(input$currentTab)
        }
    )
}
components$Pivot <- makePivot()

filters <- tagList(
  div(
  Label("Sales representative"),
  NormalPeoplePicker.shinyInput("people", options = fluentPeople)
  ),
  Toggle.shinyInput("includeOpen", label = "Include open deals")
)
filters2 <- tagList(
    div(
        Pivot(
            onLinkClick = setClickedId("currentTab"),
            PivotItem(id = "tab1", headerText = "型変換"),
            PivotItem(id = "tab2", headerText = "並べ替え"),
            PivotItem(id = "tab3", headerText = "行フィルター"),
            PivotItem(id = "tab4", headerText = "欠損値処理")
        )
    ),
    textOutput("text")
)

columns <- tibble(
  fieldName = c("rep_name", "date", "deal_amount", "city", "is_closed"),
  name = c("Sales rep", "Close date", "Amount", "City", "Is closed?")
)

makeProcessingPage <- function(name, filter, plot, table) {
    Page(
        title = "前処理",
        subtitle = "Preprocessing",
        Grid(
            GridItem(
                class = "ms-sm12 ms-xl4",
                filter
            ),
            GridItem(
                class = "ms-sm12 ms-xl8",
                plot
            ),
            GridItem(
                table
            )
        )
    )
}

preprocessing_page <- Page(
  title = "前処理",
  subtitle = "Preprocessing",
  Grid(
    GridItem(
        class = "ms-sm12 ms-xl4",
        Card(
            title="フィルター",
            filters2,
        ),
    ),
    GridItem(
        class = "ms-sm12 ms-xl8",
        Card(title = "プロット",
            plotlyOutput("plot")
        )
    ),
    GridItem(
        Card(
            title="テーブル",
            id = "table_ui",
            div(style = "height: 500px; overflow: auto;",
            uiOutput("test_table")),
        )
    ),
    
  ),
  tagList(reactOutput("modal")),
  tagList(reactOutput("reactPanel")),
  HTML('<button onclick="topFunction()" id="myBtn" title="Go to top"></button>')
)