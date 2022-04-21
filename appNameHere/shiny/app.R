library(shiny)
library(shinyThings)
library(shiny.fluent)
library(shinyWidgets)
library(shiny.router)
library(magrittr)
library(plotly)
library(sass)
library(ggplot2)
library(glue)
library(leaflet)
library(dplyr)
library(tibble)
library(jsonlite)
library(lubridate)
library(purrr)
library(TSclust)
library(zoo)


source("server_function.R")

source("utils.R")
source("navigation.R")
source("home.R")
source("dashboard.R")
source("header.R")
source("footer.R")
source("preprocessing.R")

pages <- c(
  list(route("preprocess", preprocessing_page)), 
  list(route("home", home_page)),
  list(route("dashboard", dashboard_page))
)
router <- lift(make_router)(pages)

layout <- div(class = "grid-container",
  div(class="header", header),
  div(class="sidenav", navigation),
  div(class="main", id="drop-area", ondragover="dragOver(event)", ondrop="dropData(event)", router$ui),
  div(class="footer", footer)
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
  tags$script("
      Shiny.addCustomMessageHandler('colnames', function(colInfo) {
        console.log(colInfo);
        testTags = colInfo.map(function(x){
          return {key:x, name:x};
        })
      });
    "),
  tags$script(HTML("
        function listContainsTagList(tag, tagList) {
          if (!tagList || !tagList.length || tagList.length === 0) {
            return false;
          }
          return tagList.some(compareTag => compareTag.key === tag.key);
        };

        function filterSuggestedTags(filterText, tagList) {
          return filterText
            ? testTags.filter(
                tag => tag.name.toLowerCase().indexOf(filterText.toLowerCase()) === 0 && !listContainsTagList(tag, tagList),
              )
            : [];
        };
      ")),
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

  values <- reactiveValues()
  values$dat <- data.frame(x=c())
  values$origin <- data.frame(x=c())
  values$flagUpdateSelected <- F
  values$proxyColumnInfo <- DT::dataTableProxy("columnInfo", session)
  values$dat_test <- data.frame(x=c())
  values$dat_test_init <- data.frame(x=c())
  values$scoreframe <- data.frame(x=c())
  values$clusterd_data <- data.frame(x=c())
  values$colinfo_for_clustering <- data.frame(x=c())
  values$save_plot <- list()

  iconHistory <-c()
  computing <- reactiveVal(FALSE)
  trigger <- debounce(computing, 5)
  
  # FILE UPLOAD
  path <- reactiveValues(pth = NULL)
  file_upload <- function(...) {
    pathname <- NULL;
    tryCatch({
      pathname <- file.choose();
    }, error = function(ex) {
    })
    pathname;
  }
  observeEvent(input$upload,{
    computing(TRUE)
    path$pth <- file_upload()
    csv_file <- reactive({
      req(input$upload)
      ext <- tools::file_ext(path$pth)
      switch(ext,
         csv = data <- read.csv(path$pth,stringsAsFactors=F,na.strings=""),
         tsv = data <- read.tsv(path$pth,stringsAsFactors=F,na.strings=""),
         validate("Invalid file; Please upload a .csv or .tsv file")
      )
      data
    })
    values$origin <- csv_file()
    values$dat <- csv_file()
    colInfo <- colnames(values$dat)
    session$sendCustomMessage("colnames", colInfo)
    
    updateData(csv_file(), input, output, session, values)
  })
  
  observeEvent(input$save_plot,{
    req(input$plot)
    uiSavePlot = reactive(switch(input$plot,
                             "plotBasic" = SavePlotItem("plotBasic"),
                             "plotAgg" = SavePlotItem("plotAgg"),
                             "plotFund" = SavePlotItem("plotFund"),
                             "plotImp" = SavePlotItem("plotImp"),
                             "plotCorr" = SavePlotItem("plotCorr"),
                             "plotLabel" = SavePlotItem("plotLabel"),
                             "plotClust" = SavePlotItem("plotClust")
    ))
    print(uiSavePlot())
    values$save_plot <- append(values$save_plot, list(div(
      style="text-align:right;float:right;",
      CommandBarButton.shinyInput(sprintf("delete%s", input$plot), class="deletePlot", iconProps = list("iconName" = "StatusCircleErrorX"))
    ), uiSavePlot()))
    output$uiSavePlotItems <- renderUI(values$save_plot)
  })
  
  # HISTORY
  undo_app_state <- undoHistory(
    id = "hist",
    value = reactive({
      # Value must be a reactive, but can be any structure you want
      req(!is.null(values$dat))
      values$dat
    })
  )
  
  observe({
    req(!is.null(undo_app_state())) #<< Need to update app whenever not NULL
    # Manually update app UI and reactive values
    updateData(undo_app_state(), input, output, session, values)
  })
  # SPINNER
  observeEvent(trigger(), {
    if (trigger()) {
      Sys.sleep(3) # Long computation
      computing(FALSE)
    }
  })
  
  output$spinner <- renderReact({
    if (computing()) ProgressIndicator(label="読み込み中")
  })
  
  # drag and drop table
  observeEvent(input$dataDrop, {
    computing(TRUE)
    name = names(input$dataDrop)
    # csv_file = reactive(
    #   read.csv(text=input$dataDrop[[name[length(name)]]])
    # )
    csv_file <- reactive({
      req(input$dataDrop)
      name = names(input$dataDrop)
      ext <- tools::file_ext(name[length(name)])
      switch(ext,
             csv = data <- read.csv(text=input$dataDrop[[name[length(name)]]]),
             tsv = data <- read.tsv(text=input$dataDrop[[name[length(name)]]]),
             validate("Invalid file; Please upload a .csv or .tsv file")
      )
      data
    })
    values$origin <- csv_file()
    values$dat <- csv_file()
    colInfo <- colnames(values$dat)
    session$sendCustomMessage("colnames", colInfo)
    updateData(csv_file(), input, output, session, values)
  })
  
  # VIEW MODESELECTION
  output$uiView <- renderUI({list(
    GridItem(
      class = "ms-sm12 ms-xl8",
      Card(title = "",
           div(
             style="text-align:left;",
             shiny.fluent::Text(variant = "large", 'プロットパネル'),
             div(
               style="text-align:right;float:right;",
               CommandBarButton.shinyInput("save_plot", class="commandbutton", text="ダッシュボードに保存する", iconProps = list("iconName" = "Save"))
             )
           ),
           plotMenu,
           uiOutput("uiPlotItems")
      )
    ),
    GridItem(
      Card(
        title="テーブル",
        id = "table_ui",
        div(style = "height: 500px;",
            DT::dataTableOutput("outputTable"))
      )
    )
  )})
  
  observeEvent(input$view_toggle, {
    if(input$view_toggle){
      output$uiView <- renderUI({list(
        GridItem(
          class = "ms-sm12 ms-xl8",
          Card(title = "",
               div(
                 style="text-align:left;",
                 shiny.fluent::Text(variant = "large", 'プロットパネル'),
                 div(
                   style="text-align:right;float:right;",
                   CommandBarButton.shinyInput("save_plot", class="commandbutton", text="ダッシュボードに保存する", iconProps = list("iconName" = "Save"))
                 )
               ),
               plotMenu,
               uiOutput("uiPlotItems")
          )
        ),
        GridItem(
          Card(
            title="テーブル",
            id = "table_ui",
            div(style = "height: 500px; overflow: auto;",
                DT::dataTableOutput("outputTable"))
          )
        )
      )})
    } else {
      output$uiView <- renderUI({list(
        GridItem(
          class = "ms-sm12 ms-xl8",
          Card(
            title="テーブル",
            id = "table_ui",
            div(style = "height: 500px; overflow: auto;",
                DT::dataTableOutput("outputTable"))
          )
        ),
        GridItem(
          Card(title = "",
               div(
                 style="text-align:left;",
                 shiny.fluent::Text(variant = "large", 'プロットパネル'),
                 div(
                   style="text-align:right;float:right;",
                   CommandBarButton.shinyInput("save_plot", class="commandbutton", text="ダッシュボードに保存する", iconProps = list("iconName" = "Save"))
                 )
               ),
               plotMenu,
               uiOutput("uiPlotItems")
          )
        )
      )})
    }
  })
  
  # FILTER MODE SELECTION
  observeEvent(input$filter, {
    req(input$filter)
    uiFilter = reactive(switch(input$filter,
      "convertType" = FilterItem("convertType"),
      "reorderRow" = FilterItem("reorderRow"),
      "filterRow" = FilterItem("filterRow"),
      "selectColumn" = FilterItem("selectColumn"),
      "deleteColumn" = FilterItem("deleteColumn"),
      "renameColumn" = FilterItem("renameColumn"),
      "onehotEnc" = FilterItem("onehotEnc"),
      "targetEnc" = FilterItem("targetEnc"),
      "numericNull" = FilterItem("numericNull"),
      "characterNull" = FilterItem("characterNull"),
      "allOnehotEnc" = FilterItem("allOnehotEnc"),
      "datetimeConversion" = FilterItem("datetimeConversion"),
      "datetimeFilter" = FilterItem("datetimeFilter"),
      "lagGeneration" = FilterItem("lagGeneration"),
      "leadGeneration" = FilterItem("leadGeneration"),
      "movingAverage" = FilterItem("movingAverage"),
      "clustering" = FilterItem("clustering"),
      "characterMerge" = FilterItem("characterMerge"),
      "characterSplit" = FilterItem("characterSplit"),
      "characterFilter" = FilterItem("characterFilter"),
      "characterReplace" = FilterItem("characterReplace"),
      "characterContain" = FilterItem("characterContain"),
      "customFunction" = FilterItem("customFunction"),
      "mergeIntoTable" = FilterItem("mergeIntoTable"),
      "plotlyPlot" = FilterItem("plotlyPlot"),
      "aggPlot" = FilterItem("aggPlot"),
      "fundStatPlot" = FilterItem("fundStatPlot"),
      "impPlot" = FilterItem("impPlot"),
      "corrPlot" = FilterItem("corrPlot"),
      "labelPlot" = FilterItem("labelPlot")
    ))
    uiPlot = reactive(switch(input$filter,
       "clustering" = PlotItem("plotClust")
    ))
    output$uiPlotItems <- renderUI({uiPlot()})
    output$uiFilterItems <- renderUI({uiFilter()})
  })
  
  # PLOT MODE SELECTION
  observeEvent(input$plot, {
    req(input$plot)
    uiPlot = reactive(switch(input$plot,
      "plotBasic" = PlotItem("plotBasic"),
      "plotAgg" = PlotItem("plotAgg"),
      "plotFund" = PlotItem("plotFund"),
      "plotImp" = PlotItem("plotImp"),
      "plotCorr" = PlotItem("plotCorr"),
      "plotLabel" = PlotItem("plotLabel"),
      "plotClust" = PlotItem("plotClust")
    ))
    uiFilter = reactive(switch(input$plot,
      "plotBasic" = FilterItem("plotlyPlot"),
      "plotAgg" = FilterItem("aggPlot"),
      "plotFund" = FilterItem("fundStatPlot"),
      "plotImp" = FilterItem("impPlot"),
      "plotCorr" = FilterItem("corrPlot"),
      "plotLabel" = FilterItem("labelPlot"),
      "plotClust" = FilterItem("clustering")
    ))
    output$uiPlotItems <- renderUI({uiPlot()})
    output$uiFilterItems <- renderUI({uiFilter()})
  })
  
  #
  
  observeEvent(input$deleteplotCorr, {
    req(input$deleteplotCorr)
    removeUI(
      selector = "div:has(> #corrplot2)"
    )
    removeUI(
      selector = "div:has(> #deleteplotCorr)"
    )
  })
  
  observeEvent(input$deleteplotBasic, {
    req(input$deleteplotBasic)
    removeUI(
      selector = "div:has(> #plot2)"
    )
    removeUI(
      selector = "div:has(> #deleteplotBasic)"
    )
  })
  
  observeEvent(input$deleteplotLabel, {
    req(input$deleteplotLabel)
    removeUI(
      selector = "div:has(> #labelplot2)"
    )
    removeUI(
      selector = "div:has(> #deleteplotLabel)"
    )
  })
  
  
  # MODAL
  modalVisible <- reactiveVal(FALSE)
  observeEvent(input$hideModal, modalVisible(FALSE))
  output$modal <- renderReact({
    Modal(isOpen = modalVisible(),
          Stack(tokens = list(padding = "15px", childrenGap = "10px"),
                div(style = list(display = "flex"),
                    shiny.fluent::Text("テーブル処理", variant = "large"),
                    div(style = list(flexGrow = 1)),
                    IconButton.shinyInput("hideModal", iconProps = list(iconName = "Cancel")),
                ),
                div(
                  p("テーブルUIを表示")
                )
          )
    )
  })
  
  # PANEL
  isPanelOpen <- reactiveVal(FALSE)
  mode_options <- list(
    list(key = "quick", text = "クイック"),
    list(key = "fullAuto", text = "フルオート")
  )
  
  output$reactPanel <- renderReact({
    Panel(
      headerText = "Data Robot",
      isOpen = isPanelOpen(),
      shiny.fluent::Stack(
        shiny.fluent::Text(variant = "medium", "エンドポイント"),
        TextField(),
        shiny.fluent::Text(variant = "medium", "APIキー"),
        TextField(),
        shiny.fluent::Text(variant = "medium", "プロジェクト名"),
        TextField(),
        shiny.fluent::Text(variant = "medium", "ターゲット変数"),
        TagPicker(
          onResolveSuggestions = JS("filterSuggestedTags"),
          onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
          getTextFromItem = JS("function(item) { return item.text }"),
          pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
          itemLimit = 1,
          onChange = JS("function(selection) { Shiny.setInputValue('vary', JSON.stringify(selection)) }")
        ),
        Text(variant = "medium", "オートパイロットタイプ"),
        Dropdown.shinyInput("modeoptions", value = "", options = mode_options),
        br(),
        PrimaryButton("button2", text = "送信")
      ),
      onDismiss = JS("function() { Shiny.setInputValue('hidePanel', Math.random()); }")
    )
  })
  observeEvent(input$data_robot, isPanelOpen(TRUE))
  observeEvent(input$hidePanel, isPanelOpen(FALSE))
  
  #計算式による変数追加のプレースホルダーを更新
  observeEvent(values$dat, {
    col_num <- ncol(values$dat)
    updateTextField.shinyInput(session=session, "new_col", defaultValue = paste0("V", col_num+1))
    updateTextField.shinyInput(session=session, "new_col_strs", defaultValue = paste0("V", col_num+1))
    updateTextField.shinyInput(session=session, "new_col_roll", defaultValue = paste0("V", col_num+1))
    updateTextField.shinyInput(session=session, "new_col_str_detect", defaultValue = paste0("V", col_num+1))
    updateTextField.shinyInput(session=session, "new_col_str_sub", defaultValue = paste0("V", col_num+1))
  })
  
  observeEvent(input$colnameOnehot, {
    if (jsonConvert(input$colnameOnehot)!=""){
      DT::selectRows(values$proxyColumnInfo, selected=which(colnames(values$dat)==jsonConvert(input$colnameOnehot)))
    }
    
  })
  observeEvent(input$colnameNAs, {
    if (jsonConvert(input$colnameNAs)!=""){
      DT::selectRows(values$proxyColumnInfo, selected=which(colnames(values$dat)==jsonConvert(input$colnameNAs)))
    }
  })
  observeEvent(input$colnameChange, {
    if (jsonConvert(input$colnameChange)!=""){
      DT::selectRows(values$proxyColumnInfo, selected=which(colnames(values$dat)==jsonConvert(input$colnameChange)))
    }
  })
  
  observeEvent(input$colnameSplit, {
    if (input$colnameSplit!=""){
      DT::selectRows(values$proxyColumnInfo, selected=which(colnames(values$dat)==jsonConvert(input$colnameSplit)))
    }
  })
  
  observeEvent(input$insertcolname, {
    updateTextField.shinyInput(session=session, "formula", value = paste0(input$formula, jsonConvert(input$colnameCalc)))
  })
  
  #変数の型変換
  observeEvent(input$class_change, {
    result <- classChange(values$dat,jsonConvert(input$colname_classchange),input$target_class)
    if (class(result[[1]]) != "try-error") {
      
      action_list <- list('class_change',jsonConvert(input$colname_classchange),input$target_class)
      values$history <- c(values$history,list(action_list))
      
      updateData(result[[2]], input, output, session, values)
      appendIcon("class_change", iconHistory, output)
    }
  })
  
  #文字列から日付、日付時刻型への変換
  observeEvent(input$dttm_change, {
    result <- changeDttm(values$dat,jsonConvert(input$colname_dttmchange),input$target_dttm_class,input$dttm_format)
    if (class(result[[1]]) != "try-error") {
      
      action_list <- list('dttm_change',jsonConvert(input$colname_dttmchange),input$target_dttm_class,input$dttm_format)
      values$history <- c(values$history,list(action_list))
      
      updateData(result[[2]], input, output, session, values)
      appendIcon("dttm_change", iconHistory, output)
    }
  })
  
  #行の並べ替え
  observeEvent(input$arrange_row, {
    df <- arrangeRows(values$dat,jsonConvert(input$colname_arrange),input$a_or_d)
    
    action_list <- list('arrange_row',jsonConvert(input$colname_arrange),input$a_or_d)
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
    appendIcon("arrange_row", iconHistory, output)
  })
  
  #行のフィルタリング
  observeEvent(input$filter_row, {
    result <- filterRows(values$dat,jsonConvert(input$colname_filter),input$filter_operator,input$filter_value)
    if (class(result[[1]]) != "try-error") {  
      
      action_list <- list('filter_row',jsonConvert(input$colname_filter),input$filter_operator,input$filter_value)
      values$history <- c(values$history,list(action_list))
      
      updateData(result[[2]], input, output, session, values)
      appendIcon("filter_row", iconHistory, output)
    }
  })
  
  #変数選択
  observeEvent(input$select_columns,if(!is.null(input$colname_select)) {
    df <- selectColumns(values$dat,jsonConvert(input$colname_select))
    
    action_list <- list('select_columns', jsonConvert(input$colname_select))
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
    appendIcon("select_columns", iconHistory, output)
  })
  
  # onehot化
  observeEvent(input$convertOnehot, {
    req(input$minfracOnehot)
    #処置前に元に戻る用データを更新
    values$dat_back <- values$dat
    
    #onehot処理のモジュール
    #########################
    csv_data <- values$dat
    colname <- input$colnameOnehot
    
    forwards <- getForwardColumns(csv_data, colname)
    backwards <- getBackwardColumns(csv_data, colname)
    
    #onehot変換の前にファクタ化する #中村
    csv_data[[colname]] <- as.factor(csv_data[[colname]])
    
    treatments <- try(designTreatmentsZ(csv_data, colname, codeRestriction=c('lev'), verbose=FALSE, minFraction = input$minfracOnehot),silent = TRUE)
    if (class(treatments) != "try-error") {
      renames <- shortenVarName(treatments[2]$scoreFrame)
      # print(renames)c
      
      prep <- prepare(treatments, csv_data)
      #エラー処理(新たに作成された変数がない場合)
      if(nrow(renames) != 0){
        for (i in 1:nrow(renames)){
          prep %<>% renameColumn(.,renames[i, "varName"], renames[i, "newName"])
        }
        csv_data <- cbind(forwards, prep, backwards)
        #作業の保存
        action_list <- list('Onehot',colname,treatments)
        values$history <- c(values$history,list(action_list))
        
        updateData(csv_data, input, output, session, values)
      }
    }
  })
  
  #ターゲットエンコーディング
  observeEvent(input$target_encode, {
    result <- targetEncode(values$dat, jsonConvert(input$tar_groupby),jsonConvert(input$tar_agg_colname),input$tar_new_col,input$tar_aggregation)
    
    action_list <- list('target_encode',result[[2]],jsonConvert(input$tar_groupby),input$tar_new_col)
    values$history <- c(values$history,list(action_list))
    
    updateData(result[[1]], input, output, session, values)
    
  })
  
  #ラグ変数生成
  observeEvent(input$make_lag, {
    df <- makeLag(values$dat,input$num_lag,jsonConvert(input$colname_lag))
    
    action_list <- list('make_lag',input$num_lag,jsonConvert(input$colname_lag))
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
  })
  
  #リード変数生成
  observeEvent(input$make_lead, {
    df <- makeLead(values$dat,input$num_lead,jsonConvert(input$colname_lead))
    
    action_list <- list('make_lead',input$num_lead,jsonConvert(input$colname_lead))
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
  })
  
  observeEvent(input$make_roll, if(jsonConvert(input$colname_roll)!='None'){
    df <- makeRoll(values$dat,input$new_col_roll, jsonConvert(input$colname_roll),jsonConvert(input$time_index_roll),input$aggmethod_roll,input$before_roll,input$after_roll,input$roll_type)
    
    action_list <- list('make_roll',input$new_col_roll,jsonConvert(input$colname_roll),jsonConvert(input$time_index_roll),input$aggmethod_roll,input$before_roll,input$after_roll,input$roll_type)
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
  })
  
  #移動集計
  observeEvent(input$make_roll, if(jsonConvert(input$colname_roll) !='None'){
    df <- makeRoll(values$dat,input$new_col_roll,jsonConvert(input$colname_roll),input$time_index_roll,input$aggmethod_roll,input$before_roll,input$after_roll,input$roll_type)
    
    action_list <- list('make_roll',input$new_col_roll,jsonConvert(input$colname_roll),input$time_index_roll,input$aggmethod_roll,input$before_roll,input$after_roll,input$roll_type)
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
  })
  
  # 欠損値処理
  observeEvent(input$dealNAs, if(input$colnameNAs !='None'){
    result <- dealNumericna(values$dat, jsonConvert(input$colnameNAs),input$dealna_type,input$make_na_col,input$na_target_value,target_value = NULL)
    csv_data <- result[[1]]
    #########################
    
    #作業の保存
    action_list <- list('dealNAs',jsonConvert(input$colnameNAs),input$dealna_type,input$make_na_col,input$na_target_value,result[[2]])
    values$history <- c(values$history,list(action_list))
    
    #データテーブルの更新
    updateData(csv_data, input, output, session, values)
  })
  
  observeEvent(input$dealNAs_str, if(jsonConvert(input$colnameNAs_str) !='None'){
    
    csv_data <- dealStrna(values$dat,jsonConvert(input$colnameNAs_str),input$na_rep_str)
    
    #作業の保存
    action_list <- list('dealNAs_str',jsonConvert(input$colnameNAs_str),input$na_rep_str)
    values$history <- c(values$history,list(action_list))
    
    #データテーブルの更新
    updateData(csv_data, input, output, session, values)
  })
  
  
  
  # onehot, 欠損値の一括処理
  observeEvent(input$dealAll, {
    req(input$minfracAll)
    
    #処置前に元に戻る用データを更新
    values$dat_back <- values$dat
    csv_data <- values$dat
    
    cols <- colnames(csv_data)
    
    treatments <- try(designTreatmentsZ(csv_data, cols, codeRestriction=c('lev', 'clean', 'isBAD'), verbose=FALSE, minFraction = input$minfracAll),silent = TRUE)
    
    if (class(treatments) != "try-error") {
      treatments[2]$scoreFrame %<>% sortScoreFrameByOrigName(., csv_data)
      renames <- shortenVarName(treatments[2]$scoreFrame)
      prep <- prepare(treatments, csv_data)
      
      
      #新たに作成された変数がある場合のみデータテーブルを更新
      if(nrow(renames) != 0){
        for (i in 1:nrow(renames)){
          prep %<>% renameColumn(.,renames[i, "varName"], renames[i, "newName"])
        }
        #作業の保存
        action_list <- list('dealAll',treatments)
        values$history <- c(values$history,list(action_list))
        
        updateData(prep, input, output, session, values)
        
      }
    }
  })
  
  
  #列名の変更
  observeEvent(input$nameChange, {

    #変更後の列名が存在しない場合のみ更新
    if ((!is.element(input$newName, colnames(values$dat)))&&(input$newName!="")){
      
      #処置前に元に戻る用データを更新
      values$dat_back <- values$dat
      
      #作業の保存
      action_list <- list('nameChange',jsonConvert(input$colnameChange),input$newName)
      values$history <- c(values$history,list(action_list))
      
      updateData(renameColumn(values$dat, jsonConvert(input$colnameChange), input$newName), input, output, session, values)
      appendIcon("nameChange", iconHistory, output)
    }
  })
  
  
  # 新しい列を作成
  observeEvent(input$calc, {

    #処置前に元に戻る用データを一時保存
    dat_back <- values$dat
    
    #新しい変数名が入力されていないときはV+変数番号を変数名とする
    if(input$new_col==""){
      col_num <- ncol(values$dat)
      new_col <- paste0("V", col_num+1)
    }
    
    error_check <- try(updateData(calc(new_col, input$formula, values$dat), input, output, session, values))
    if (class(error_check) == "try-error") {
      sendSweetAlert(
        session = session,
        title = NULL,
        text = "入力された式に誤りがあります",
        type = "warning"
      )
      #エラーなしの場合
    }else{
      #処置前に元に戻る用データを更新
      values$dat_back <- dat_back
      #作業の保存
      action_list <- list('calc',new_col,input$formula)
      values$history <- c(values$history,list(action_list))
    }
  })
  
  # 列の削除
  observeEvent(input$columnDelete,
               if(!is.null(jsonConvert(input$colnameDelete))&&(length(jsonConvert(input$colnameDelete))!=ncol(values$dat))) {

                 # #処置前に元に戻る用データを更新
                 # values$dat_back <- values$dat
                 
                 #作業の保存
                 action_list <- list('columnDelete',jsonConvert(input$colnameDelete))
                 values$history <- c(values$history,list(action_list))
                 
                 updateData(removeColumn(values$dat, jsonConvert(input$colnameDelete)), input, output, session, values)
                 appendIcon("columnDelete", iconHistory, output)
               })
  
  
  
  ###中村コード追加
  
  #文字列の結合
  observeEvent(input$columnCombine,if(!is.null(jsonConvert(input$colnameCombine1))){
    
    #処置前に元に戻る用データを更新
    values$dat_back <- values$dat
    
    #新しい変数名が入力されていないときはV+変数番号を変数名とする
    if(input$new_col_strc==""){
      col_num <- ncol(values$dat)
      new_col <- paste0("V", col_num+1)
    }else{
      new_col <- input$new_col_strc
    }
    
    #作業の保存
    # action_list <- list('columnCombine',new_col,input$c_sep_token,input$colnameCombine1,input$colnameCombine2)
    # values$history <- c(values$history,list(action_list))
    # 
    # df <- combine_str(new_col,input$c_sep_token,values$dat,input$colnameCombine1,input$colnameCombine2)
    
    df <- combineStr(values$dat, new_col, input$c_sep_token ,jsonConvert(input$colnameCombine1))
    action_list <- list('columnCombine',new_col, input$c_sep_token ,jsonConvert(input$colnameCombine1))
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
  })
  
  #文字列の分割
  observeEvent(input$columnSplit, {

    #処置前に元に戻る用データを更新
    values$dat_back <- values$dat
    
    #新しい変数名が入力されていないときはV+変数番号を変数名とする
    if(input$new_col_strs==""){
      col_num <- ncol(values$dat)
      new_col <- paste0("V", col_num+1)
    }
    
    #作業の保存
    action_list <- list('columnSplit',new_col,input$rorl,input$s_sep_token,jsonConvert(input$colnameSplit))
    values$history <- c(values$history,list(action_list))
    
    updateData(split_str(new_col,input$rorl,input$s_sep_token,values$dat,jsonConvert(input$colnameSplit)), 
               input, output, session, values)
  })
  
  #文字列の含有判定
  observeEvent(input$str_detect, {
    df <- strDetect(values$dat,input$new_col_str_detect,jsonConvert(input$colname_str_detect),input$partial_string)
    
    #作業の保存
    action_list <- list('str_detect',input$new_col_str_detect,jsonConvert(input$colname_str_detect),input$partial_string)
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
  })
  
  #文字列の抽出
  observeEvent(input$str_substract, {
    df <- strSub(values$dat,input$new_col_str_sub,jsonConvert(input$colname_str_sub),input$str_sub_start,input$str_sub_end)
    
    #作業の保存
    action_list <- list('str_substract',input$new_col_str_sub,jsonConvert(input$colname_str_sub),input$str_sub_start,input$str_sub_end)
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
  })
  
  #文字列の置換
  observeEvent(input$str_replace, {
    df <- strReplace(values$dat,jsonConvert(input$colname_str_rep),input$str_rep_before,input$str_rep_after)
    
    #作業の保存
    action_list <- list('str_replace',jsonConvert(input$colname_str_rep),input$str_rep_before,input$str_rep_after)
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
  })
  
  #日付時刻データからの要素抽出
  observeEvent(input$dttm_sub,if(input$colname_dttm_sub !='None'){
    df <- dttmSub(values$dat,input$dttm_sub_new_col,jsonConvert(input$colname_dttm_sub),input$dttm_sub_element)
    
    #作業の保存
    action_list <- list('dttm_sub',input$dttm_sub_new_col,jsonConvert(input$colname_dttm_sub),input$dttm_sub_element)
    values$history <- c(values$history,list(action_list))
    
    updateData(df, input, output, session, values)
  })
  
  observeEvent(values$dat, {
    req(input$upload, input$dataDrop)
    
    c_name <- as.data.frame(colnames(values$dat))
    datatype <- as.data.frame(t(as.data.frame(lapply(values$dat, class))))[,1]
    datatype <- as.data.frame(cbind(c_name,datatype))
    
    rownames(datatype) <- NULL
    colnames(datatype) <- c('変数名','変数型')
    
    datatype_num_int <- datatype[datatype[['変数型']]=='numeric'|datatype[['変数型']]=='integer',]
    datatype_char_fact <- datatype[datatype[['変数型']]=='factor'|datatype[['変数型']]=='character',]
    
    colInfo_char_fact <- datatype_char_fact[['変数名']]
    colInfo_num_int <- datatype_num_int[['変数名']]
    
    #ユニーク値の数が5以下の文字列・ファクタ変数
    
    unique_count_df <- as.data.frame(t(as.data.frame(lapply(values$dat[colInfo_char_fact], unique_count))))
    
    if(ncol(unique_count_df) != 0){
      colnames(unique_count_df) <- c("ユニーク数")
    }
    
    if(nrow(unique_count_df) != 0){
      colInfo_char_fact_u5 <- unique_count_df %>% filter(ユニーク数<=5) %>% row.names()
    }else{
      colInfo_char_fact_u5 <- c()
    }
    
    colInfo_target <- union(colInfo_num_int,colInfo_char_fact_u5)
    
    colInfo <- colnames(values$dat)
    
    
    session$sendCustomMessage("colnames", colInfo)
  })
  
  #グラフプロットの仕様を変更するUI
  observeEvent(values$dat,{
    output$graphcontrols <- renderUI({
      output = tagList()
      
      #Y変数の入力がない場合(ヒストグラムか棒グラフの場合)
      if(is.null(input$vary)){
        req(input$varx)
        #ヒストグラム or カーネル密度推定
        if(class(values$dat[[jsonConvert(input$varx)]]) == 'numeric'){
          output[[1]] = radioGroupButtons("histogramtype", "表示オプション", c('カウント','カーネル密度','カーネル密度+ヒストグラム'), selected = values$histogramtype,justified = TRUE)
          values$show <- TRUE
          output
          
          # 棒グラフオプション(横並び、積み上げ、割合)
        }else{
          output[[1]] = radioGroupButtons("bartype", "表示オプション", c('カウント(横並び)','カウント(積み上げ)','割合'), selected = values$bartype,justified = TRUE)
          values$show <- FALSE
          output
        }
        
        #Y変数の入力がある場合(散布図の場合)
        
      }else{
        
        #X変数,Y変数が共に数値型あるいは日付時刻型の場合
        if((class(values$dat[[jsonConvert(input$varx)]]) == 'numeric' | class(values$dat[[jsonConvert(input$varx)]]) == 'integer'|class(values$dat[[jsonConvert(input$varx)]]) == 'Date'|class(values$dat[[jsonConvert(input$varx)]])[1] == 'POSIXct') && 
           (class(values$dat[[jsonConvert(input$vary)]]) == 'numeric'| class(values$dat[[jsonConvert(input$vary)]]) == 'integer'|class(values$dat[[jsonConvert(input$varx)]]) == 'Date'|class(values$dat[[jsonConvert(input$varx)]])[1] == 'POSIXct' )){
          output[[1]] = radioGroupButtons("smoothlabel", "近似曲線", c('無','有'), selected = values$smoothlabel,justified = TRUE)
          output[[2]] = radioGroupButtons("pointorline", "グラフ種類", c('散布図','ジッター','折れ線'), selected = values$pointorline,justified = TRUE)
          values$show <- FALSE
          output
          
          #X変数,Y変数のどちらかがファクター変数の場合 
        }else if((class(values$dat[[jsonConvert(input$varx)]]) == 'factor' | class(values$dat[[jsonConvert(input$vary)]]) == 'factor')|
                 (class(values$dat[[jsonConvert(input$varx)]]) == 'character' | class(values$dat[[jsonConvert(input$vary)]]) == 'character')){
          # output[[1]] = radioGroupButtons("jitterlabel", "ジッター有無", c('無','有'), selected = values$jitterlabel,justified = TRUE)
          output[[2]] = radioGroupButtons("graphtype2", "グラフ種類", c('散布図','ジッター','ボックス','ボックス+ジッター'), selected = values$graphtype2,justified = TRUE)
          output
        }
      }
    })
  })
  
  output$conditionalcontrols <- renderUI({
    
    #input$histogramtypeとvalues$graphtypeが揃った段階で処理を実行する
    req(input$histogramtype,values$graphtype)
    if((input$histogramtype == "カウント"|input$histogramtype =='カーネル密度+ヒストグラム') && values$graphtype =="ヒストグラム"){
      output = tagList()
      output[[1]] = sliderInput("histbin",
                                label = "ビン数:",
                                min = 1,
                                max = 50,
                                value = 30,width = '100%',step = NULL,ticks = FALSE)
      output
    }
    
  })
  
  observeEvent(input$histogramtype, {
    #現在のヒストグラムタイプを保存しておく。
    values$histogramtype <- input$histogramtype
  })
  
  observeEvent(input$modetype, {
    #現在のMax/Min/Max+Minタイプを保存しておく。
    values$modetype <- input$modetype
  })
  
  
  observeEvent(input$bartype, {
    #現在の棒グラフタイプを保存しておく。
    values$bartype <- input$bartype
  })
  
  observeEvent(input$smoothlabel, {
    #現在の近似曲線の有無を保存しておく。
    values$smoothlabel <- input$smoothlabel
  })
  
  
  observeEvent(input$pointorline, {
    #現在のグラフタイプを保存しておく。
    values$pointorline <- input$pointorline
  })
  
  observeEvent(input$jitterlabel, {
    #現在のジッター有無を保存しておく。
    values$jitterlabel <- input$jitterlabel
  })
  
  observeEvent(input$graphtype2, {
    #現在のジッター有無を保存しておく。
    values$graphtype2 <- input$graphtype2
  })
  
  #Rio: Max/Min/Max+Minの表に更新
  observeEvent(input$make_label, {
    if (jsonConvert(input$varx_time) == jsonConvert(input$vary_time)) {
      sendSweetAlert(
        session = session,
        title = NULL,
        text = "X軸のデータとY軸のデータが同一です。",
        type = "warning"
      )
    } else if (is.null(input$varx_time)) {
      sendSweetAlert(
        session = session,
        title = NULL,
        text = "X軸の変数を入力してください",
        type = "warning"
      )
    } else if (is.null(input$vary_time)){
      sendSweetAlert(
        session = session,
        title = NULL,
        text = "Y軸の変数を入力してください",
        type = "warning"
      )
    } else {
      df <- makeEventLabel(dat=values$dat, x=jsonConvert(input$varx_time), y=jsonConvert(input$vary_time), w=as.numeric(input$window_size), span=as.numeric(input$span), type=input$modetype)
      action_list <- list('make_label',input$varx_time, input$vary_time, input$window_size, input$span, input$modetype)
      values$history <- c(values$history,list(action_list))
      updateData(df, input, output, session, values)
    }
  })
  
  #変数入力が変更されたら、グラフを作成する。変数Xが入力されたら発火する。初期varxが""の時は処理しない。
  observeEvent(input$plotlyPlotButton,
               if(!is.null(jsonConvert(input$varx)))  {
                 output$plot <- renderPlot({
                   
                   theme <- theme_gray(base_family = "HiraKakuPro-W3",base_size = 15)
                   
                   #変数yが入力されている場合
                   if(!is.null(input$vary)){
                     
                     values$graphtype <- "散布図"
                     
                     #X,Yとも数値入力の場合(近似曲線の表示あり)
                     if((class(values$dat[[jsonConvert(input$varx)]]) != 'factor' && class(values$dat[[jsonConvert(input$varx)]]) != 'character' )
                        && (class(values$dat[[jsonConvert(input$vary)]]) != 'factor'&& class(values$dat[[jsonConvert(input$vary)]]) != 'character' )){ 
                       
                       #input$smoothlabelが入力されるまで待機
                       req(input$smoothlabel,input$pointorline)
                       switch(input$smoothlabel,
                              "有" =  smooth <- geom_smooth(se = FALSE),
                              "無" =  smooth <- NULL
                       )
                       
                       switch(input$pointorline,
                              "散布図" =  temp_plot <- geom_point(),
                              "ジッター" =  temp_plot <- geom_point(position = position_jitter()),
                              "折れ線" =  temp_plot <- geom_line(),
                       )
                       jitter <- NULL
                       
                       #X,Yのどちらかがファクター/文字列入力の場合(近似曲線を表示しない)  
                     }else{
                       # req(input$jitterlabel)
                       # smooth <- NULL
                       # switch(input$jitterlabel,
                       #        "有" =  temp_plot <- geom_point(position = position_jitter()),
                       #        "無" =  temp_plot <- geom_point(),
                       # )
                       req(input$graphtype2)
                       smooth <- NULL
                       jitter <- NULL
                       switch(input$graphtype2,
                              "散布図" =  temp_plot <- geom_point(),
                              "ジッター" =  temp_plot <- geom_point(position = position_jitter()),
                              "ボックス" =  temp_plot <- geom_boxplot(),
                              "ボックス+ジッター" =  temp_plot <- geom_boxplot(),
                       )
                       
                       if (input$graphtype2 =="ボックス+ジッター"){
                         jitter <- geom_jitter()
                       }
                     }
                     
                     p <- ggplot(values$dat, aes(x = .data[[jsonConvert(input$varx)]], y = .data[[jsonConvert(input$vary)]] ),na.rm = TRUE)+
                       temp_plot+
                       xlab(jsonConvert(input$varx)) + 
                       ylab(jsonConvert(input$vary)) +
                       theme+smooth+jitter
                     
                     #色あり
                     if(!is.null(input$varc)){
                       p <- p+aes(colour = .data[[jsonConvert(input$varc)]])+labs(color=jsonConvert(input$varc))
                     }
                     
                     #ファセットあり
                     if(!is.null(input$varf)){
                       p <- p+facet_wrap(~ .data[[jsonConvert(input$varf)]])
                     }  
                     
                     print(p)
                     
                     #変数yが入力されていない場合  
                   }else{
                     
                     #変数xが整数あるいはカテゴリの場合、棒グラフを表示する。
                     if(class(values$dat[[jsonConvert(input$varx)]]) != 'numeric'){
                       req(input$bartype)
                       values$graphtype <- "棒グラフ"
                       
                       #表示オプションに応じてgeom_bar内のオプションを変更
                       switch(input$bartype,
                              "カウント(横並び)"= temp_plot <-  geom_bar(position = "dodge", alpha = 1) ,
                              "カウント(積み上げ)"= temp_plot <-  geom_bar(position = "stack", alpha = 1),
                              "割合" = temp_plot <-  geom_bar(position = "fill", alpha = 1),
                       )
                       
                       #表示オプションに応じてY軸のラベル変更
                       switch(input$bartype,
                              "カウント(横並び)"= ylabel <-  ylab('count'),
                              "カウント(積み上げ)"=ylabel <-  ylab('count'),
                              "割合" = ylabel <-  ylab('weight'),
                       )
                       
                       p <- ggplot(values$dat, aes(x = .data[[jsonConvert(input$varx)]]))+
                         temp_plot+ylabel+xlab(jsonConvert(input$varx))+theme
                       
                       #色あり
                       if(!is.null(input$varc)){
                         p <- p + aes(fill = as.factor(.data[[jsonConvert(input$varc)]])) + labs(fill=jsonConvert(input$varc))
                       }
                       #ファセットあり
                       if(!is.null(input$varf)){
                         p <- p + facet_wrap(~ .data[[jsonConvert(input$varf)]])
                       }
                       
                       print(p)
                       
                       #変数xが数値型の場合、ヒストグラムを表示する。    
                     }else{   
                       
                       values$graphtype <- "ヒストグラム"
                       #histogramtypeが入力されていない時は処理を止める
                       req(input$histogramtype,input$histbin)
                       
                       alpha <- ifelse(jsonConvert(input$varc) == jsonConvert(input$varf), 0.7, 0.5)
                       
                       switch(input$histogramtype,
                              "カウント"= temp_plot <-  geom_histogram(position = "identity", alpha = alpha,bins = input$histbin),
                              "カーネル密度"= temp_plot <-  geom_density(position = "identity", alpha = alpha),
                              "カーネル密度+ヒストグラム" = temp_plot <-  geom_density(position = "identity", alpha = 0.2),
                       )
                       
                       p <- ggplot(values$dat, aes(x = .data[[jsonConvert(input$varx)]]))+
                         xlab(jsonConvert(input$varx))+theme+temp_plot
                       
                       if(input$histogramtype=="カーネル密度+ヒストグラム"){
                         p <- p + geom_histogram(aes(y = ..density..),position = "identity", alpha = alpha,bins = input$histbin)
                       }
                       
                       
                       #色あり
                       if(!is.null(input$varc)){
                         p <- p+aes(fill= as.factor(.data[[jsonConvert(input$varc)]]))+
                           labs(fill=input$varc)
                       }
                       
                       #ファセットあり 
                       if(!is.null(input$varf)){
                         p <- p + facet_wrap(~ .data[[jsonConvert(input$varf)]])
                       }
                       print(p)
                     }
                   }
                 }, res = 96)
                 
                 output$plot2 <- renderPlot({
                   
                   theme <- theme_gray(base_family = "HiraKakuPro-W3",base_size = 15)
                   
                   #変数yが入力されている場合
                   if(!is.null(input$vary)){
                     
                     values$graphtype <- "散布図"
                     
                     #X,Yとも数値入力の場合(近似曲線の表示あり)
                     if((class(values$dat[[jsonConvert(input$varx)]]) != 'factor' && class(values$dat[[jsonConvert(input$varx)]]) != 'character' )
                        && (class(values$dat[[jsonConvert(input$vary)]]) != 'factor'&& class(values$dat[[jsonConvert(input$vary)]]) != 'character' )){ 
                       
                       #input$smoothlabelが入力されるまで待機
                       req(input$smoothlabel,input$pointorline)
                       switch(input$smoothlabel,
                              "有" =  smooth <- geom_smooth(se = FALSE),
                              "無" =  smooth <- NULL
                       )
                       
                       switch(input$pointorline,
                              "散布図" =  temp_plot <- geom_point(),
                              "ジッター" =  temp_plot <- geom_point(position = position_jitter()),
                              "折れ線" =  temp_plot <- geom_line(),
                       )
                       jitter <- NULL
                       
                       #X,Yのどちらかがファクター/文字列入力の場合(近似曲線を表示しない)  
                     }else{
                       # req(input$jitterlabel)
                       # smooth <- NULL
                       # switch(input$jitterlabel,
                       #        "有" =  temp_plot <- geom_point(position = position_jitter()),
                       #        "無" =  temp_plot <- geom_point(),
                       # )
                       req(input$graphtype2)
                       smooth <- NULL
                       jitter <- NULL
                       switch(input$graphtype2,
                              "散布図" =  temp_plot <- geom_point(),
                              "ジッター" =  temp_plot <- geom_point(position = position_jitter()),
                              "ボックス" =  temp_plot <- geom_boxplot(),
                              "ボックス+ジッター" =  temp_plot <- geom_boxplot(),
                       )
                       
                       if (input$graphtype2 =="ボックス+ジッター"){
                         jitter <- geom_jitter()
                       }
                     }
                     
                     p <- ggplot(values$dat, aes(x = .data[[jsonConvert(input$varx)]], y = .data[[jsonConvert(input$vary)]] ),na.rm = TRUE)+
                       temp_plot+
                       xlab(jsonConvert(input$varx)) + 
                       ylab(jsonConvert(input$vary)) +
                       theme+smooth+jitter
                     
                     #色あり
                     if(!is.null(input$varc)){
                       p <- p+aes(colour = .data[[jsonConvert(input$varc)]])+labs(color=jsonConvert(input$varc))
                     }
                     
                     #ファセットあり
                     if(!is.null(input$varf)){
                       p <- p+facet_wrap(~ .data[[jsonConvert(input$varf)]])
                     }  
                     
                     print(p)
                     
                     #変数yが入力されていない場合  
                   }else{
                     
                     #変数xが整数あるいはカテゴリの場合、棒グラフを表示する。
                     if(class(values$dat[[jsonConvert(input$varx)]]) != 'numeric'){
                       req(input$bartype)
                       values$graphtype <- "棒グラフ"
                       
                       #表示オプションに応じてgeom_bar内のオプションを変更
                       switch(input$bartype,
                              "カウント(横並び)"= temp_plot <-  geom_bar(position = "dodge", alpha = 1) ,
                              "カウント(積み上げ)"= temp_plot <-  geom_bar(position = "stack", alpha = 1),
                              "割合" = temp_plot <-  geom_bar(position = "fill", alpha = 1),
                       )
                       
                       #表示オプションに応じてY軸のラベル変更
                       switch(input$bartype,
                              "カウント(横並び)"= ylabel <-  ylab('count'),
                              "カウント(積み上げ)"=ylabel <-  ylab('count'),
                              "割合" = ylabel <-  ylab('weight'),
                       )
                       
                       p <- ggplot(values$dat, aes(x = .data[[jsonConvert(input$varx)]]))+
                         temp_plot+ylabel+xlab(jsonConvert(input$varx))+theme
                       
                       #色あり
                       if(!is.null(input$varc)){
                         p <- p + aes(fill = as.factor(.data[[jsonConvert(input$varc)]])) + labs(fill=jsonConvert(input$varc))
                       }
                       #ファセットあり
                       if(!is.null(input$varf)){
                         p <- p + facet_wrap(~ .data[[jsonConvert(input$varf)]])
                       }
                       
                       print(p)
                       
                       #変数xが数値型の場合、ヒストグラムを表示する。    
                     }else{   
                       
                       values$graphtype <- "ヒストグラム"
                       #histogramtypeが入力されていない時は処理を止める
                       req(input$histogramtype,input$histbin)
                       
                       alpha <- ifelse(jsonConvert(input$varc) == jsonConvert(input$varf), 0.7, 0.5)
                       
                       switch(input$histogramtype,
                              "カウント"= temp_plot <-  geom_histogram(position = "identity", alpha = alpha,bins = input$histbin),
                              "カーネル密度"= temp_plot <-  geom_density(position = "identity", alpha = alpha),
                              "カーネル密度+ヒストグラム" = temp_plot <-  geom_density(position = "identity", alpha = 0.2),
                       )
                       
                       p <- ggplot(values$dat, aes(x = .data[[jsonConvert(input$varx)]]))+
                         xlab(jsonConvert(input$varx))+theme+temp_plot
                       
                       if(input$histogramtype=="カーネル密度+ヒストグラム"){
                         p <- p + geom_histogram(aes(y = ..density..),position = "identity", alpha = alpha,bins = input$histbin)
                       }
                       
                       
                       #色あり
                       if(!is.null(input$varc)){
                         p <- p+aes(fill= as.factor(.data[[jsonConvert(input$varc)]]))+
                           labs(fill=input$varc)
                       }
                       
                       #ファセットあり 
                       if(!is.null(input$varf)){
                         p <- p + facet_wrap(~ .data[[jsonConvert(input$varf)]])
                       }
                       print(p)
                     }
                   }
                 }, res = 96)
               })
  
  observeEvent(values$dat, {
    #現在入力されている変数名
    # selected <- input$variables
    
    output$datatypes <- DT::renderDataTable({
      c_name <- as.data.frame(colnames(values$dat))
      
      datatype <- as.data.frame(t(as.data.frame(lapply(values$dat, class))))[,1]
      
      datatype <- as.data.frame(cbind(c_name,datatype))
      
      rownames(datatype) <- NULL
      colnames(datatype) <- c('変数名','変数型')
      
      # print(datatype)
      # print(datatype[datatype['変数型']=='numeric'])
      # print(typeof(datatype))
      
      # datatype[datatype[['変数型']]=='numeric','変数型'] <- '実数'
      # datatype[datatype[['変数型']]=='integer' ,'変数型']<- '整数'
      # datatype[datatype[['変数型']]== 'factor' ,'変数型'] <- 'カテゴリ'
      
      
      # datatypes <- datatype %>% tibble::rownames_to_column(var = "変数名")
      datatypes <- datatype
      values$dattypes <- datatypes
      datatypes
    },rownames = T,extensions = c('Scroller'),colnames = c(No. = 1),
    options = list(dom = 'fti',scrollY = 400,scroller = TRUE,
                   initComplete = DT::JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({});",
                     "}")),
    # selection=list(mode="single", selected=getColumnIndexbyName(values$dat, selected)),
    selection=list(mode="single", selected=getColumnIndexbyName(values$dat, names(values$dat)[1])),
    )
    
    
    #統計量要約の表示
    output$summary <- DT::renderDataTable({
      
      df <- skimr::skim(values$dat) %>% as.data.frame()
      col_length <- df %>% colnames() %>% length()
      c(2,1,3:col_length)
      
      df <- df[c(2,1,3:col_length)]
      
      colname <- df %>% colnames()
      colnames(df) <- c("varName","type",colname[3:length(colname)])
      df
      
    },extensions = c('Scroller'), rownames = TRUE,class = 'cell-border stripe',colnames = c(No. = 1),
    options = list(
      autoWidth = TRUE,
      fixedColumns = list(leftColumns = 3),
      dom = 'rti',
      scrollY = 500,
      scrollX = TRUE,
      scroller = TRUE,
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#1f618d', 'color': '#fff'});",
        "}")
    ))
    
    output$summary2 <- DT::renderDataTable({
      
      df <- skimr::skim(values$dat) %>% as.data.frame()
      col_length <- df %>% colnames() %>% length()
      c(2,1,3:col_length)
      
      df <- df[c(2,1,3:col_length)]
      
      colname <- df %>% colnames()
      colnames(df) <- c("varName","type",colname[3:length(colname)])
      df
      
    },extensions = c('Scroller'), rownames = TRUE,class = 'cell-border stripe',colnames = c(No. = 1),
    options = list(
      autoWidth = TRUE,
      fixedColumns = list(leftColumns = 3),
      dom = 'rti',
      scrollY = 500,
      scrollX = TRUE,
      scroller = TRUE,
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#1f618d', 'color': '#fff'});",
        "}")
    ))
    
    # output$summary <- DT::renderDataTable({
    #   
    #   tableplot <- values$dat %>%
    #     summary() %>%                       # なにはともあれsummary()
    #     rbind() %>%                         # matrix化
    #     magrittr::set_rownames(NULL) %>%    # 行名""を削除
    #     as.tibble() %>%                     # tbl_df化
    #     map_dfr(~ str_trim(.)) %>%          # 要素の前後空白を削除
    #     set_names(str_trim(names(.)))
    #   
    #   tableplot
    #   
    # })
    
  })
  
  #変数相関プロットの表示
  output$corrplot<- renderPlot({
    
    #データフレームが空でない場合に相関表示
    if(nrow(values$dat)!=0){
      c_name <- as.data.frame(colnames(values$dat))
      datatype <- as.data.frame(t(as.data.frame(lapply(values$dat, class))))[,1]
      datatype <- as.data.frame(cbind(c_name,datatype))
      
      rownames(datatype) <- NULL
      colnames(datatype) <- c('変数名','変数型')
      
      datatype <- datatype[datatype[['変数型']]=='numeric'|datatype[['変数型']]=='integer',]
      colInfo_num_int <- datatype[['変数名']]
      
      df <- values$dat[colInfo_num_int]
      df_corr <- cor(df,use = "complete.obs")
      
      print(corrplot::corrplot(df_corr,tl.col="black", tl.srt=45,cl.pos = 'b'))
    }
    
  },res = 96)
  
  output$corrplot2<- renderPlot({
    
    #データフレームが空でない場合に相関表示
    if(nrow(values$dat)!=0){
      c_name <- as.data.frame(colnames(values$dat))
      datatype <- as.data.frame(t(as.data.frame(lapply(values$dat, class))))[,1]
      datatype <- as.data.frame(cbind(c_name,datatype))
      
      rownames(datatype) <- NULL
      colnames(datatype) <- c('変数名','変数型')
      
      datatype <- datatype[datatype[['変数型']]=='numeric'|datatype[['変数型']]=='integer',]
      colInfo_num_int <- datatype[['変数名']]
      
      df <- values$dat[colInfo_num_int]
      df_corr <- cor(df,use = "complete.obs")
      
      print(corrplot::corrplot(df_corr,tl.col="black", tl.srt=45,cl.pos = 'b'))
    }
    
  },res = 96)
  
  observeEvent(input$groupby,{
    
    output$aggregationtable <- DT::renderDataTable({
      agg_df()
    }, extensions = c('Scroller'), rownames = FALSE,class = 'cell-border stripe',
    options = list(
      deferRender = TRUE,
      dom = 'rti',
      scrollY = 500,
      scrollX = TRUE,
      scroller = TRUE,
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#1f618d', 'color': '#fff'});",
        "}")
    )
    )
    
    output$aggplot <- renderPlot({
      df <- agg_df()
      
      agg_name <- paste(input$groupby,collapse  = ".")
      agg_name <- rlang::sym(agg_name)
      col_name <- colnames(df)
      
      if (length(input$groupby)>=2){
        df <- df %>% mutate("{{agg_name}}" := paste(!!!rlang::syms(input$groupby),sep = "."))
        plot <- df %>% ggplot(aes(x = reorder({{agg_name}}, desc({{agg_name}})),y = !!rlang::sym(col_name[ncol(df)-1])))
      }else{
        plot <- df %>% ggplot(aes(x = reorder({{agg_name}}, desc({{agg_name}})),y = !!rlang::sym(col_name[ncol(df)])))
      }
      
      plot <- plot+geom_bar(stat = "sum")+theme_gray(base_family = "HiraKakuPro-W3",base_size = 15)+
        theme(legend.position = 'none')+coord_flip()+xlab({{agg_name}})
      
      print(plot)
    },res = 96)
    
  })
  
  
  
  observeEvent(input$target_colname,{
    
    scoreFrame <- reactive({
      c_name <- as.data.frame(colnames(values$dat))
      datatype <- as.data.frame(t(as.data.frame(lapply(values$dat, class))))[,1]
      datatype <- as.data.frame(cbind(c_name,datatype))
      
      rownames(datatype) <- NULL
      colnames(datatype) <- c('変数名','変数型')
      
      datatype <- datatype[datatype[['変数型']]=='numeric'|datatype[['変数型']]=='integer',]
      colInfo_num_int <- datatype[['変数名']]
      
      df <- values$dat[colInfo_num_int]
      
      
      
      if(input$target_colname %in% colnames(df)){
        df %>% select(-input$target_colname)
        colInfo_num_int <- colInfo_num_int[colInfo_num_int != input$target_colname]
      }
      
      df_target <- values$dat[input$target_colname]
      df <-cbind(df, df_target)
      
      df <- df[!is.na(df[input$target_colname]),]
      
      switch(class(df_target[[input$target_colname]]),
             integer = scoreFrame <- designTreatmentsN(df,colInfo_num_int,input$target_colname,verbose=FALSE,codeRestriction = c("clean","isBAD"))$scoreFrame,
             numeric = scoreFrame <- designTreatmentsN(df,colInfo_num_int,input$target_colname,verbose=FALSE,codeRestriction = c("clean","isBAD"))$scoreFrame,
             factor = scoreFrame <- mkCrossFrameMExperiment(df,colInfo_num_int,input$target_colname,verbose=T,codeRestriction = c("clean","isBAD"))$score_frame,
             character = scoreFrame <- mkCrossFrameMExperiment(df,colInfo_num_int,input$target_colname,verbose=T,codeRestriction = c("clean","isBAD"))$score_frame,
      )
      
      if('outcome_level' %in% colnames(scoreFrame)){
        scoreFrame <- scoreFrame %>% filter(code=="clean") %>% select(varName,outcome_level,rsq)
        colnames(scoreFrame) <- c("varName","outcome_level","significance_R2")
      }else{
        scoreFrame <- scoreFrame %>% filter(code=="clean") %>% select(varName,rsq)
        colnames(scoreFrame) <- c("varName","significance_R2")
      }
      scoreFrame <- scoreFrame %>% arrange(desc(significance_R2))
      
    })
    
    output$sigtable <- DT::renderDataTable({
      scoreFrame()
    },class = 'cell-border stripe',colnames = c(No. = 1),
    options = list(
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#1f618d', 'color': '#fff'});",
        "}")
    )
    )
    
    output$sigplot <- renderPlot({
      
      plot <- scoreFrame() %>% ggplot(aes(x = reorder(.data[["varName"]],.data[["significance_R2"]]),y = .data[["significance_R2"]]))
      plot <- plot+geom_bar(stat = "summary", fun = "mean")+theme_gray(base_family = "HiraKakuPro-W3",base_size = 15)+
        theme(legend.position = 'none')+coord_flip()+xlab("varName")
      
      print(plot)
      
    },res = 96)
    
    output$sigplot2 <- renderPlot({
      
      plot <- scoreFrame() %>% ggplot(aes(x = reorder(.data[["varName"]],.data[["significance_R2"]]),y = .data[["significance_R2"]]))
      plot <- plot+geom_bar(stat = "summary", fun = "mean")+theme_gray(base_family = "HiraKakuPro-W3",base_size = 15)+
        theme(legend.position = 'none')+coord_flip()+xlab("varName")
      
      print(plot)
      
    },res = 96)
    
  })
  
  
  #ターゲットの変換型により選択できるフォーマットを変更する。
  observeEvent(input$target_dttm_class,{
    
    options_date_format <- list(
      list(key = "ymd(年月日)", text = "ymd(年月日)"),
      list(key = "ydm(年日月)", text = "ydm(年日月)"),
      list(key = "myd(月年日)", text = "myd(月年日)"),
      list(key = "mdy(月日年)", text = "mdy(月日年)"),
      list(key = "dmy(日月年)", text = "dmy(日月年)")
    )
    
    options_datetime_format <- list(
      list(key = "ymd_h(年月日_時)", text = "ymd_h(年月日_時)"),
      list(key = "ymd_hm(年月日_時分)", text = "ymd_hm(年月日_時分)"),
      list(key = "ymd_hms(年月日_時分秒)", text = "ymd_hms(年月日_時分秒)"),
      list(key = "ydm_h(年日月_時)", text = "ydm_h(年日月_時)"),
      list(key = "ydm_hm(年日月_時分)", text = "ydm_hm(年日月_時分)"),
      list(key = "ydm_hms(年日月_時分秒)", text = "ydm_hms(年日月_時分秒)"),
      list(key = "mdy_h(月日年_時)", text = "mdy_h(月日年_時)"),
      list(key = "mdy_hm(月日年_時分)", text = "mdy_hm(月日年_時分)"),
      list(key = "mdy_hms(月日年_時分秒)", text = "mdy_hms(月日年_時分秒)"),
      list(key = "dmy_h(日月年_時)", text = "dmy_h(日月年_時)"),
      list(key = "dmy_hm(日月年_時分)", text = "dmy_hm(日月年_時分)"),
      list(key = "dmy_hms(日月年_時分秒)", text = "dmy_hms(日月年_時分秒)")
    )
    

    if(input$target_dttm_class =="日付(Date)"){
      updateDropdown.shinyInput(session=session, "dttm_format", values = c(), options=options_date_format)
    }else{
      updateDropdown.shinyInput(session = session, "dttm_format", values = c(), options=options_datetime_format)
    }
  })
  
  #　移動集計の集計単位を入力するためのUI
  # observeEvent(input$time_index_roll,{
  #   if(input$time_index_roll != "None"){
  #     updateSelectInput(session, "time_unit_roll", choices=c("年","月","日","時","分","秒"), selected=NULL)
  #   }else{
  #     updateSelectInput(session, "time_unit_roll", choices=c("None"), selected=NULL)
  #   }
  # })
  
  observeEvent(input$dr_start,{
    
    switch(input$dr_autopilot,
           クイック = dr_mode <- AutopilotMode$Quick,
           フルオート = dr_mode <- AutopilotMode$FullAuto
    )
    
    try_dr_start <- try(
      ConnectToDataRobot(endpoint = input$dr_endpoint, 
                         token = input$dr_token),
      StartProject(dataSource = values$dat,
                   projectName = input$dr_projectname,
                   target = input$dr_targetcolname,
                   wait = FALSE,mode = dr_mode)
    )
  })
  
  #時系列クラスタリング対象のデータを作る(数値型,欠損値を含まない変数のみ)
  observeEvent(values$dat,{
    req(input$file)
    #変数名とクラスを格納したデータフレームを作成する
    datatype <- makeNameClassdf(values$dat)
    colInfo_num_int <- datatype[datatype[['変数型']]=='numeric'|datatype[['変数型']]=='integer',][['変数名']]
    values$colinfo_for_clustering <- datatype %>% filter(変数名 %in% colInfo_num_int)
    #数値かつ欠損値を含まない列のみクラスタリング対象の変数とする
    not_any_na <- function(x) all(!is.na(x))
    values$clustered_data <- values$dat[,colInfo_num_int] %>% select(where(not_any_na))
    
    output$colinfo_for_clustering<- DT::renderDataTable({
      values$colinfo_for_clustering
    },class = 'cell-border stripe',colnames = c(No. = 1),
    options = list(
      initComplete = DT::JS(
        # "function(settings, json) {",
        # "$(this.api().table().header()).css({'background-color': 'rgb(45,59,66)', 'color': '#fff'});",
        # "}"
      )
    )
    )
  })
  
  
  # #時系列クラスタリングの実行
  observeEvent(input$exec_clustering,if(!is.null(input$num_cluster)) {
    req(input$file)
    switch(input$clustering_type,
           "DTW：傾向" = diss_type <- "DTWARP",
           "INT.PER：周期"= diss_type <- "INT.PER"
    )
    d <- diss(values$clustered_data, diss_type,normalize = (input$c_normalize_type == "有"))
    h <- hclust(d)
    values$cluster <- cutree(h, input$num_cluster)
    
    output$clustering_ui <- renderUI({
      output = tagList()
      output[[1]] <- selectInput("select_cluster_name", "クラス", choices=c(),width = '50%',multiple = TRUE)
      output[[2]] <- actionButton("select_cluster", "選択したクラスのデータのみ残す",icon("ok",lib = "glyphicon"))
      output
    })
    #時系列プロットの表示
    p <- plot_group(values$clustered_data,values$cluster)
    output$clustering_plot<- renderPlot({
      print(p)
    })
    output$clustering_plot2<- renderPlot({
      print(p)
    })
    #セレクトインプットの更新
    updateSelectInput(session, "select_cluster_name", choices=sort(unique(values$cluster)),selected = sort(unique(values$cluster)))
    #変数データテーブルの表示
    output$colinfo_for_clustering_result<- DT::renderDataTable({
      values$colinfo_for_clustering
    },class = 'cell-border stripe',colnames = c(No. = 1),
    options = list(
      initComplete = DT::JS(
        # "function(settings, json) {",
        # "$(this.api().table().header()).css({'background-color': 'rgb(45,59,66)', 'color': '#fff'});",
        # "}"
      )
    )
    )
  })
  
  #選択したクラスのグラフのみを表示する
  observeEvent(input$select_cluster_name, {
    req(input$file)
    p <- plot_group(values$clustered_data,values$cluster,select_class = input$select_cluster_name)
    output$clustering_plot<- renderPlot({
      print(p)
    })
    
    colname_select <- colnames(values$clustered_data)[values$cluster %in% input$select_cluster_name]
    
    show_data <- cbind(values$colinfo_for_clustering,data.frame(values$cluster)) %>% filter(変数名 %in% colname_select)
    colnames(show_data)[3] <- "クラス"
    rownames(show_data) <- NULL
    output$colinfo_for_clustering_result<- DT::renderDataTable({
      show_data
    },class = 'cell-border stripe',colnames = c(No. = 1),
    options = list(
      initComplete = DT::JS(
        # "function(settings, json) {",
        # "$(this.api().table().header()).css({'background-color': 'rgb(45,59,66)', 'color': '#fff'});",
        # "}"
      )
    )
    )
  })
  
  
  #時系列クラスタリング(選択したクラスのみ残す)
  observeEvent(input$select_cluster,if(!is.null(input$select_cluster_name)) {
    req(input$file)
    colname_select <- colnames(values$clustered_data)[values$cluster %in% input$select_cluster_name]
    csv_data <- values$clustered_data[,colname_select]
    updateData(csv_data, input, output, session, values)
    updateSelectInput(session, "select_cluster_name", choices=sort(unique(input$select_cluster_name)),selected = sort(unique(input$select_cluster_name)))
  })
  
  # Rio: varx_timeが入力されたらグラフを作成する。""の時は処理しない
  observeEvent(input$make_labelplot, if(!is.null(input$varx_time)) {
    output$labelplot <- renderPlot({
      #プロットテーマ
      theme <- theme_gray(base_family = "HiraKakuPro-W3",base_size = 15)
      if(!is.null(input$vary_time)){
        switch(input$modetype,
               "max" =  p <- ggplotWithPeaks(type="max", data=values$dat, x=jsonConvert(input$varx_time), y=jsonConvert(input$vary_time), w=as.numeric(input$window_size), span=as.numeric(input$span)),
               "min" =  p <- ggplotWithPeaks(type="min", data=values$dat, x=jsonConvert(input$varx_time), y=jsonConvert(input$vary_time), w=as.numeric(input$window_size), span=as.numeric(input$span)),
               "max+min" =  p <- ggplotWithPeaks(type="max+min", data=values$dat, x=jsonConvert(input$varx_time), y=jsonConvert(input$vary_time), w=as.numeric(input$window_size), span=as.numeric(input$span)),
        )
        p <- p + xlab(jsonConvert(input$varx_time)) + ylab(jsonConvert(input$vary_time))
        print(p)
      }else{
        p <- ggplot(values$dat, aes(x = values$dat[[jsonConvert(input$varx_time)]], y = values$dat[[jsonConvert(input$varx_time)]] ),na.rm = TRUE)+
          geom_line() +
          xlab(jsonConvert(input$varx_time)) + 
          ylab(jsonConvert(input$varx_time)) +
          theme
        print(p)
      }
    }, res = 96)
    output$labelplot2 <- renderPlot({
      #プロットテーマ
      theme <- theme_gray(base_family = "HiraKakuPro-W3",base_size = 15)
      if(!is.null(input$vary_time)){
        switch(input$modetype,
               "max" =  p <- ggplotWithPeaks(type="max", data=values$dat, x=jsonConvert(input$varx_time), y=jsonConvert(input$vary_time), w=as.numeric(input$window_size), span=as.numeric(input$span)),
               "min" =  p <- ggplotWithPeaks(type="min", data=values$dat, x=jsonConvert(input$varx_time), y=jsonConvert(input$vary_time), w=as.numeric(input$window_size), span=as.numeric(input$span)),
               "max+min" =  p <- ggplotWithPeaks(type="max+min", data=values$dat, x=jsonConvert(input$varx_time), y=jsonConvert(input$vary_time), w=as.numeric(input$window_size), span=as.numeric(input$span)),
        )
        p <- p + xlab(jsonConvert(input$varx_time)) + ylab(jsonConvert(input$vary_time))
        print(p)
      }else{
        p <- ggplot(values$dat, aes(x = values$dat[[jsonConvert(input$varx_time)]], y = values$dat[[jsonConvert(input$varx_time)]] ),na.rm = TRUE)+
          geom_line() +
          xlab(jsonConvert(input$varx_time)) + 
          ylab(jsonConvert(input$varx_time)) +
          theme
        print(p)
      }
    })
  })
  
}
enableBookmarking("server")
shinyApp(ui, server)