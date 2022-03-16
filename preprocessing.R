filterItems <- list(
    list(key = "item1", name = "基本", subMenuProps = list(
        items = list(
            list(key = 'convertType', name = '変数型変換', onClick = JS("function() { Shiny.setInputValue('filter', 'convertType'); console.log('convertType'); }")),
            list(key = 'reorderRow', name = '行の並べ替え', onClick = JS("function() { Shiny.setInputValue('filter', 'reorderRow'); }")),
            list(key = 'filterRow', name = '行フィルター', onClick = JS("function() { Shiny.setInputValue('filter', 'filterRow'); }")),
            list(key = 'selectColumn', name = '変数選択', onClick = JS("function() { Shiny.setInputValue('filter', 'selectColumn'); }")),
            list(key = 'deleteColumn', name = '変数削除', onClick = JS("function() { Shiny.setInputValue('filter', 'deleteColumn'); }")),
            list(key = 'renameColumn', name = '変数名の変更', onClick = JS("function() { Shiny.setInputValue('filter', 'renameColumn'); }"))
        )
    )),
    list(key = "item2", name = "欠損値", subMenuProps = list(
        items = list(
            list(key = 'onehotEnc', name = 'ワンホット', onClick = JS("function() { Shiny.setInputValue('filter', 'onehotEnc'); }")),
            list(key = 'targetEnc', name = 'ターゲット', onClick = JS("function() { Shiny.setInputValue('filter', 'targetEnc'); }")),
            list(key = 'numericNull', name = '数値欠損値処理', onClick = JS("function() { Shiny.setInputValue('filter', 'numericNull'); }")),
            list(key = 'characterNull', name = '文字列欠損値処理', onClick = JS("function() { Shiny.setInputValue('filter', 'characterNull'); }")),
            list(key = 'allOnehotEnc', name = '一括ワンホット処理', onClick = JS("function() { Shiny.setInputValue('filter', 'allOnehotEnc'); }"))
        )
    )),
    list(key = "item3", name = "時系列", subMenuProps = list(
        items = list(
            list(key = 'datetimeConversion', name = '日付変換', onClick = JS("function() { Shiny.setInputValue('filter', 'datetimeConversion'); }")),
            list(key = 'datetimeFilter', name = '日付抽出', onClick = JS("function() { Shiny.setInputValue('filter', 'datetimeFilter'); }")),
            list(key = 'lagGeneration', name = 'ラグ生成', onClick = JS("function() { Shiny.setInputValue('filter', 'lagGeneration'); }")),
            list(key = 'leadGeneration', name = 'リード生成', onClick = JS("function() { Shiny.setInputValue('filter', 'leadGeneration'); }")),
            list(key = 'movingAverage', name = '移動集計', onClick = JS("function() { Shiny.setInputValue('filter', 'movingAverage'); }"))
        )
    )),
    list(key = "item4", name = "文字列", subMenuProps = list(
      items = list(
        list(key = 'characterMerge', name = '結合', onClick = JS("function() { Shiny.setInputValue('filter', 'characterMerge'); }")),
        list(key = 'characterSplit', name = '分割', onClick = JS("function() { Shiny.setInputValue('filter', 'characterSplit'); }")),
        list(key = 'characterFilter', name = '抽出', onClick = JS("function() { Shiny.setInputValue('filter', 'characterFilter'); }")),
        list(key = 'characterReplace', name = '置換', onClick = JS("function() { Shiny.setInputValue('filter', 'characterReplace'); }")),
        list(key = 'characterContain', name = '含有判定', onClick = JS("function() { Shiny.setInputValue('filter', 'characterContain'); }"))
      )
    )),
    list(key = "item4", name = "可視化", subMenuProps = list(
      items = list(
        list(key = 'plotlyPlot', name = 'プロット', onClick = JS("function() { Shiny.setInputValue('filter', 'plotlyPlot'); }")),
        list(key = 'aggPlot', name = '集計', onClick = JS("function() { Shiny.setInputValue('filter', 'aggPlot'); }")),
        list(key = 'fundStatPlot', name = '基本統計量', onClick = JS("function() { Shiny.setInputValue('filter', 'fundStatPlot'); }")),
        list(key = 'impPlot', name = '変数重要度', onClick = JS("function() { Shiny.setInputValue('filter', 'impPlot'); }")),
        list(key = 'corrPlot', name = '変数相関', onClick = JS("function() { Shiny.setInputValue('filter', 'corrPlot'); }")),
        list(key = 'labelPlot', name = 'イベントラベル', onClick = JS("function() { Shiny.setInputValue('filter', 'labelPlot'); }")),
        list(key = 'clustering', name = '時系列クラスタリング', onClick = JS("function() { Shiny.setInputValue('filter', 'clustering'); }"))
      )
    ))
)

filterOverflowItems <- list(
    list(key = "customFunction", name = "カスタム関数", onClick = JS("function() { Shiny.setInputValue('filter', 'customFunction'); }"))
)

onRenderItem <- JS("item =>
    jsmodule['react'].createElement(jsmodule['@fluentui/react'].CommandBarButton, {
        role: 'menuitem',
        text: item.name,
        menuProps: item.subMenuProps,
        onClick: item.onClick
    })
")

onRenderOverflowButton <- JS("overflowItems =>
    jsmodule['react'].createElement(jsmodule['@fluentui/react'].CommandBarButton, {
        role: 'menuitem',
        title: 'More items',
        styles: {
        root: { minWidth: 0, padding: '0 4px', alignSelf: 'stretch', height: 'auto', }
        },
        menuIconProps: { iconName: 'More' },
        menuProps: { items: overflowItems }
    })
")

filterMenu <- OverflowSet(
    vertical = FALSE,
    items = filterItems,
    overflowItems = filterOverflowItems,
    onRenderItem = onRenderItem,
    onRenderOverflowButton = onRenderOverflowButton
)

# Plot Item

plotItems <- list(
  list(key = "item1", name = "プロットタイプ", subMenuProps = list(
    items = list(
      list(key = 'plotBasic', name = 'プロット', onClick = JS("function() { Shiny.setInputValue('filter', 'plotBasic'); console.log('convertType'); }")),
      list(key = 'plotAgg', name = '集計', onClick = JS("function() { Shiny.setInputValue('filter', 'plotAgg'); }")),
      list(key = 'plotFund', name = '基本統計量', onClick = JS("function() { Shiny.setInputValue('filter', 'plotFund'); }")),
      list(key = 'plotImp', name = '重要度', onClick = JS("function() { Shiny.setInputValue('filter', 'plotImp'); }")),
      list(key = 'plotCorr', name = '変数相関', onClick = JS("function() { Shiny.setInputValue('filter', 'plotCorr'); }")),
      list(key = 'plotLabel', name = 'イベントラベル', onClick = JS("function() { Shiny.setInputValue('filter', 'plotLabel'); }")),
      list(key = 'plotClust', name = 'クラスタリング', onClick = JS("function() { Shiny.setInputValue('filter', 'plotClust'); }"))
    )
  ))
)

plotMenu <- OverflowSet(
  vertical = FALSE,
  items = plotItems,
  onRenderItem = onRenderItem,
  onRenderOverflowButton = onRenderOverflowButton
)

# Drop Down
options_placeholder <- list(
    list(key = "A", text = "Option A"),
    list(key = "B", text = "Option B"),
    list(key = "C", text = "Option C")
)

options_type <- list(
    list(key = "numeric", text = "Numeric"),
    list(key = "integer", text = "Integer"),
    list(key = "factor", text = "Factor"),
    list(key = "character", text = "Character")
)

options_order <- list(
  list(key = "昇順", text = "昇順"),
  list(key = "降順", text = "降順")
)

options_operator <- list(
  list(key = "=", text = "="),
  list(key = "≠", text = "≠"),
  list(key = "≧", text = "≧"),
  list(key = ">", text = ">"),
  list(key = "≦", text = "≦"),
  list(key = "<", text = "<"),
  list(key = "欠損値を含む行を削除", text = "欠損値を含む行を削除"),
  list(key = "欠損値を含まない行を削除", text = "欠損値を含まない行を削除")
)

options_aggfunc <- list(
  list(key = "平均", text = "平均"),
  list(key = "最大", text = "最大"),
  list(key = "最小", text = "最小"),
  list(key = "分散", text = "分散"),
  list(key = "中央値", text = "中央値"),
  list(key = "合計", text = "合計"),
  list(key = "カウント", text = "カウント")
)

options_aggfunc2 <- list(
  list(key = "平均", text = "平均"),
  list(key = "最大", text = "最大"),
  list(key = "最小", text = "最小"),
  list(key = "分散", text = "分散"),
  list(key = "中央値", text = "中央値"),
  list(key = "合計", text = "合計")
)

options_complement<- list(
  list(key = "平均", text = "平均"),
  list(key = "中央", text = "中央"),
  list(key = "最頻", text = "最頻"),
  list(key = "特定値", text = "特定値"),
  list(key = "直前の非欠損値", text = "直前の非欠損値"),
  list(key = "前後非欠値による線形補完", text = "前後非欠値による線形補完")
  
)

options_missing <- list(
  list(key = "有", text = "有"),
  list(key = "無", text = "無")
)

options_time_datatype <- list(
  list(key = "日付(Date)", text = "日付（Date）"),
  list(key = "日付時刻(POSIXct)", text = "日付（POSIXct）")
)

options_time_format <- list(
  list(key = "ymd", text = "年月日"),
  list(key = "ymdhm", text = "年月日時刻")
)

options_time_filter <- list(
  list(key = "年", text = "年"),
  list(key = "月", text = "月"),
  list(key = "日", text = "日"),
  list(key = "時", text = "時"),
  list(key = "分", text = "分"),
  list(key = "秒", text = "秒")
)


options_leftright <- list(
  list(key = "区切文字の左側を返す", text = "区切り文字の左側を返す"),
  list(key = "区切文字の右側を返す", text = "区切り文字の右側を返す")
)

options_labelMode <- list(
  list(key = "max", text = "max"),
  list(key = "min", text = "min"),
  list(key = "max+min", text = "max+min")
)


options_clustering <- list(
  list(key = "DTW：傾向", text = "DTW：傾向"),
  list(key = "INT.PER：周期", text = "INT.PER：周期")
)

options_normalize <- list(
  list(key = "有", text = "有"),
  list(key = "無", text = "無")
)

PlotItem <- function(inputKey) {
  switch(inputKey,
         "plotBasic" = div(plotOutput("plot",height = '100%'), style = "height: 75vh"),
         "plotAgg" = div(plotOutput("aggplot",height = '100%'), style = "height: 75vh"),
         "plotFund" = DT::dataTableOutput('summary'),
         "plotImp" = div(plotOutput("sigplot",height = '100%'), style = "height: 75vh"),
         "plotCorr" = div(plotOutput("corrplot",height = '100%'), style = "height: 75vh"),
         "plotLabel" = div(plotOutput("labelplot",height = '100%'), style = "height: 75vh"),
         "plotClust" = div(plotOutput("clustering_plot",height = '100%'), style = "height: 75vh"),
         div(plotOutput("plot",height = '100%'), style = "height: 75vh")
  )
}

FilterItem <- function(inputKey) {
  switch(inputKey,
         "convertType" = list(
           Text(variant = "large", "変数型変換"),
           Text(variant = "medium", "変数名"),
           TagPicker(
             onResolveSuggestions = JS("filterSuggestedTags"),
             onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
             getTextFromItem = JS("function(item) { return item.text }"),
             pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
             itemLimit = 1,
             onChange = JS("function(selection) { Shiny.setInputValue('colname_classchange', JSON.stringify(selection)) }")
           ),
           Text(variant = "medium", "型"),
           Dropdown.shinyInput("target_class", options=options_type),
           br(),
           PrimaryButton.shinyInput("class_change", text = "適用")
         ),
         "reorderRow" = list(
           Text(variant = "large", "列の並べ替え"),
           Text(variant = "medium", "変数名(複数選択可能）"),
           TagPicker(
             onResolveSuggestions = JS("filterSuggestedTags"),
             onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
             getTextFromItem = JS("function(item) { return item.text }"),
             pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
             onChange = JS("function(selection) { Shiny.setInputValue('colname_arrange', JSON.stringify(selection)) }")
           ),
           Text(variant = "medium", "昇順/降順"),
           Dropdown.shinyInput("a_or_d", options=options_order),
           br(),
           PrimaryButton.shinyInput("arrange_row", text = "適用")
         ),
        "filterRow" = list(
          Text(variant = "large", "行フィルター"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colname_filter', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "演算子"),
          Dropdown.shinyInput("filter_operator", options = options_operator),
          Text(variant = "medium", "値"),
          TextField.shinyInput("filter_value"),
          Text(variant = "small", '*文字列を入力する場合は""で囲んでください'),
          br(),
          PrimaryButton.shinyInput("filter_row", text = "適用")
        ),
        "selectColumn" = list(
          Text(variant = "large", "変数を選択する"),
          Text(variant = "medium", "変数名(複数選択可能）"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            onChange = JS("function(selection) { Shiny.setInputValue('colname_select', JSON.stringify(selection)) }")
          ),
          br(),
          PrimaryButton.shinyInput("select_columns", text = "適用")
        ),
        "deleteColumn" = list(
          Text(variant = "large", "変数を削除する"),
          Text(variant = "medium", "変数名(複数選択可能）"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            onChange = JS("function(selection) { Shiny.setInputValue('colnameDelete', JSON.stringify(selection)) }")
          ),
          br(),
          PrimaryButton.shinyInput("columnDelete", text = "適用")
        ),
        "renameColumn" = list(
          Text(variant = "large", "列名変更"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colnameChange', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "保持するラベルの最低出現頻度(最小値：0 - 最大値：１)"),
          TextField.shinyInput("newName"),
          br(),
          PrimaryButton.shinyInput("nameChange", text = "適用")
        ),
        "onehotEnc" = list(
          Text(variant = "large", "ワンホットエンコーディング"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colnameOnehot', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "保持するラベルの最低出現頻度(最小値：0 - 最大値：１)"),
          TextField.shinyInput("minfracOnehot"),
          Text(variant = "small", '*文字列を入力する場合は""で囲んでください'),
          br(),
          PrimaryButton.shinyInput("convertOnehot", text = "適用")
        ),
        "targetEnc" = list(
          Text(variant = "large", "ターゲットエンコーディング"),
          Text(variant = "medium", "新しい変数名"),
          TextField.shinyInput("tar_new_col"),
          Text(variant = "medium", "グループ化変数"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('tar_groupby', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "集計変数（数値型変数のみ）"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('tar_agg_colname', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "集計方法"),
          Dropdown.shinyInput("tar_aggregation", options = options_aggfunc),
          br(),
          PrimaryButton.shinyInput("target_encode", text = "適用")
        ),
        "numericNull" = list(
          Text(variant = "large", "数値データの欠損値処理"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colnameNAs', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "欠損値の補完方法"),
          Dropdown.shinyInput("dealna_type", options = options_complement),
          Text(variant = "medium", "特定値"),
          TextField.shinyInput("na_target_value"),
          Text(variant = "medium", "欠損値の有無を示す変数を追加有無"),
          Dropdown(options = options_missing),
          br(),
          PrimaryButton.shinyInput("dealNAs", text = "適用")
        ),
        "characterNull" = list(
          Text(variant = "large", "文字列データの欠損値処理"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colnameNAs_str', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "欠損値を置き換える文字列"),
          TextField.shinyInput("na_rep_str"),
          br(),
          PrimaryButton.shinyInput("dealNAs_str", text = "適用")
        ),
        "allOnehotEnc" = list(
          Text(variant = "large", "一括ワンホット・欠損値処理"),
          Text(variant = "small", 'カテゴリ変数に対するワンホットエンコーディング、及び数値変数に対する欠損値処理を一度に行います。'),
          Text(variant = "small", '*数値変数はワンホットエンコーディングされません。'),
          Text(variant = "medium", "保持するラベルの最低出現頻度(最小値：0 - 最大値：１)"),
          TextField.shinyInput("minfracAll"),
          Text(variant = "small", '*カテゴリ変数をワンホットする際、全体にする出現頻度がこの数値よりも小さいラベルは除外されます。'),
          br(),
          PrimaryButton.shinyInput("dealAll", text = "適用")
        ),
        "datetimeConversion" = list(
          Text(variant = "large", "文字列型から日付時刻型への変換"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colname_dttmchange', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "変換後の型"),
          Dropdown.shinyInput("target_dttm_class", values = c(), options = options_time_datatype),
          Text(variant = "medium", "フォーマット"),
          Dropdown.shinyInput("dttm_format", values = c()),
          br(),
          PrimaryButton.shinyInput("dttm_change", text = "適用")
        ),
        "datetimeFilter" = list(
          Text(variant = "large", "日付時刻要素の抽出"),
          Text(variant = "medium", "新しい変数名"),
          TextField.shinyInput("dttm_sub_new_col"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colname_dttm_sub', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "抽出する要素"),
          Dropdown.shinyInput("dttm_sub_element", options = options_time_filter),
          br(),
          PrimaryButton.shinyInput("dttm_sub", text = "適用")
        ),
        "lagGeneration" = list(
          Text(variant = "large", "ラグ変数生成"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colname_lag', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "繰り返し数"),
          TextField.shinyInput("num_lag"),
          br(),
          PrimaryButton.shinyInput("make_lag", text = "適用")
        ),
        "leadGeneration" = list(
          Text(variant = "large", "リード変数生成"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colname_lead', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "繰り返し数"),
          TextField.shinyInput("num_lead"),
          br(),
          PrimaryButton.shinyInput("make_lead", text = "適用")
        ),
        "movingAverage" = list(
          Text(variant = "large", "移動集計"),
          Text(variant = "medium", "新しい変数名"),
          TextField.shinyInput("new_col_roll"),
          Text(variant = "medium", "集計する変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colname_roll', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "時系列インデックス変数指定"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('time_index_roll', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "集計範囲（前）"),
          TextField.shinyInput("before_rollbefore_roll"),
          Text(variant = "medium", "集計範囲（前）"),
          TextField.shinyInput("after_roll"),
          Text(variant = "medium", "集計方法"),
          Dropdown("aggmethod_roll", options = options_aggfunc2),
          br(),
          PrimaryButton.shinyInput("make_roll", text = "適用")
        ),
        "characterMerge" = list(
          Text(variant = "large", "文字列の結合"),
          Text(variant = "medium", "新しい変数名"),
          TextField.shinyInput("new_col_strc"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colnameCombine1', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "区切り文字"),
          TextField.shinyInput("c_sep_token"),
          br(),
          PrimaryButton.shinyInput("columnCombine", text = "適用")
        ),
        "characterSplit" = list(
          Text(variant = "large", "文字列の分割"),
          Text(variant = "medium", "新しい変数名"),
          TextField.shinyInput("new_col_strs"),
          Text(variant = "medium", "分割する変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colnameSplit', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "区切り文字"),
          TextField.shinyInput("s_sep_token"),
          Text(variant = "medium", "区切り文字の左側・右側を返す。"),
          Dropdown.shinyInput("rorl", options = options_leftright),
          br(),
          PrimaryButton.shinyInput("columnSplit", text = "適用")
        ),
        "characterFilter" = list(
          Text(variant = "large", "文字列の抽出"),
          Text(variant = "medium", "新しい変数名"),
          TextField.shinyInput("new_col_str_sub"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colname_str_sub', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "何文字目から"),
          TextField.shinyInput("str_sub_start"),
          Text(variant = "medium", "何文字目まで"),
          TextField.shinyInput("str_sub_end"),
          br(),
          PrimaryButton.shinyInput("str_substract", text = "適用")
        ),
        "characterReplace" = list(
          Text(variant = "large", "文字列の置換"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colname_str_rep', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "置換対象の文字列"),
          TextField.shinyInput("str_rep_before"),
          Text(variant = "medium", "置換後の文字列"),
          TextField.shinyInput("str_rep_after"),
          br(),
          PrimaryButton.shinyInput("str_replace", text = "適用")
        ),
        "characterContain" = list(
          Text(variant = "large", "含有判定"),
          Text(variant = "medium", "新しい変数名"),
          TextField.shinyInput("new_col_str_detect"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colname_str_detect', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "指定文字列or正規表現"),
          TextField.shinyInput("partial_string"),
          br(),
          PrimaryButton.shinyInput("str_detect", text = "適用")
        ),
        "customFunction" = list(
          Text(variant = "large", "カスタム関数"),
          Text(variant = "medium", "新しい変数名"),
          TextField.shinyInput("new_col"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('colnameCalc', JSON.stringify(selection)) }")
          ),
          ActionButton("insertcolname", text="変数を挿入"),
          
          Text(variant = "medium", "計算式　(e.g. A*B/C)"),
          TextField.shinyInput("formula"),
          br(),
          PrimaryButton.shinyInput("calc", text = "適用")
        ),
        "plotlyPlot" = list(
          Text(variant = "large", "プロット"),
          Text(variant = "medium", "X"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('varx', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "Y"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('vary', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "カラー"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('varc', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "ファセット"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('varf', JSON.stringify(selection)) }")
          )
        ),
        "aggPlot" = list(
          Text(variant = "large", "集計"),
          Text(variant = "medium", "グループ化変数"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            onChange = JS("function(selection) { Shiny.setInputValue('groupby', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "集計変数"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('agg_colname', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "集計方法"),
          Dropdown.shinyInput("aggregation", options = options_aggfunc)
        ),
        "fundStatPlot" = list(
          Text(variant = "large", "基本統計量"),
          Text(variant = "medium", "変数名"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('summary_colname', JSON.stringify(selection)) }")
          )
        ),
        "impPlot" = list(
          Text(variant = "large", "変数重要度"),
          Text(variant = "medium", "ターゲット変数"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('target_colname', JSON.stringify(selection)) }")
          )
        ),
        "corrPlot" = list(
          Text(variant = "large", "変数相関")
        ),
        "labelPlot" = list(
          Text(variant = "large", "イベントラベル"),
          Text(variant = "medium", "X"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('varx_time', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "Y"),
          TagPicker(
            onResolveSuggestions = JS("filterSuggestedTags"),
            onEmptyInputFocus = JS("function(tagList) { return testTags.filter(tag => !listContainsTagList(tag, tagList)); }"),
            getTextFromItem = JS("function(item) { return item.text }"),
            pickerSuggestionsProps = list(suggestionsHeaderText = '変数リスト', noResultsFoundText = 'データセットが読み込まれていません。'),
            itemLimit = 1,
            onChange = JS("function(selection) { Shiny.setInputValue('vary_time', JSON.stringify(selection)) }")
          ),
          Text(variant = "medium", "Span"),
          TextField.shinyInput("span"),
          Text(variant = "medium", "Window Size"),
          TextField.shinyInput("window_size"),
          Text(variant = "medium", "ラベルモード"),
          Dropdown.shinyInput("modetype", options = options_labelMode),
          br(),
          PrimaryButton.shinyInput("make_label", text = "プロット")
        ),
        "clustering" = list(
          Text(variant = "large", "時系列クラスタリング"),
          Text(variant = "medium", "クラスタ数"),
          TextField.shinyInput("num_cluster"),
          Text(variant = "medium", "使用する距離関数"),
          Dropdown.shinyInput("clustering_type", options = options_clustering),
          Text(variant = "medium", "基準化"),
          Dropdown.shinyInput("c_normalize_type", options = options_normalize),
          br(),
          PrimaryButton.shinyInput("exce_clustering", text = "プロット")
        )
  )
}

preprocessing_page <- Page(
  title = "前処理",
  subtitle = "Preprocessing",
  Grid(
    reactOutput("spinner"),
    GridItem(
        class = "ms-sm12 ms-xl4",
        Card(
            title="フィルター",
            filterMenu,
            uiOutput('uiFilterItems')
        )
    ),
    uiOutput('uiView')
    
  ),
  tagList(reactOutput("reactDialog")),
  tagList(reactOutput("reactPanel")),
  HTML('<button onclick="topFunction()" id="myBtn" title="Go to top"></button>')
)