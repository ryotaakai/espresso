

#関数記述用ファイル

# JSON CONVERSION
jsonConvert <- function(json){
  print(json)
  if (!is.null(json)) {
    myjsondf = fromJSON(json)
    avector = c()
    for(atmp in myjsondf['key']) { avector <- atmp }
    return(avector)
  } else {
    return(NULL)
  }
}

prepare_data <- function(csv_data, history){
  
  for(action in history){
    if(action[[1]]=='Onehot'){
      colname <- action[[2]]
      treatments <- action[[3]]
      forwards <- getForwardColumns(csv_data, colname)
      backwards <- getBackwardColumns(csv_data, colname)
      
      #onehot変換の前にファクタ化する #中村
      csv_data[[colname]] <- as.factor(csv_data[[colname]])
      renames <- shortenVarName(treatments[2]$scoreFrame)
      # print(renames)
      csv_data <- prepare(treatments, csv_data)
      for (i in 1:nrow(renames)){
        csv_data %<>% renameColumn(.,renames[i, "varName"], renames[i, "newName"])
      }
      csv_data <- cbind(forwards, csv_data, backwards)
    }
    if(action[[1]]=='dealNAs'){
      csv_data <- dealNumericna(csv_data,action[[2]],action[[3]],action[[4]],action[[5]],action[[6]])[[1]]
    }
    if(action[[1]]=='dealNAs_str'){
      csv_data <- dealStrna(csv_data,action[[2]],action[[3]])
    }
    if(action[[1]]=='dealAll'){
      treatments <- action[[2]]
      treatments[2]$scoreFrame %<>% sortScoreFrameByOrigName(., csv_data)
      renames <- shortenVarName(treatments[2]$scoreFrame)
      csv_data <- prepare(treatments, csv_data)
      for (i in 1:nrow(renames)){
        csv_data %<>% renameColumn(.,renames[i, "varName"], renames[i, "newName"])
      }
    }
    if(action[[1]]=='nameChange'){
      csv_data <- renameColumn(csv_data, action[[2]], action[[3]])
    }
    if(action[[1]]=='calc'){
      csv_data <- calc(action[[2]], action[[3]], csv_data)
    }
    if(action[[1]]=='columnDelete'){
      csv_data <- removeColumn(csv_data, action[[2]])
    }
    if(action[[1]]=='columnCombine'){
      csv_data <- combineStr(csv_data,action[[2]],action[[3]],action[[4]])
    }
    if(action[[1]]=='columnSplit'){
      csv_data <- split_str(action[[2]],action[[3]],action[[4]],csv_data,action[[5]])
    }
    if(action[[1]]=='class_change'){
      result <- classChange(csv_data,action[[2]],action[[3]])
      csv_data <- result[[2]]
    }
    if(action[[1]]=='dttm_change'){
      result <- changeDttm(csv_data,action[[2]],action[[3]],action[[4]])
      csv_data <- result[[2]]
    }
    if(action[[1]]=='arrange_row'){
      csv_data <- arrangeRows(csv_data,action[[2]],action[[3]])
    }
    if(action[[1]]=='filter_row'){
      result <- filterRows(csv_data,action[[2]],action[[3]],action[[4]])
      csv_data <- result[[2]]
    }
    if(action[[1]]=='select_columns'){
      csv_data <- selectColumns(csv_data,action[[2]])
    }
    if(action[[1]]=='make_lag'){
      csv_data <- makeLag(csv_data,action[[2]],action[[3]])
    }
    if(action[[1]]=='make_lead'){
      csv_data <- makeLead(csv_data,action[[2]],action[[3]])
    }
    
    if(action[[1]]=='make_roll'){
      csv_data <- makeRoll(csv_data,action[[2]],action[[3]],action[[4]],action[[5]],action[[6]],action[[7]],action[[8]])
    }
    if(action[[1]]=='target_encode'){
      #マッピング辞書と結合
      join_df <- csv_data[action[[3]]] %>% left_join(action[[2]])
      csv_data <- cbind(csv_data,join_df[2])
    }
    if(action[[1]]=='str_detect'){
      csv_data <- strDetect(csv_data,action[[2]],action[[3]],action[[4]])
    }
    if(action[[1]]=='str_substract'){
      csv_data <- strSub(csv_data,action[[2]],action[[3]],action[[4]],action[[5]])
    }
    if(action[[1]]=='str_replace'){
      csv_data <- strReplace(csv_data,action[[2]],action[[3]],action[[4]])
    }
    if(action[[1]]=='dttm_sub'){
      csv_data <- dttmSub(csv_data,action[[2]],action[[3]],action[[4]])
    }
    if(action[[1]]=='make_label'){
      csv_data <- makeEventLabel(csv_data,action[[2]],action[[3]],action[[4]],action[[5]],action[[6]])
    }
  }
  return(csv_data)
}


#データフレームのユニーク値の数を数える関数
unique_count <- function(x){
  length(unique(x))
}


exists <- function(dataframe, colname){
  return (colname %in% colnames(dataframe))
}

addcol <- function(dataframe, colname, data){
  if (!exists(dataframe, colname)){
    dataframe[colname] <- data
    return(dataframe)
  }else{
    return(dataframe)
  }
}

getElements <- function(dataframe, colname){
  elements <- data.frame()
  for (i in dataframe[[colname]]){
    elements <- addcol(elements, as.character(i), numeric(0))
  }
  return(colnames(elements))
}

calc <- function(new_col, formula, df){
  
  df[[new_col]] <- eval(str2lang(paste("df%$%(", formula, ")")))
  return(df)
}

####中村コード追加　文字列の分割
split_str <- function(new_col, rorl, sep_token ,df,colname){
  
  if(rorl=="区切文字の左側を返す"){
    df[[new_col]] <-  stri_split_fixed(as.character(df[[colname]]), sep_token ,n=2,simplify = TRUE)[,1]
  }else{
    df[[new_col]] <-  stri_split_fixed(as.character(df[[colname]]), sep_token ,n=2, simplify = TRUE)[,2]
  }
  return(df)
}


####中村コード追加　文字列の結合
# combine_str <- function(new_col, sep_token ,df,col_1, col_2){
#   df[[new_col]] <-  str_c(as.character(df[[col_1]]),as.character(df[[col_2]]), sep = sep_token)
#   return(df)
# }

combineStr <- function(dat,new_col, sep_token ,col_1){
  df_col_1 <- dat[col_1]
  new_column <- df_col_1 %>% unite_(new_col,col_1,sep = sep_token,remove = T)
  df[[new_col]] <-new_column[[1]]
  return(df)
}


#変数の型変換を行う関数
classChange <- function(dat,colname_classchange,target_class){
  df <- dat
  col_sym <- rlang::sym(colname_classchange)
  
  try_class_change <- try(
    switch (target_class,
            'numeric' = df <- df %>% mutate("{{col_sym}}":= as.numeric(as.character({{col_sym}}))),
            'integer' = df <- df %>% mutate("{{col_sym}}":= as.integer(as.character({{col_sym}}))),
            'factor' = df <- df %>% mutate("{{col_sym}}":= as.factor(as.character({{col_sym}}))),
            "character" = df <- df %>% mutate("{{col_sym}}":= as.character(as.character({{col_sym}})))
    )
  )
  return(list(try_class_change,df))
}



# 文字列から日付時刻への変換を行う関数
changeDttm <- function(dat,colname_dttmchange,target_dttm_class,dttm_format){
  df <- dat
  col_sym <- rlang::sym(colname_dttmchange)
  
  try_dttm_change <- try(
    if(target_dttm_class =="日付(Date)"){
      switch (dttm_format,
              "ymd(年月日)" = df <- df %>% mutate("{{col_sym}}":= as_date(ymd({{col_sym}}))),
              "ydm(年日月)"= df <- df %>% mutate("{{col_sym}}":= as_date(ydm({{col_sym}}))),
              "myd(月年日)"= df <- df %>% mutate("{{col_sym}}":= as_date(myd({{col_sym}}))),
              "mdy(月日年)"= df <- df %>% mutate("{{col_sym}}":= as_date(mdy({{col_sym}}))),
              "dym(日年月)"= df <- df %>% mutate("{{col_sym}}":= as_date(dym({{col_sym}}))),
              "dmy(日月年)"= df <- df %>% mutate("{{col_sym}}":= as_date(dmy({{col_sym}})))
      )
    }else{
      switch (dttm_format,
              "ymd_h(年月日_時)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(ymd_h({{col_sym}}))),
              "ymd_hm(年月日_時分)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(ymd_hm({{col_sym}}))),
              "ymd_hms(年月日_時分秒)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(ymd_hms({{col_sym}}))),
              "ydm_h(年日月_時)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(ydm_h({{col_sym}}))),
              "ydm_hm(年日月_時分)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(ydm_hm({{col_sym}}))),
              "ydm_hms(年日月_時分秒)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(ydm_hms({{col_sym}}))),
              "mdy_h(月日年_時)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(mdy_h({{col_sym}}))),
              "mdy_hm(月日年_時分)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(mdy_hm({{col_sym}}))),
              "mdy_hms(月日年_時分秒)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(mdy_hms({{col_sym}}))),
              "dmy_h(日月年_時)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(dmy_h({{col_sym}}))),
              "dmy_hm(日月年_時分)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(dmy_hm({{col_sym}}))),
              "dmy_hms(日月年_時分秒)"= df <- df %>% mutate("{{col_sym}}":= as_datetime(dmy_hms({{col_sym}})))
      )
    }
  )
  return(list(try_dttm_change,df))
}


#行の並べ替えを行う関数
arrangeRows <- function(dat,colname_arrange,a_or_d){
  df <- dat
  col_sym <- rlang::syms(colname_arrange)
  if(a_or_d =="昇順"){
    df <- df %>% arrange(!!!col_sym)
  }else{
    df <- df %>% arrange_(.dots = paste0("desc(", colname_arrange, ")"))
  }
  return(df)
}


#行をフィルタリングする関数
filterRows <- function(dat,colname_filter,filter_operator,filter_value){
  df <- dat
  col_sym <- rlang::sym(colname_filter)
  
  if((filter_operator !="欠損値を含む行を削除") & (filter_operator !="欠損値を含まない行を削除")){
    try_filter <- try(
      switch (filter_operator,
              '=' = df <- df %>% filter({{col_sym}} == eval(str2lang(filter_value))),
              '≠' = df <- df %>% filter({{col_sym}}!= eval(str2lang(filter_value))),
              ">" = df <- df %>% filter({{col_sym}} > eval(str2lang(filter_value))),
              ">=" = df <- df %>% filter({{col_sym}} >= eval(str2lang(filter_value))),
              "<" = df <- df %>% filter({{col_sym}}  < eval(str2lang(filter_value))),
              "<=" = df <- df %>% filter({{col_sym}}   <= eval(str2lang(filter_value))),
      )
    )
  }else if(filter_operator =="欠損値を含む行を削除"){
    try_filter <- try(
      if(class(df[[colname_filter]]) == "character"|class(df[[colname_filter]]) == "factor"){
        df <- df %>% filter({{col_sym}} != "")
      }else{
        df <- df %>% filter(!is.na({{col_sym}}))
      }
    )
  }else{
    try_filter <- try(
      if(class(df[[colname_filter]]) == "character"|class(df[[colname_filter]]) == "factor"){
        df <- df %>% filter({{col_sym}} == "")
      }else{
        df <- df %>% filter(is.na({{col_sym}}))
      }
    )
  }
  return(list(try_filter,df))
}


#変数を選択する関数
selectColumns <- function(dat,colname_select){
  select_col_sym <- rlang::syms(colname_select)
  df <- dat %>% select(!!!select_col_sym)
  return(df)
}

#数値データの欠損値埋めを行う関数
dealNumericna <- function(dat,colnameNAs,dealna_type,make_na_col,na_target_value,target_value =NULL){
  csv_data <- dat
  colname <- colnameNAs
  dest_col <- csv_data[colname]
  #NA判定の列名
  na_judge_colname <- paste0(colname,"_isBAD")
  
  forwards <- getForwardColumns(csv_data, colname)
  backwards <- getBackwardColumns(csv_data, colname)
  
  prep <- csv_data[colname]
  
  if(is.null(target_value)){
    if(dealna_type %in% c("平均","中央","最頻","特定値")){
      switch (dealna_type,
              平均 = target_value <- mean(dest_col[[colname]],na.rm = T),
              中央 = target_value <- median(dest_col[[colname]],na.rm = T),
              最頻 = target_value <- names(which.max(table(dest_col[[colname]]))),
              特定値 = target_value <- na_target_value
      )
      prep[[colname]] <- dest_col[[colname]] %>% replace_na(target_value)
    }else{
      switch (dealna_type,
              直前の非欠損値 = prep[[colname]] <- dest_col[[colname]] %>% na.locf(),
              前後非欠値による線形補完 = prep[[colname]] <- dest_col[[colname]] %>% na.approx()
      )
    }
  }
  
  
  if(make_na_col =="有"){
    prep[[na_judge_colname]] <- as.integer(is.na(dest_col[[colname]]))
  }
  
  csv_data <- repair_names(cbind(forwards, prep, backwards),sep = ".")
  return(list(csv_data,target_value))
}

#文字列データの欠損値置換を行う関数
dealStrna <- function(dat,colnameNAs_str,na_rep_str){
  df <- dat
  df[[colnameNAs_str]] <- df[[colnameNAs_str]] %>% str_replace_na(replacement = na_rep_str)
  return(df)
}


#ターゲットエンコーディングを行う関数
targetEncode <- function(dat,group_col,agg_col,new_col,tar_aggregation){
  
  if(new_col %in% colnames(dat)){
    i <- 1
    new_col_raw <- new_col
    while(new_col %in% colnames(dat)){
      i <- i+1
      new_col <- paste0(new_col_raw,".",i)
    }
  }
  
  group_col_sym <- rlang::sym(group_col)
  agg_col_sym <- rlang::sym(agg_col)
  new_col_sym <- rlang::sym(new_col)
  
  g_df <- dat %>% group_by({{group_col_sym}})
  
  if(new_col ==""){
    switch (tar_aggregation,
            '最大' = agg_df <- g_df %>% mutate("{{group_col_sym}}_{{agg_col_sym}}_max":=max({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            '最小' = agg_df <- g_df %>% mutate("{{group_col_sym}}_{{agg_col_sym}}_min":=min({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            '平均' = agg_df <- g_df %>% mutate("{{group_col_sym}}_{{agg_col_sym}}_mean":=mean({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            '分散' = agg_df <- g_df %>% mutate("{{group_col_sym}}_{{agg_col_sym}}_var":=var({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            '中央値' = agg_df <- g_df %>% mutate("{{group_col_sym}}_{{agg_col_sym}}_median":=median({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            '合計' = agg_df <- g_df %>% mutate("{{group_col_sym}}_{{agg_col_sym}}_sum":=sum({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            'カウント' = agg_df <- g_df %>% mutate("{{group_col_sym}}_{{agg_col_sym}}_count":=n()) %>% ungroup()
    )
    switch (tar_aggregation,
            '最大' = agg_map <- g_df %>% summarise("{{group_col_sym}}_{{agg_col_sym}}_max":=max({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            '最小' = agg_map <- g_df %>% summarise("{{group_col_sym}}_{{agg_col_sym}}_min":=min({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            '平均' = agg_map <- g_df %>% summarise("{{group_col_sym}}_{{agg_col_sym}}_mean":=mean({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            '分散' = agg_map <- g_df %>% summarise("{{group_col_sym}}_{{agg_col_sym}}_var":=var({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            '中央値' = agg_map <- g_df %>% summarise("{{group_col_sym}}_{{agg_col_sym}}_median":=median({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            '合計' = agg_map <- g_df %>% summarise("{{group_col_sym}}_{{agg_col_sym}}_sum":=sum({{agg_col_sym}},na.rm = TRUE)) %>% ungroup(),
            'カウント' = agg_map <- g_df %>% summarise("{{group_col_sym}}_{{agg_col_sym}}_count":=n()) %>% ungroup()
    )
  }else{
    switch (tar_aggregation,
            '最大' = agg_df <- g_df %>% mutate("{{new_col_sym}}":=max({{agg_col_sym}},na.rm = TRUE)),
            '最小' = agg_df <- g_df %>% mutate("{{new_col_sym}}":=min({{agg_col_sym}},na.rm = TRUE)),
            '平均' = agg_df <- g_df %>% mutate("{{new_col_sym}}":=mean({{agg_col_sym}},na.rm = TRUE)),
            '分散' = agg_df <- g_df %>% mutate("{{new_col_sym}}":=var({{agg_col_sym}},na.rm = TRUE)),
            '中央値' = agg_df <- g_df %>% mutate("{{new_col_sym}}":=median({{agg_col_sym}},na.rm = TRUE)),
            '合計' = agg_df <- g_df %>% mutate("{{new_col_sym}}":=sum({{agg_col_sym}},na.rm = TRUE)),
            'カウント' = agg_df <- g_df %>% mutate("{{new_col_sym}}":=n())
    )
    switch (tar_aggregation,
            '最大' = agg_map <- g_df %>% summarise("{{new_col_sym}}":=max({{agg_col_sym}},na.rm = TRUE)),
            '最小' = agg_map <- g_df %>% summarise("{{new_col_sym}}":=min({{agg_col_sym}},na.rm = TRUE)),
            '平均' = agg_map <- g_df %>% summarise("{{new_col_sym}}":=mean({{agg_col_sym}},na.rm = TRUE)),
            '分散' = agg_map <- g_df %>% summarise("{{new_col_sym}}":=var({{agg_col_sym}},na.rm = TRUE)),
            '中央値' = agg_map <- g_df %>% summarise("{{new_col_sym}}":=median({{agg_col_sym}},na.rm = TRUE)),
            '合計' = agg_map <- g_df %>% summarise("{{new_col_sym}}":=sum({{agg_col_sym}},na.rm = TRUE)),
            'カウント' = agg_map <- g_df %>% summarise("{{new_col_sym}}":=n())
    )
  }
  return(list(agg_df,agg_map))
}

#ラグ変数生成を行う関数
makeLag <- function(dat,num_lag,colname_lag){
  df <- dat
  for(i in 1:num_lag){
    col_sym <- rlang::sym(colname_lag)
    df <- df %>% mutate("{{col_sym}}_Lag{i}":= lag({{col_sym}},i))
  }
  return(df)
}

#リード変数生成を行う関数
makeLead <- function(dat,num_lead,colname_lead){
  df <- dat
  for(i in 1:num_lead){
    col_sym <- rlang::sym(colname_lead)
    df <- df %>% mutate("{{col_sym}}_Lead{i}":= lead({{col_sym}},i))
  }
  return(df)
}

#移動集計を行う関数
makeRoll <- function(dat,new_col_roll,colname_roll,time_index_roll,aggmethod_roll,before_roll,after_roll,roll_type){
  #新しい変数名が入力されていないときはV+変数番号を変数名とする
  if(new_col_roll==""){
    col_num <- ncol(dat)
    new_col <- paste0("V", col_num+1)
  }else{
    new_col <- new_col_roll
  }
  
  
  if(roll_type =="前方全ての要素を集計"){
    before_roll <- Inf
    after_roll <- 0
  }
  if(roll_type =="後方全ての要素を集計"){
    before_roll <- 0
    after_roll <- Inf
  }
  
  new_col_sym <- rlang::sym(new_col)
  col_sym <- rlang::sym(colname_roll)
  time_index_sym<- rlang::sym(time_index_roll)
  
  df <- dat
  
  #時系列インデックスの指定がない場合
  if(time_index_roll == 'None'){
    switch (aggmethod_roll,
            '最大' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_vec(.x = {{col_sym}},.f = ~max(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T)),
            '最小' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_vec(.x = {{col_sym}},.f = ~min(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T)),
            '平均' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_vec(.x = {{col_sym}},.f = ~mean(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T)),
            '分散' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_vec(.x = {{col_sym}},.f = ~var(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T)),
            '中央値' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_vec(.x = {{col_sym}},.f = ~median(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T)),
            '合計' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_vec(.x = {{col_sym}},.f = ~sum(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T))
    )
    
    #時系列インデックスの指定がある場合
  }else{
    
    #インデックスを用いて昇順に並び替える(これをしないとslide_index_dblでエラーが発生)
    df <- df %>% arrange({{time_index_sym}})
    
    switch (aggmethod_roll,
            '最大' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_index_dbl(.x = {{col_sym}},.i ={{time_index_sym}} ,.f = ~max(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T)),
            '最小' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_index_dbl(.x = {{col_sym}},.i ={{time_index_sym}},.f = ~min(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T)),
            '平均' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_index_dbl(.x = {{col_sym}},.i ={{time_index_sym}},.f = ~mean(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T)),
            '分散' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_index_dbl(.x = {{col_sym}},.i ={{time_index_sym}},.f = ~var(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T)),
            '中央値' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_index_dbl(.x = {{col_sym}},.i ={{time_index_sym}},.f = ~median(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T)),
            '合計' = df <- df %>%
              mutate("{{new_col_sym}}" := slide_index_dbl(.x = {{col_sym}},.i ={{time_index_sym}},.f = ~sum(.x,na.rm = TRUE), .before = before_roll, .after= after_roll,.complete = T))
    )
  }
  return(df)
}

#文字列の含有判定を行う関数
strDetect <- function(dat,new_col_str_detect,colname_str_detect,partial_string){
  df <- dat
  df[[new_col_str_detect]] <- df[[colname_str_detect]] %>% as.character() %>% str_detect(partial_string) %>% as.integer()
  return(df)
}

#文字列抽出を行う関数
strSub <- function(dat,new_col_str_sub,colname_str_sub,str_sub_start,str_sub_end){
  df <- dat
  df[[new_col_str_sub]] <- df[[colname_str_sub]] %>% as.character() %>% str_sub(str_sub_start,str_sub_end)
  return(df)
}

#文字列の置換を行う関数
strReplace <- function(dat,colname_str_rep,str_rep_before,str_rep_after){
  df <- dat
  df[[colname_str_rep]] <- df[[colname_str_rep]] %>% as.character() %>% str_replace(str_rep_before,str_rep_after)
  return(df)
}

dttmSub <- function(dat,dttm_sub_new_col,colname_dttm_sub,dttm_sub_element){
  df <- dat
  switch(dttm_sub_element,
         年 = df[[dttm_sub_new_col]] <- df[[colname_dttm_sub]] %>% year(),
         月= df[[dttm_sub_new_col]] <- df[[colname_dttm_sub]] %>% month(),
         日= df[[dttm_sub_new_col]] <- df[[colname_dttm_sub]] %>% day(),
         時= df[[dttm_sub_new_col]] <- df[[colname_dttm_sub]] %>% hour(),
         分= df[[dttm_sub_new_col]] <- df[[colname_dttm_sub]] %>% minute(),
         秒= df[[dttm_sub_new_col]] <- df[[colname_dttm_sub]] %>% second(),
  )
  return(df)
}


safeUpdateSelectInput <- function(input, session, inputId, choices, selected){
  # 中村コメントアウト 欠損値処理等を実施した際に新しい変数名がインプットに現れなかった。
  # if (input[[inputId]] != selected){
  updateSelectInput(session, inputId, choices=choices, selected=selected)
  # }
}

# 列の名前から番号を取り出す(左端が1)
getColumnIndexbyName <- function(data, name){
  cols <- colnames(data)
  if(name==""){
    return(1)
  }
  index <- match(name, cols)
  if (length(index)>=1 && !is.na(index)){
    return (index[1])
  }
  index <- grep(paste0("^", name, "_"), cols)
  if (length(index)>=1 && !is.na(index)){
    return (index[1])
  }
  return(1)
}

# 選ばれている列を更新する
updateSelected <- function(input, output, session, values){
  colInfo <- colnames(values$dat)
  
  # print(colInfo)
  
  c_name <- as.data.frame(colnames(values$dat))
  datatype <- as.data.frame(t(as.data.frame(lapply(values$dat, class))))[,1]
  datatype <- as.data.frame(cbind(c_name,datatype))
  
  rownames(datatype) <- NULL
  colnames(datatype) <- c('変数名','変数型')
  
  colInfo_num_int <- datatype[datatype[['変数型']]=='numeric'|datatype[['変数型']]=='integer',][['変数名']]
  colInfo_time <- datatype[datatype[['変数型']]=='Date'|datatype[['変数型']]=='POSIXct',][['変数名']]
  colInfo_str <- datatype[datatype[['変数型']]=="character",][['変数名']]
  
  selected <- colInfo[input$columnInfo_rows_selected]
  
  if(selected %in% colInfo_num_int){
    selected_NA <- selected
  }else{
    selected_NA <- 'None'
  }
  
  if(selected %in% colInfo_time){
    selected_time <- selected
  }else{
    selected_time <- 'None'
  }
  
  if(selected %in% colInfo_str){
    selected_str <- selected
  }else{
    selected_str <- 'None'
  }
  
  safeUpdateSelectInput(input, session, "colnameOnehot", choices=colInfo, selected=selected)
  safeUpdateSelectInput(input, session, "colnameNAs", choices=c('None',colInfo_num_int), selected=selected_NA)
  safeUpdateSelectInput(input, session, "colnameNAs_str", choices=c('None',colInfo_str), selected=selected_str)
  safeUpdateSelectInput(input, session, "colnameChange", choices=colInfo, selected=selected)
  safeUpdateSelectInput(input, session, "colnameDelete", choices=colInfo, selected=NULL)
  
  ######中村追加
  safeUpdateSelectInput(input, session, "colnameCombine1", choices=colInfo, selected=NULL)
  # safeUpdateSelectInput(input, session, "colnameCombine2", choices=colInfo, selected=selected)
  safeUpdateSelectInput(input, session, "colnameSplit", choices=colInfo, selected=selected)
  safeUpdateSelectInput(input, session, "colnameCalc", choices=colInfo, selected=selected)
  
  ## 鈴木追加
  safeUpdateSelectInput(input, session, "colnameJoin", choices=colInfo, selected=selected)
  
  safeUpdateSelectInput(input, session, "tar_groupby", choices = colInfo,selected= NULL)
  safeUpdateSelectInput(input, session, "tar_agg_colname", choices = colInfo_num_int,selected= NULL)
  safeUpdateSelectInput(input, session, "colname_lag", choices=colInfo, selected=selected)
  safeUpdateSelectInput(input, session, "colname_lead", choices=colInfo, selected=selected)
  safeUpdateSelectInput(input, session, "colname_roll", choices=c('None',colInfo_num_int), selected=selected_NA)
  safeUpdateSelectInput(input, session, "time_index_roll", choices=c('None',colInfo_time), selected='None')
  
  safeUpdateSelectInput(input, session, "colname_arrange", choices=colInfo, selected=NULL)
  safeUpdateSelectInput(input, session, "colname_filter", choices=colInfo, selected=selected)
  safeUpdateSelectInput(input, session, "colname_select", choices=colInfo, selected= NULL)
  
  safeUpdateSelectInput(input, session, "colname_classchange", choices=colInfo, selected=selected)
  safeUpdateSelectInput(input, session, "colname_dttmchange", choices=colInfo, selected=selected)
  
  
  safeUpdateSelectInput(input, session, "colname_str_sub", choices=colInfo, selected=selected)
  safeUpdateSelectInput(input, session, "colname_str_detect", choices=colInfo, selected=selected)
  safeUpdateSelectInput(input, session, "colname_str_rep", choices=colInfo, selected=selected)
  
  safeUpdateSelectInput(input, session, "dr_targetcolname", choices=colInfo, selected=selected)
  
  safeUpdateSelectInput(input, session, "colname_dttm_sub", choices=c('None',colInfo_time), selected='None')
  
}

# テストデータテーブルを更新する
updateTestData <- function(df, input, output, session, values){
  values$dat_test <- df
  output$outputTestTable <- DT::renderDataTable({
    values$dat_test
  }, extensions = c('Scroller','FixedColumns'), colnames = c(No. = 1), class = 'cell-border stripe',
  options = list(
    deferRender = TRUE,
    dom = 'rti',
    scrollY = 500,
    scrollX = TRUE,
    scroller = TRUE,
    fixedColumns = TRUE,
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#1f618d', 'color': '#fff'});",
      "}")
  )
  )
}


# 指定した列より前(左側)の列を取得
getForwardColumns <- function(data, colname){
  cols <- colnames(data)
  num <- match(colname, cols)
  forwards <- data[,numeric(0)]  
  if(num > 2){
    forwards <- data[,1:(num-1)]
  }else if (num==2){
    forwards[cols[1]] <- data[, 1]
  }
  return(forwards)
}

# 指定した列より後ろ(右側)の列を取得
getBackwardColumns <- function(data, colname){
  cols <- colnames(data)
  num <- match(colname, cols)
  len_cols <- length(cols)
  backwards <- data[,numeric(0)]
  if (num<len_cols-1){
    backwards <- data[,(num+1):len_cols]
  }else if (num==len_cols-1){
    backwards[cols[len_cols]] <- data[,len_cols]
  }
  return(backwards)
}

# 列の名前を変更する
renameColumn <- function(df, oldname, newname){
  forwards <- getForwardColumns(df, oldname)
  backwards <- getBackwardColumns(df, oldname)
  dest_frame <- df[,numeric(0)]
  dest_frame[newname] <- df[,oldname]
  return(cbind(forwards, dest_frame, backwards))
}

# 列を削除する
# removeColumn <- function(df, colname){
#   # print(colname)
#   return(df[,colnames(df)!=colname])
# }
#変数名を複数選択できるバージョン
removeColumn <- function(dat, colnameDelete){
  # print(colname)
  select_col_sym <- rlang::syms(colnameDelete)
  delet_colname <- dat %>% select(!!!select_col_sym) %>% colnames()
  df <- dat[setdiff(colnames(dat), delet_colname)]
  return(df)
}

# 鈴木追加: 結合
joinDataframe <- function(to_data, from_data, key, type) {
  switch(type,
         "内部結合"  = dat <- merge(to_data, from_data, by=key),
         "(左)外部結合"  = dat <- merge(to_data, from_data, all.x=T, by=key),
         "完全外部結合"  = dat <- merge(to_data, from_data, all=T, by=key),
  )
  dat
}

# vtreatで使う変換を定義したデータフレームをソートする
sortScoreFrameByOrigName <- function(scoreFrame, data){
  cols <- colnames(data)
  ret_frame <- scoreFrame[numeric(0),]
  for (i in cols){
    ret_frame <- (filter(scoreFrame, scoreFrame$origName==i) %>% rbind(ret_frame, .))
  }
  return(ret_frame)
}

# vtreatのonehot化で生成された列の名前を短縮する
shortenVarName <- function(scoreFrame){
  rem <-  nchar("_lev_x")
  ret_frame <- data.frame(varName=character(0), newName=character(0))
  for(i in 1:nrow(scoreFrame)){
    record <- scoreFrame[i, ]
    
    # print(record$code=="lev")
    if (record$code=="lev"){
      
      # print(record$varName)
      # print(str_detect(record$varName,'_lev_NA'))
      
      #値がNA(空白文字)の場合
      if(str_detect(record$varName,'_lev_NA|_lev_x_$')){
        newName <-  '_NA'
      }else{
        newName <-  substr(record$varName, nchar(record$origName)+rem+1, nchar(record$varName))
      }
      
      ret_frame <- rbind(ret_frame, data.frame(varName=c(record$varName), newName=c(paste0(record$origName, newName))))
    }
  }
  return(ret_frame)
}

# Rio: Peak detection
argmax  <- function(x, y, w=1, ...) {
  require(zoo)
  n <- length(y)
  y.smooth <- loess(y ~ as.numeric(x), ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

argmin  <- function(x, y, w=1, ...) {
  require(zoo)
  n <- length(y)
  y.smooth <- loess(y ~ as.numeric(x), ...)$fitted
  y.min <- rollapply(zoo(y.smooth), 2*w+1, min, align="center")
  delta <- y.min - y.smooth[-c(1:w, n+1-1:w)]
  i.min <- which(delta >= 0) + w
  list(x=x[i.min], i=i.min, y.hat=y.smooth)
}

ggplotWithPeaks <- function(type, data, x, y, w, span) {
  x <- data[[x]]
  y <- data[[y]]
  if(type == "max") {
    peaks <- argmax(x, y, w=w, span=span)
    xPeaks <-  x[peaks$i]
    yPeaks <-  peaks$y.hat[peaks$i]
  } else if (type == "min") {
    peaks <- argmin(x, y, w=w, span=span)
    xPeaks <-  x[peaks$i]
    yPeaks <-  peaks$y.hat[peaks$i]
  } else {
    max_peaks <- argmax(x, y, w=w, span=span)
    min_peaks <- argmin(x, y, w=w, span=span)
    peaks <- list(x=append(max_peaks$x, min_peaks$x), i=append(max_peaks$i, min_peaks$i), y.hat=max_peaks$y.hat)
    xPeaks <-  x[peaks$i]
    yPeaks <-  peaks$y.hat[peaks$i]
  }
  peaks_df <- data.frame(xPeaks=xPeaks, yPeaks=yPeaks)
  ggplot(data = data, mapping=aes(x=x, y=y)) + 
    geom_point(color='grey') + 
    geom_line(aes(x = x, y = peaks$y.hat)) + 
    geom_point(data=peaks_df, mapping=aes(x=xPeaks, y=yPeaks), color='red')
}

# Rio: Event Label Output
makeEventLabel <- function(dat, x, y, w=1, span=0.05, type){
  df <- dat
  x <- dat[[x]]
  y <- dat[[y]]
  switch(type,
         "max" = peaks <- argmax(x, y, w=w, span=span),
         "min" = peaks <- argmin(x, y, w=w, span=span)
  )
  if(type=="max+min") {
    max_peaks <- argmax(x, y, w=w, span=span)
    min_peaks <- argmin(x, y, w=w, span=span)
    max_data <- c()
    min_data <- c()
    for(i in rownames(df)){
      if(i %in% max_peaks$i){
        max_data <- c(max_data, 1)
      }else{
        max_data <- c(max_data, 0)
      }
    }
    for(i in rownames(df)){
      if(i %in% min_peaks$i){
        min_data <- c(min_data, 1)
      }else{
        min_data <- c(min_data, 0)
      }
    }
    df["max_label"] = max_data
    df["min_label"] = min_data
  }else{
    data <- c()
    for(i in rownames(df)){
      if(i %in% peaks$i){
        data <- c(data, 1)
      }else{
        data <- c(data, 0)
      }
    }
    text <- "label"
    colname = paste(type, text, sep = "_")
    df[colname] = data
  }
  return(df)
}

# データテーブルを更新する
updateData <- function(df, input, output, session, values){
  values$dat <- df
  
  colInfo <-  colnames(values$dat)

  selected <- input$colnameOnehot

  output$processed_table <- renderUI({
    DetailsList(items = values$dat, checkboxVisibility = 2)
  })
  
  output$origin_table <- renderUI({
    DetailsList(items = values$origin, checkboxVisibility = 2)
  })
  
  output$originTable <- DT::renderDataTable({
    values$origin
  }, extensions = c('Scroller','FixedColumns'), colnames = c(No. = 1), class = 'cell-border stripe',
  options = list(
    deferRender = TRUE,
    dom = 'rti',
    scrollY = 500,
    scrollX = TRUE,
    scroller = TRUE,
    fixedColumns = TRUE,
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#ffffff', 'color': '#c99000'});",
      "}")
  )
  )
  output$outputTable <- DT::renderDataTable({
    values$dat
  }, filter = 'top', extensions = c('Scroller','FixedColumns'), colnames = c(No. = 1), class = 'cell-border stripe',
  options = list(
    autoWidth = TRUE,
    deferRender = TRUE,
    dom = 'rti',
    scrollY = 500,
    scrollX = TRUE,
    scroller = TRUE,
    fixedColumns = TRUE,
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#ffffff', 'color': '#c99000'});",
      "}")
  )
  )
  
  output$download <-  downloadHandler(
    filename <-  "prepared_train_data.csv",
    content <-  function(file) {
      
      #一旦文字列に変換する(POSIXctを文字列に変換するため)
      data <- values$dat %>% map_df(as.character())
      write.csv(data, file)
    }
  )
  
  output$download_test <-  downloadHandler(
    filename <-  "prepared_test_data.csv",
    content <-  function(file) {
      
      #一旦文字列に変換する(POSIXctを文字列に変換するため)
      data <- values$dat_test %>% map_df(as.character())
      write.csv(data, file)
    }
  )
  
  output$save_treatment <-  downloadHandler(
    filename <-  "treatment.obj",
    content <-  function(file) {
      saveRDS(values$history, file)
    }
  )
  
  output$columnInfo <- DT::renderDataTable({
    # data.frame(変数名=colInfo)
    c_name <- as.data.frame(colnames(values$dat))
    datatype <- as.data.frame(t(as.data.frame(lapply(values$dat, class))))[,1]
    datatype <- as.data.frame(cbind(c_name,datatype))
    rownames(datatype) <- NULL
    colnames(datatype) <- c('変数名','変数型')
    datatypes <- datatype
    values$dattypes <- datatypes
    datatypes
    
  }, server = FALSE, selection=list(mode="single", selected=getColumnIndexbyName(df, selected)),extensions = c('Scroller'),colnames = c(No. = 1),
  options = list(dom = 'fti',scrollY = 300, scroller = TRUE,
                 initComplete = DT::JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({});",
                   "}")))
}

# 時系列クラスタリングの結果をプロットする関数
plot_group <- function(plot.data, cluster = NULL, method = '',select_class =NULL) {
  span <- nrow(plot.data)
  plot.data$index <- 1:span
  plot.data <- gather(plot.data, variable, value, -index)
  cluster <- as.factor(cluster)
  plot.data$colour <- rep(cluster, rep(span, length(cluster)))
  
  if(!is.null(select_class)){
    plot.data <- plot.data %>% filter(colour %in% select_class)
  }
  p <- ggplot(plot.data, aes(x = index, y = value, group = variable, colour = colour)) +
    geom_line() +
    xlab('') + ylab('') + ggtitle(method)
  return(p)
}