navigation <- shiny.fluent::Nav(
  groups = list(
    list(links = list(
      list(name = 'オリジナルデータ', url = '#!/home', key = 'home', icon = 'Coffee', isExpanded= TRUE),
      list(name = '前処理', url = '#!/preprocess', key = 'preprocess', icon = 'Filter'),
      list(name = 'ダッシュボード', url = '#!/dashboard', key = 'dashboard', icon = 'CoffeeScript')
    ))
  ),
  initialSelectedKey = 'preprocess',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)