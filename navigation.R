navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'オリジナルデータ', url = '#!/home', key = 'home', icon = 'Table', isExpanded= TRUE),
      list(name = '前処理', url = '#!/preprocess', key = 'preprocess', icon = 'TripleColumnEdit'),
      list(name = '可視化', url = '#!/visualize', key = 'visualize', icon = 'LineChart'),
      list(name = '学習', url = '#!/learn', key = 'learn', icon = 'SearchAndApps'),
      list(name = 'ダッシュボード', url = '#!/dashboard', key = 'dashboard', icon = 'ViewDashboard')
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