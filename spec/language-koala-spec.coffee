describe 'Koala settings', ->
  [editor, languageMode] = []

  afterEach ->
    editor.destroy()

  beforeEach ->
    waitsForPromise ->
      atom.workspace.open('sample.koala').then (o) ->
        editor = o
        languageMode = editor.languageMode

    waitsForPromise ->
      atom.packages.activatePackage('language-koala')

  it 'matches lines correctly using the increaseIndentPattern', ->
    increaseIndentRegex = languageMode.increaseIndentRegexForScopeDescriptor(['source.koala'])

    expect(increaseIndentRegex.testSync('  case true:')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  default:')).toBeTruthy()
    expect(increaseIndentRegex.testSync('func something() {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  if true {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  else {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  switch {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  switch true {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  select {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  select true {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  for v := range val {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  for i := 0; i < 10; i++ {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  for i := 0; i < 10; i++ {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  type something struct {')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  fmt.Printf("some%s",')).toBeTruthy()
    expect(increaseIndentRegex.testSync('  aSlice := []string{}{')).toBeTruthy()

  it 'matches lines correctly using the decreaseIndentPattern', ->
    decreaseIndentRegex = languageMode.decreaseIndentRegexForScopeDescriptor(['source.koala'])

    expect(decreaseIndentRegex.testSync('  case true:')).toBeTruthy()
    expect(decreaseIndentRegex.testSync('  default:')).toBeTruthy()
    expect(decreaseIndentRegex.testSync('  }')).toBeTruthy()
    expect(decreaseIndentRegex.testSync('  },')).toBeTruthy()
    expect(decreaseIndentRegex.testSync('  )')).toBeTruthy()
    expect(decreaseIndentRegex.testSync('  ),')).toBeTruthy()

  it 'matches lines correctly using the decreaseNextIndentPattern', ->
    decreaseNextIndentRegex = languageMode.decreaseNextIndentRegexForScopeDescriptor(['source.koala'])

    expect(decreaseNextIndentRegex.testSync('  fmt.Println("something"))')).toBeTruthy()
    expect(decreaseNextIndentRegex.testSync('  fmt.Println("something")),')).toBeTruthy()
    expect(decreaseNextIndentRegex.testSync('  fmt.Println("something"), "x"),')).toBeTruthy()
    expect(decreaseNextIndentRegex.testSync('  fmt.Println(fmt.Sprint("something"))),')).toBeTruthy()
    expect(decreaseNextIndentRegex.testSync('  fmt.Println(fmt.Sprint("something"), "x")),')).toBeTruthy()

    expect(decreaseNextIndentRegex.testSync('  fmt.Println("something")')).toBeFalsy()
    expect(decreaseNextIndentRegex.testSync('  fmt.Println("something"),')).toBeFalsy()

    # a line with many (), testing for catastrophic backtracking.
    # see https://github.com/atom/language-koala/issues/78
    longLine = 'first.second().third().fourth().fifth().sixth().seventh().eighth().ninth().tenth()'
    expect(decreaseNextIndentRegex.testSync(longLine)).toBeFalsy()
