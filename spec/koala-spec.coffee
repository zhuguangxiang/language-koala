describe 'Koala grammar', ->
  grammar = null

  beforeEach ->
    waitsForPromise ->
      atom.packages.activatePackage('language-koala')

    runs ->
      grammar = atom.grammars.grammarForScopeName('source.koala')

  it 'parses the grammar', ->
    expect(grammar).toBeTruthy()
    expect(grammar.scopeName).toBe 'source.koala'

  it 'tokenizes comments', ->
    {tokens} = grammar.tokenizeLine('// I am a comment')
    expect(tokens[0].value).toEqual '//'
    expect(tokens[0].scopes).toEqual ['source.koala', 'comment.line.double-slash.koala', 'punctuation.definition.comment.koala']
    expect(tokens[1].value).toEqual ' I am a comment'
    expect(tokens[1].scopes).toEqual ['source.koala', 'comment.line.double-slash.koala']

    tokens = grammar.tokenizeLines('/*\nI am a comment\n*/')
    expect(tokens[0][0].value).toEqual '/*'
    expect(tokens[0][0].scopes).toEqual ['source.koala', 'comment.block.koala', 'punctuation.definition.comment.koala']
    expect(tokens[1][0].value).toEqual 'I am a comment'
    expect(tokens[1][0].scopes).toEqual ['source.koala', 'comment.block.koala']
    expect(tokens[2][0].value).toEqual '*/'
    expect(tokens[2][0].scopes).toEqual ['source.koala', 'comment.block.koala', 'punctuation.definition.comment.koala']

  it 'tokenizes comments in imports', ->
    lines = grammar.tokenizeLines '''
      import (
        // comment!
      )
    '''
    expect(lines[1][1]).toEqual value: '//', scopes: ['source.koala', 'comment.line.double-slash.koala', 'punctuation.definition.comment.koala']

  it 'tokenizes strings', ->
    delims =
      'string.quoted.double.koala': '"'
      'string.quoted.raw.koala': '`'

    for scope, delim of delims
      {tokens} = grammar.tokenizeLine(delim + 'I am a string' + delim)
      expect(tokens[0].value).toEqual delim
      expect(tokens[0].scopes).toEqual ['source.koala', scope, 'punctuation.definition.string.begin.koala']
      expect(tokens[1].value).toEqual 'I am a string'
      expect(tokens[1].scopes).toEqual ['source.koala', scope]
      expect(tokens[2].value).toEqual delim
      expect(tokens[2].scopes).toEqual ['source.koala', scope, 'punctuation.definition.string.end.koala']

  it 'tokenizes placeholders in strings', ->
    # Taken from koala/src/pkg/fmt/fmt_test.koala
    verbs = [
      '%# x', '%-5s', '%5s', '%05s', '%.5s', '%10.1q', '%10v', '%-10v', '%.0d'
      '%.d', '%+07.2f', '%0100d', '%0.100f', '%#064x', '%+.3F', '%-#20.8x',
      '%[1]d', '%[2]*[1]d', '%[3]*.[2]*[1]f', '%[3]*.[2]f', '%3.[2]d', '%.[2]d'
      '%-+[1]x', '%d', '%-d', '%+d', '%#d', '% d', '%0d', '%1.2d', '%-1.2d'
      '%+1.2d', '%-+1.2d', '%*d', '%.*d', '%*.*d', '%0*d', '%-*d'
    ]

    for verb in verbs
      {tokens} = grammar.tokenizeLine('"' + verb + '"')
      expect(tokens[0].value).toEqual '"',
      expect(tokens[0].scopes).toEqual ['source.koala', 'string.quoted.double.koala', 'punctuation.definition.string.begin.koala']
      expect(tokens[1].value).toEqual verb
      expect(tokens[1].scopes).toEqual ['source.koala', 'string.quoted.double.koala', 'constant.other.placeholder.koala']
      expect(tokens[2].value).toEqual '"',
      expect(tokens[2].scopes).toEqual ['source.koala', 'string.quoted.double.koala', 'punctuation.definition.string.end.koala']

  it 'tokenizes character escapes in strings', ->
    escapes = [
      '\\a', '\\b', '\\f', '\\n', '\\r', '\\t', '\\v', '\\\\'
      '\\000', '\\007', '\\377', '\\x07', '\\xff', '\\u12e4', '\\U00101234'
    ]

    for escape in escapes
      {tokens} = grammar.tokenizeLine('"' + escape + '"')
      expect(tokens[1].value).toEqual escape
      expect(tokens[1].scopes).toEqual ['source.koala', 'string.quoted.double.koala', 'constant.character.escape.koala']

    {tokens} = grammar.tokenizeLine('"\\""')
    expect(tokens[1].value).toEqual '\\"'
    expect(tokens[1].scopes).toEqual ['source.koala', 'string.quoted.double.koala', 'constant.character.escape.koala']

  it 'tokenizes placeholders in raw strings', ->
    # Taken from koala/src/pkg/fmt/fmt_test.koala
    verbs = [
      '%# x', '%-5s', '%5s', '%05s', '%.5s', '%10.1q', '%10v', '%-10v', '%.0d'
      '%.d', '%+07.2f', '%0100d', '%0.100f', '%#064x', '%+.3F', '%-#20.8x',
      '%[1]d', '%[2]*[1]d', '%[3]*.[2]*[1]f', '%[3]*.[2]f', '%3.[2]d', '%.[2]d'
      '%-+[1]x', '%d', '%-d', '%+d', '%#d', '% d', '%0d', '%1.2d', '%-1.2d'
      '%+1.2d', '%-+1.2d', '%*d', '%.*d', '%*.*d', '%0*d', '%-*d'
    ]

    for verb in verbs
      {tokens} = grammar.tokenizeLine('`' + verb + '`')
      expect(tokens[0].value).toEqual '`',
      expect(tokens[0].scopes).toEqual ['source.koala', 'string.quoted.raw.koala', 'punctuation.definition.string.begin.koala']
      expect(tokens[1].value).toEqual verb
      expect(tokens[1].scopes).toEqual ['source.koala', 'string.quoted.raw.koala', 'constant.other.placeholder.koala']
      expect(tokens[2].value).toEqual '`',
      expect(tokens[2].scopes).toEqual ['source.koala', 'string.quoted.raw.koala', 'punctuation.definition.string.end.koala']

  it 'tokenizes runes', ->
    verbs = [
      'u', 'X', '$', ':', '(', '.', '2', '=', '!', '@',
      '\\a', '\\b', '\\f', '\\n', '\\r', '\\t', '\\v', '\\\\'
      '\\000', '\\007', '\\377', '\\x07', '\\xff', '\\u12e4', '\\U00101234'
    ]

    for verb in verbs
      {tokens} = grammar.tokenizeLine('\'' + verb + '\'')
      expect(tokens[0].value).toEqual '\'' + verb + '\'',
      expect(tokens[0].scopes).toEqual ['source.koala', 'constant.other.rune.koala']

  it 'tokenizes invalid runes and single quoted strings', ->
    {tokens} = grammar.tokenizeLine('\'ab\'')
    expect(tokens[0].value).toEqual '\'ab\''
    expect(tokens[0].scopes).toEqual ['source.koala', 'invalid.illegal.unknown-rune.koala']

    {tokens} = grammar.tokenizeLine('\'some single quote string\'')
    expect(tokens[0].value).toEqual '\'some single quote string\''
    expect(tokens[0].scopes).toEqual ['source.koala', 'invalid.illegal.unknown-rune.koala']

  it 'tokenizes invalid whitespace around chan annotations', ->
    invalid_send =
      'chan <- sendonly': ' '

    invalid_receive =
      '<- chan recvonly': ' '

    for expr, invalid of invalid_send
      {tokens} = grammar.tokenizeLine(expr)
      expect(tokens[1].value).toEqual invalid
      expect(tokens[1].scopes).toEqual ['source.koala', 'invalid.illegal.send-channel.koala']

    for expr, invalid of invalid_receive
      {tokens} = grammar.tokenizeLine(expr)
      expect(tokens[1].value).toEqual invalid
      expect(tokens[1].scopes).toEqual ['source.koala', 'invalid.illegal.receive-channel.koala']

  it 'tokenizes keywords', ->
    keywordLists =
      'keyword.control.koala': ['break', 'case', 'continue', 'default', 'defer', 'else', 'fallthrough', 'for', 'go', 'goto', 'if', 'range', 'return', 'select', 'switch']
      'keyword.channel.koala': ['chan']
      'keyword.const.koala': ['const']
      'keyword.function.koala': ['func']
      'keyword.interface.koala': ['interface']
      'keyword.import.koala': ['import']
      'keyword.map.koala': ['map']
      'keyword.package.koala': ['package']
      'keyword.struct.koala': ['struct']
      'keyword.type.koala': ['type']
      'keyword.var.koala': ['var']

    for scope, list of keywordLists
      for keyword in list
        {tokens} = grammar.tokenizeLine keyword
        expect(tokens[0].value).toEqual keyword
        expect(tokens[0].scopes).toEqual ['source.koala', scope]

  it 'tokenizes storage types', ->
    storageTypes =
      'storage.type.boolean.koala': ['bool']
      'storage.type.byte.koala': ['byte']
      'storage.type.error.koala': ['error']
      'storage.type.numeric.koala': ['int', 'int8', 'int16', 'int32', 'int64', 'uint', 'uint8', 'uint16', 'uint32', 'uint64', 'float32', 'float64', 'complex64', 'complex128']
      'storage.type.rune.koala': ['rune']
      'storage.type.string.koala': ['string']
      'storage.type.uintptr.koala': ['uintptr']

    for scope, types of storageTypes
      for type in types
        {tokens} = grammar.tokenizeLine type
        expect(tokens[0].value).toEqual type
        expect(tokens[0].scopes).toEqual ['source.koala', scope]

  it 'tokenizes func regardless of the context', ->
    funcKeyword = ['func f()', 'func (x) f()', 'func(x) f()', 'func']
    for line in funcKeyword
      {tokens} = grammar.tokenizeLine line
      expect(tokens[0].value).toEqual 'func'
      expect(tokens[0].scopes).toEqual ['source.koala', 'keyword.function.koala']

    funcType = [
      {
        'line': 'var f1 func('
        'tokenPos': 4
      }
      {
        'line': 'f2 :=func()'
        'tokenPos': 3
      }
      {
        'line': '\tfunc('
        'tokenPos': 1
      }
      {
        'line': 'type HandlerFunc func('
        'tokenPos': 4
      }
    ]
    for t in funcType
      {tokens} = grammar.tokenizeLine t.line
      relevantToken = tokens[t.tokenPos]
      expect(relevantToken.value).toEqual 'func'
      expect(relevantToken.scopes).toEqual ['source.koala', 'keyword.function.koala']

      next = tokens[t.tokenPos + 1]
      expect(next.value).toEqual '('
      expect(next.scopes).toEqual ['source.koala', 'punctuation.other.bracket.round.koala']

  it 'only tokenizes func when it is an exact match', ->
    tests = ['myfunc', 'funcMap']
    for test in tests
      {tokens} = grammar.tokenizeLine test
      expect(tokens[0].value).not.toEqual 'func'
      expect(tokens[0].scopes).not.toEqual ['source.koala', 'keyword.function.koala']

  it 'tokenizes func names in their declarations', ->
    tests = [
      {
        'line': 'func f()'
        'tokenPos': 2
      }
      {
        'line': 'func (T) f()'
        'tokenPos': 6
      }
      {
        'line': 'func (t T) f()'
        'tokenPos': 6
      }
      {
        'line': 'func (t *T) f()'
        'tokenPos': 8
      }
    ]

    for t in tests
      {tokens} = grammar.tokenizeLine t.line
      expect(tokens[0].value).toEqual 'func'
      expect(tokens[0].scopes).toEqual ['source.koala', 'keyword.function.koala']

      relevantToken = tokens[t.tokenPos]
      expect(relevantToken).toBeDefined()
      expect(relevantToken.value).toEqual 'f'
      expect(relevantToken.scopes).toEqual ['source.koala', 'entity.name.function.koala']

      next = tokens[t.tokenPos + 1]
      expect(next.value).toEqual '('
      expect(next.scopes).toEqual ['source.koala', 'punctuation.other.bracket.round.koala']

  it 'tokenizes operators method declarations', ->
    tests = [
      {
        'line': 'func (t *T) f()'
        'tokenPos': 4
      }
    ]

    for t in tests
      {tokens} = grammar.tokenizeLine t.line
      expect(tokens[0].value).toEqual 'func'
      expect(tokens[0].scopes).toEqual ['source.koala', 'keyword.function.koala']

      relevantToken = tokens[t.tokenPos]
      expect(relevantToken.value).toEqual '*'
      expect(relevantToken.scopes).toEqual ['source.koala', 'keyword.operator.address.koala']

  it 'tokenizes numerics', ->
    numbers =
      'constant.numeric.integer.koala': ['42', '0600', '0xBadFace', '170141183460469231731687303715884105727', '1E6', '0i', '011i', '1E6i']
      'constant.numeric.floating-point.koala': [
        '0.', '72.40', '072.40', '2.71828', '1.e+0', '6.67428e-11', '.25', '.12345E+5',
        '0.i', '2.71828i', '1.e+0i', '6.67428e-11i', '.25i', '.12345E+5i'
      ]

    for scope, nums of numbers
      for num in nums
        {tokens} = grammar.tokenizeLine num
        expect(tokens[0].value).toEqual num
        expect(tokens[0].scopes).toEqual ['source.koala', scope]

    invalidOctals = ['08', '039', '0995']
    for num in invalidOctals
      {tokens} = grammar.tokenizeLine num
      expect(tokens[0].value).toEqual num
      expect(tokens[0].scopes).toEqual ['source.koala', 'invalid.illegal.numeric.koala']

  it 'tokenizes language constants', ->
    constants = ['true', 'false', 'nil', 'iota']
    for constant in constants
      {tokens} = grammar.tokenizeLine constant
      expect(tokens[0].value).toEqual constant
      expect(tokens[0].scopes).toEqual ['source.koala', 'constant.language.koala']

  it 'tokenizes built-in functions', ->
    funcs = [
      'append(x)', 'cap(x)', 'close(x)', 'complex(x)', 'copy(x)', 'delete(x)', 'imag(x)', 'len(x)', 'make(x)', 'new(x)',
      'panic(x)', 'print(x)', 'println(x)', 'real(x)', 'recover(x)'
    ]
    funcVals = ['append', 'cap', 'close', 'complex', 'copy', 'delete', 'imag', 'len', 'make', 'new', 'panic', 'print', 'println', 'real', 'recover']

    for func in funcs
      funcVal = funcVals[funcs.indexOf(func)]
      {tokens} = grammar.tokenizeLine func
      expect(tokens[0].value).toEqual funcVal
      expect(tokens[0].scopes).toEqual ['source.koala', 'support.function.builtin.koala']

  it 'tokenizes operators', ->
    binaryOpers =
      'keyword.operator.arithmetic.koala': ['+', '-', '*', '/', '%']
      'keyword.operator.arithmetic.bitwise.koala': ['&', '|', '^', '&^', '<<', '>>']
      'keyword.operator.assignment.koala': ['=', '+=', '-=', '|=', '^=', '*=', '/=', ':=', '%=', '<<=', '>>=', '&=', '&^=']
      'keyword.operator.channel.koala': ['<-']
      'keyword.operator.comparison.koala': ['==', '!=', '<', '<=', '>', '>=']
      'keyword.operator.decrement.koala': ['--']
      'keyword.operator.ellipsis.koala': ['...']
      'keyword.operator.increment.koala': ['++']
      'keyword.operator.logical.koala': ['&&', '||']

    unaryOpers =
      'keyword.operator.address.koala': ['*var', '&var']
      'keyword.operator.arithmetic.koala': ['+var', '-var']
      'keyword.operator.arithmetic.bitwise.koala': ['^var']
      'keyword.operator.logical.koala': ['!var']

    for scope, ops of binaryOpers
      for op in ops
        {tokens} = grammar.tokenizeLine op
        expect(tokens[0].value).toEqual op
        expect(tokens[0].scopes).toEqual ['source.koala', scope]

    for scope, ops of unaryOpers
      for op in ops
        {tokens} = grammar.tokenizeLine op
        expect(tokens[0].value).toEqual op[0]
        expect(tokens[0].scopes).toEqual ['source.koala', scope]

  it 'tokenizes punctuation brackets', ->
    brackets =
      'punctuation.other.bracket.square.koala': [ '[', ']' ]
      'punctuation.other.bracket.round.koala': [ '(', ')' ]
      'punctuation.other.bracket.curly.koala': [ '{', '}' ]

    for scope, brkts of brackets
      for brkt in brkts
        {tokens} = grammar.tokenizeLine brkt
        expect(tokens[0].value).toEqual brkt
        expect(tokens[0].scopes).toEqual ['source.koala', scope]

  it 'tokenizes punctuation delimiters', ->
    delims =
      'punctuation.other.comma.koala': ','
      'punctuation.other.period.koala': '.'
      'punctuation.other.colon.koala': ':'

    for scope, delim of delims
      {tokens} = grammar.tokenizeLine delim
      expect(tokens[0].value).toEqual delim
      expect(tokens[0].scopes).toEqual ['source.koala', scope]

  it 'tokenizes func names in calls to them', ->
    tests = [
      {
        'line': 'a.b()'
        'name': 'b'
        'tokenPos': 2
        'isFunc': true
      }
      {
        'line': 'pkg.Func1('
        'name': 'Func1'
        'tokenPos': 2
        'isFunc': true
      }
      {
        'line': 'pkg.Func1().Func2('
        'name': 'Func2'
        'tokenPos': 6
        'isFunc': true
      }
      {
        'line': 'pkg.var'
        'name': 'var'
        'tokenPos': 2
        'isFunc': false
      }
      {
        'line': 'doWork(ch)'
        'name': 'doWork'
        'tokenPos': 0
        'isFunc': true
      }
      {
        'line': 'f1()'
        'name': 'f1'
        'tokenPos': 0
        'isFunc': true
      }
    ]

    want = ['source.koala', 'support.function.koala']

    for t in tests
      {tokens} = grammar.tokenizeLine t.line

      relevantToken = tokens[t.tokenPos]
      if t.isFunc
        expect(relevantToken).not.toBeNull()
        expect(relevantToken.value).toEqual t.name
        expect(relevantToken.scopes).toEqual want

        next = tokens[t.tokenPos + 1]
        expect(next.value).toEqual '('
        expect(next.scopes).toEqual ['source.koala', 'punctuation.other.bracket.round.koala']
      else
        expect(relevantToken.scopes).not.toEqual want

  it 'tokenizes package names', ->
    tests = ['package main', 'package mypackage']

    for test in tests
      {tokens} = grammar.tokenizeLine test
      expect(tokens[0].scopes).toEqual ['source.koala', 'keyword.package.koala']
      expect(tokens[2].scopes).toEqual ['source.koala', 'entity.name.package.koala']

  it 'tokenizes invalid package names as such', ->
    {tokens} = grammar.tokenizeLine 'package 0mypackage'
    expect(tokens[0]).toEqual value: 'package', scopes: ['source.koala', 'keyword.package.koala']
    expect(tokens[2]).toEqual value: '0mypackage', scopes: ['source.koala', 'invalid.illegal.identifier.koala']

  it 'does not treat words that have a trailing package as a package name', ->
    {tokens} = grammar.tokenizeLine 'func myFunc(Varpackage string)'
    expect(tokens[4]).toEqual value: 'Varpackage ', scopes: ['source.koala']
    expect(tokens[5]).toEqual value: 'string', scopes: ['source.koala', 'storage.type.string.koala']

  it 'tokenizes type names', ->
    tests = ['type mystring string', 'type mytype interface{']

    for test in tests
      {tokens} = grammar.tokenizeLine test
      expect(tokens[0].scopes).toEqual ['source.koala', 'keyword.type.koala']
      expect(tokens[2].scopes).toEqual ['source.koala', 'entity.name.type.koala']

  it 'tokenizes invalid type names as such', ->
    {tokens} = grammar.tokenizeLine 'type 0mystring string'
    expect(tokens[0]).toEqual value: 'type', scopes: ['source.koala', 'keyword.type.koala']
    expect(tokens[2]).toEqual value: '0mystring', scopes: ['source.koala', 'invalid.illegal.identifier.koala']

  it 'does not treat words that have a trailing type as a type name', ->
    {tokens} = grammar.tokenizeLine 'func myFunc(Vartype string)'
    expect(tokens[4]).toEqual value: 'Vartype ', scopes: ['source.koala']
    expect(tokens[5]).toEqual value: 'string', scopes: ['source.koala', 'storage.type.string.koala']

  describe 'in variable declarations', ->
    testVar = (token) ->
      expect(token.value).toBe 'var'
      expect(token.scopes).toEqual ['source.koala', 'keyword.var.koala']

    testVarAssignment = (token, name) ->
      expect(token.value).toBe name
      expect(token.scopes).toEqual ['source.koala', 'variable.other.assignment.koala']

    testVarDeclaration = (token, name) ->
      expect(token.value).toBe name
      expect(token.scopes).toEqual ['source.koala', 'variable.other.declaration.koala']

    testOp = (token, op) ->
      expect(token.value).toBe op
      expect(token.scopes).toEqual ['source.koala', 'keyword.operator.koala']

    testOpAddress = (token, op) ->
      expect(token.value).toBe op
      expect(token.scopes).toEqual ['source.koala', 'keyword.operator.address.koala']

    testOpAssignment = (token, op) ->
      expect(token.value).toBe op
      expect(token.scopes).toEqual ['source.koala', 'keyword.operator.assignment.koala']

    testOpBracket = (token, op) ->
      expect(token.value).toBe op
      expect(token.scopes).toEqual ['source.koala', 'punctuation.other.bracket.round.koala']

    testOpPunctuation = (token, op) ->
      expect(token.value).toBe op
      expect(token.scopes).toEqual ['source.koala', 'punctuation.other.comma.koala']

    testOpTermination = (token, op) ->
      expect(token.value).toBe op
      expect(token.scopes).toEqual ['source.koala', 'punctuation.terminator.koala']

    testNumType = (token, name) ->
      expect(token.value).toBe name
      expect(token.scopes).toEqual ['source.koala', 'storage.type.numeric.koala']

    testStringType = (token, name) ->
      expect(token.value).toBe name
      expect(token.scopes).toEqual ['source.koala', 'storage.type.string.koala']

    testNum = (token, value) ->
      expect(token.value).toBe value
      expect(token.scopes).toEqual ['source.koala', 'constant.numeric.integer.koala']

    testString = (token, value) ->
      expect(token.value).toBe value
      expect(token.scopes).toEqual ['source.koala', 'string.quoted.double.koala']

    describe 'in var statements', ->
      it 'tokenizes a single variable assignment', ->
        {tokens} = grammar.tokenizeLine 'i = 7'
        testVarAssignment tokens[0], 'i'
        testOpAssignment tokens[2], '='
        testNum tokens[4], '7'

      it 'tokenizes a multiple variable assignments', ->
        {tokens} = grammar.tokenizeLine 'i, j = 7, 8'
        testVarAssignment tokens[0], 'i'
        testOpPunctuation tokens[1], ','
        testVarAssignment tokens[3], 'j'
        testOpAssignment tokens[5], '='
        testNum tokens[7], '7'
        testNum tokens[10], '8'

      it 'tokenizes a single name and a type', ->
        {tokens} = grammar.tokenizeLine 'var i int'
        testVar tokens[0]
        testVarDeclaration tokens[2], 'i'
        testNumType tokens[4], 'int'

      it 'tokenizes a single name and a type', ->
        {tokens} = grammar.tokenizeLine 'var s []string'
        testVar tokens[0]
        testVarDeclaration tokens[2], 's'
        testStringType tokens[6], 'string'

      it 'tokenizes a single name and a type with length', ->
        {tokens} = grammar.tokenizeLine 'var s [4]string'
        testVar tokens[0]
        testVarDeclaration tokens[2], 's'
        expect(tokens[4]).toEqual value: '[', scopes: ['source.koala', 'punctuation.other.bracket.square.koala']
        expect(tokens[5]).toEqual value: '4', scopes: ['source.koala', 'constant.numeric.integer.koala']
        expect(tokens[6]).toEqual value: ']', scopes: ['source.koala', 'punctuation.other.bracket.square.koala']
        testStringType tokens[7], 'string'

      it 'tokenizes a single name and multi-dimensional types with an address', ->
        {tokens} = grammar.tokenizeLine 'var e [][]*string'
        testVar tokens[0]
        testVarDeclaration tokens[2], 'e'
        expect(tokens[4]).toEqual value: '[', scopes: ['source.koala', 'punctuation.other.bracket.square.koala']
        expect(tokens[5]).toEqual value: ']', scopes: ['source.koala', 'punctuation.other.bracket.square.koala']
        expect(tokens[6]).toEqual value: '[', scopes: ['source.koala', 'punctuation.other.bracket.square.koala']
        expect(tokens[7]).toEqual value: ']', scopes: ['source.koala', 'punctuation.other.bracket.square.koala']
        testOpAddress tokens[8], '*'
        testStringType tokens[9], 'string'

      it 'tokenizes a single name and its initialization', ->
        {tokens} = grammar.tokenizeLine ' var k =  0'
        testVar tokens[1]
        testVarAssignment tokens[3], 'k'
        testOpAssignment tokens[5], '='
        testNum tokens[7], '0'

      it 'tokenizes a single name, a type, and an initialization', ->
        {tokens} = grammar.tokenizeLine 'var z blub = 7'
        testVar tokens[0]
        testVarAssignment tokens[2], 'z'
        expect(tokens[3]).toEqual value: ' blub ', scopes: ['source.koala']
        testOpAssignment tokens[4], '='
        testNum tokens[6], '7'

      it 'does not tokenize more than necessary', ->
        # This test is worded vaguely because it's hard to describe.
        # Basically, make sure that the variable match isn't tokenizing the entire line
        # in a (=.+) style match. This prevents multiline stuff after the assignment
        # from working correctly, because match can only tokenize single lines.
        lines = grammar.tokenizeLines '''
          var multiline string = `wow!
          this should work!`
        '''
        testVar lines[0][0]
        testVarAssignment lines[0][2], 'multiline'
        testStringType lines[0][4], 'string'
        testOpAssignment lines[0][6], '='
        expect(lines[0][8]).toEqual value: '`', scopes: ['source.koala', 'string.quoted.raw.koala', 'punctuation.definition.string.begin.koala']
        expect(lines[1][1]).toEqual value: '`', scopes: ['source.koala', 'string.quoted.raw.koala', 'punctuation.definition.string.end.koala']

      it 'tokenizes multiple names and a type', ->
        {tokens} = grammar.tokenizeLine 'var U, V,  W  float64'
        testVar tokens[0]
        testVarDeclaration tokens[2], 'U'
        testOpPunctuation tokens[3], ','
        testVarDeclaration tokens[5], 'V'
        testOpPunctuation tokens[6], ','
        testVarDeclaration tokens[8], 'W'

      it 'tokenizes multiple names and initialization expressions', ->
        {tokens} = grammar.tokenizeLine 'var x, y, z = 1, 2, 3'
        testVar tokens[0]
        testVarAssignment tokens[2], 'x'
        testOpPunctuation tokens[3], ','
        testVarAssignment tokens[5], 'y'
        testOpPunctuation tokens[6], ','
        testVarAssignment tokens[8], 'z'
        testOpAssignment tokens[10], '='
        testNum tokens[12], '1'
        testOpPunctuation tokens[13], ','
        testNum tokens[15], '2'
        testOpPunctuation tokens[16], ','
        testNum tokens[18], '3'

      it 'tokenizes multiple names, a type, and initialization expressions', ->
        {tokens} = grammar.tokenizeLine 'var x, y float32 = float, thirtytwo'
        testVar tokens[0]
        testVarAssignment tokens[2], 'x'
        testOpPunctuation tokens[3], ','
        testVarAssignment tokens[5], 'y'
        testNumType tokens[7], 'float32'
        testOpAssignment tokens[9], '='
        testOpPunctuation tokens[11], ','

      it 'tokenizes multiple names and a function call', ->
        {tokens} = grammar.tokenizeLine 'var re, im = complexSqrt(-1)'
        testVar tokens[0]
        testVarAssignment tokens[2], 're'
        testVarAssignment tokens[5], 'im'
        testOpAssignment tokens[7], '='

      it 'tokenizes with a placeholder', ->
        {tokens} = grammar.tokenizeLine 'var _, found = entries[name]'
        testVar tokens[0]
        testVarAssignment tokens[2], '_'
        testVarAssignment tokens[5], 'found'
        testOpAssignment tokens[7], '='

      it 'does not treat words that have a trailing var as a variable declaration', ->
        {tokens} = grammar.tokenizeLine 'func test(envvar string)'
        expect(tokens[4]).toEqual value: 'envvar ', scopes: ['source.koala']
        expect(tokens[5]).toEqual value: 'string', scopes: ['source.koala', 'storage.type.string.koala']

      describe 'in var statement blocks', ->
        it 'tokenizes single names with a type', ->
          [kwd, decl, closing] = grammar.tokenizeLines '\tvar (\n\t\tfoo *bar\n\t)'
          testVar kwd[1]
          testOpBracket kwd[3], '('
          testVarDeclaration decl[1], 'foo'
          testOpAddress decl[3], '*'
          testOpBracket closing[1], ')'

        it 'tokenizes single names with an initializer', ->
          [kwd, decl, closing] = grammar.tokenizeLines 'var (\n\tfoo = 42\n)'
          testVar kwd[0], 'var'
          testOpBracket kwd[2], '('
          testVarAssignment decl[1], 'foo'
          testOpAssignment decl[3], '='
          testNum decl[5], '42'
          testOpBracket closing[0], ')'

        it 'tokenizes multiple names', ->
          [kwd, decl, closing] = grammar.tokenizeLines 'var (\n\tfoo, bar = baz, quux\n)'
          testVar kwd[0]
          testOpBracket kwd[2], '('
          testVarAssignment decl[1], 'foo'
          testOpPunctuation decl[2], ','
          testVarAssignment decl[4], 'bar'
          testOpAssignment decl[6], '='
          testOpPunctuation decl[8], ','
          testOpBracket closing[0], ')'

        it 'tokenizes non variable declarations (e.g. comments)', ->
          [kwd, comment, decl, closing] = grammar.tokenizeLines 'var (\n\t// I am a comment\n\tfoo *bar\n)'
          testVar kwd[0]
          testOpBracket kwd[2], '('
          expect(comment[1].value).toEqual '//'
          expect(comment[1].scopes).toEqual ['source.koala', 'comment.line.double-slash.koala', 'punctuation.definition.comment.koala']
          expect(comment[2].value).toEqual ' I am a comment'
          expect(comment[2].scopes).toEqual ['source.koala', 'comment.line.double-slash.koala']
          testVarDeclaration decl[1], 'foo'
          testOpAddress decl[3], '*'
          testOpBracket closing[0], ')'

        it 'tokenizes all parts of variable initializations correctly', ->
          [kwd, decl, init, _, closing] = grammar.tokenizeLines 'var (\n\tm = map[string]int{\n\t\t"key": 10,\n\t}\n)'
          testVar kwd[0]
          testOpBracket kwd[2], '('
          testVarAssignment decl[1], 'm'
          testOpAssignment decl[3], '='
          testString init[2], 'key'
          testNum init[6], '10'
          testOpBracket closing[0], ')'

        it 'tokenizes non-ASCII variable names', ->
          {tokens} = grammar.tokenizeLine 'über = test'
          testVarAssignment tokens[0], 'über'
          testOpAssignment tokens[2], '='

        it 'tokenizes invalid variable names as such', ->
          {tokens} = grammar.tokenizeLine 'var 0test = 0'
          testVar tokens[0]
          expect(tokens[2]).toEqual value: '0test', scopes: ['source.koala', 'invalid.illegal.identifier.koala']

      describe 'in shorthand variable declarations', ->
        it 'tokenizes single names', ->
          {tokens} = grammar.tokenizeLine 'f := func() int { return 7 }'
          testVarAssignment tokens[0], 'f'
          testOpAssignment tokens[2], ':='

          {tokens} = grammar.tokenizeLine 'ch := make(chan int)'
          testVarAssignment tokens[0], 'ch'
          testOpAssignment tokens[2], ':='

        it 'tokenizes multiple names', ->
          {tokens} = grammar.tokenizeLine 'i, j := 0, 10'
          testVarAssignment tokens[0], 'i'
          testOpPunctuation tokens[1], ','
          testVarAssignment tokens[3], 'j'

          {tokens} = grammar.tokenizeLine 'if _, y, z := coord(p); z > 0'
          testVarAssignment tokens[2], '_'
          testVarAssignment tokens[5], 'y'
          testVarAssignment tokens[8], 'z'
          testOpAssignment tokens[10], ':='
          testOpTermination tokens[16], ';'

  describe 'in imports declarations', ->
    testImport = (token) ->
      expect(token.value).toBe 'import'
      expect(token.scopes).toEqual ['source.koala', 'keyword.import.koala']

    testImportAlias = (token, name) ->
      expect(token.value).toBe name
      expect(token.scopes).toEqual ['source.koala', 'entity.alias.import.koala']

    testImportPackage = (token, name) ->
      expect(token.value).toBe name
      expect(token.scopes).toEqual ['source.koala', 'string.quoted.double.koala', 'entity.name.import.koala']

    testOpBracket = (token, op) ->
      expect(token.value).toBe op
      expect(token.scopes).toEqual ['source.koala', 'punctuation.other.bracket.round.koala']

    testBeginQuoted = (token) ->
      expect(token.value).toBe '"'
      expect(token.scopes).toEqual ['source.koala', 'string.quoted.double.koala', 'punctuation.definition.string.begin.koala']

    testEndQuoted = (token) ->
      expect(token.value).toBe '"'
      expect(token.scopes).toEqual ['source.koala', 'string.quoted.double.koala', 'punctuation.definition.string.end.koala']

    describe 'when it is a single line declaration', ->
      it 'tokenizes declarations with a package name', ->
        {tokens} = grammar.tokenizeLine 'import "fmt"'
        testImport tokens[0]
        testBeginQuoted tokens[2]
        testImportPackage tokens[3], 'fmt'
        testEndQuoted tokens[4]

      it 'tokenizes declarations with a package name and an alias', ->
        {tokens} = grammar.tokenizeLine 'import . "fmt"'
        testImport tokens[0]
        testImportAlias tokens[2], '.'
        testBeginQuoted tokens[4]
        testImportPackage tokens[5], 'fmt'
        testEndQuoted tokens[6]
        {tokens} = grammar.tokenizeLine 'import otherpackage "github.com/test/package"'
        testImport tokens[0]
        testImportAlias tokens[2], 'otherpackage'
        testBeginQuoted tokens[4]
        testImportPackage tokens[5], 'github.com/test/package'
        testEndQuoted tokens[6]

      it 'does not treat words that have a trailing import as a import declaration', ->
        {tokens} = grammar.tokenizeLine 'func myFunc(Varimport string)'
        expect(tokens[4]).toEqual value: 'Varimport ', scopes: ['source.koala']
        expect(tokens[5]).toEqual value: 'string', scopes: ['source.koala', 'storage.type.string.koala']

    describe 'when it is a multi line declaration', ->
      it 'tokenizes single declarations with a package name', ->
        [kwd, decl, closing] = grammar.tokenizeLines 'import (\n\t"github.com/test/package"\n)'
        testImport kwd[0]
        testOpBracket kwd[2], '('
        testBeginQuoted decl[1]
        testImportPackage decl[2], 'github.com/test/package'
        testEndQuoted decl[3]
        testOpBracket closing[0], ')'

      it 'tokenizes multiple declarations with a package name', ->
        [kwd, decl, decl2, closing] = grammar.tokenizeLines 'import (\n\t"github.com/test/package"\n\t"fmt"\n)'
        testImport kwd[0]
        testOpBracket kwd[2], '('
        testBeginQuoted decl[1]
        testImportPackage decl[2], 'github.com/test/package'
        testEndQuoted decl[3]
        testBeginQuoted decl2[1]
        testImportPackage decl2[2], 'fmt'
        testEndQuoted decl2[3]
        testOpBracket closing[0], ')'

      it 'tokenizes single imports with an alias for a multi-line declaration', ->
        [kwd, decl, closing] = grammar.tokenizeLines 'import (\n\t. "github.com/test/package"\n)'
        testImport kwd[0]
        testOpBracket kwd[2], '('
        testImportAlias decl[1], '.'
        testBeginQuoted decl[3]
        testImportPackage decl[4], 'github.com/test/package'
        testEndQuoted decl[5]
        testOpBracket closing[0], ')'

      it 'tokenizes multiple imports with an alias for a multi-line declaration', ->
        [kwd, decl, decl2, closing] = grammar.tokenizeLines 'import (\n\t. "github.com/test/package"\n\t"fmt"\n)'
        testImport kwd[0]
        testOpBracket kwd[2], '('
        testImportAlias decl[1], '.'
        testBeginQuoted decl[3]
        testImportPackage decl[4], 'github.com/test/package'
        testEndQuoted decl[5]
        testBeginQuoted decl2[1]
        testImportPackage decl2[2], 'fmt'
        testEndQuoted decl2[3]
        testOpBracket closing[0], ')'
