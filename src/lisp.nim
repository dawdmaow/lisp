import std/[sequtils, strutils, parseutils, tables, sugar]

type
  Keyword = enum
    kwQuote = "quote"
    kwVar = "var"
    kwDo = "do"
    kwIf = "if"
    kwPlus = "+"
    ## Builtin procedures.

  TokenKind = enum
    tkSymbol, tkNumber, tkString, tkOpen, tkClose, tkKeyword, tkBool
  Token = ref object
    line: int
    case kind: TokenKind
    of tkOpen, tkClose:
      discard
    of tkSymbol:
      sym: string
    of tkNumber:
      num: float
    of tkString:
      str: string
    of tkKeyword:
      keyword: Keyword
    of tkBool:
      boolVal: bool

  NodeKind = enum
    nkSymbol, nkNumber, nkString, nkList, nkFunction, nkKeyword, nkBool
  Node = ref object
    line: int
    case kind: NodeKind
    of nkKeyword:
      keyword: Keyword
    of nkSymbol:
      sym: string
    of nkNumber:
      num: float
    of nkString:
      str: string
    of nkList:
      kids: seq[Node]
    of nkBool:
      boolVal: bool
    of nkFunction:
      funcParams: seq[string]
      funcBody: Node

  Stack = ref object
    symbols: Table[string, Node]

  State = ref object
    stacks: seq[Stack]

const
  symbolCharacters = Letters + Digits + {'_'} + {'?', '.', '!', '+', '-', '*', '/', '<', '>', '=', '&', '|', '%', '^',
      '~', ':'}

iterator tokenize(s: string): Token =
  var i = 0
  var line = 1
  var tmp: string
  while i <= s.high:
    tmp.setLen 0
    case s[i]
    of Newlines:
      inc i
    of Whitespace - Newlines:
      inc i
      inc line
    of '(':
      inc i
      yield Token(kind: tkOpen, line: line)
    of ')':
      inc i
      yield Token(kind: tkClose, line: line)
    of '"':
      inc i
      i += s.parseUntil(tmp, '"', i)
      inc i
      yield Token(kind: tkString, str: tmp, line: line)
    of Digits:
      i += s.parseWhile(tmp, Digits + {'.'}, i)
      yield Token(kind: tkNumber, num: tmp.parseFloat, line: line)
    else:
      i += s.parseWhile(tmp, symbolCharacters, i)
      const keywords = collect(for it in Keyword: $it)
      if tmp in keywords:
        yield Token(kind: tkKeyword, keyword: parseEnum[Keyword](tmp), line: line)
      else:
        yield Token(kind: tkSymbol, sym: tmp, line: line)

template assert(t: Token or Node, cond: bool, s: string) =
  if not cond:
    echo "Error on line ", t.line, ": ", s
    quit QuitFailure

template assert(t: Token or Node, cond: bool) =
  assert(t, cond, "Assertion failed")

proc toNodes(tokens: seq[Token]): Node =
  result = Node(kind: nkList, line: 1, kids: @[Node(kind: nkKeyword, keyword: kwDo, line: 1)])
  var stack = @[result]
  for t in tokens:
    let current = stack[^1]

    case t.kind

    of tkOpen:
      var newNode = Node(kind: nkList, line: t.line)
      current.kids.add newNode # Add this new scope as a child of the current scope.
      stack.add newNode # This new scope is now the current scope.

    of tkClose:
      t.assert stack.len > 0, "Unmatched closing parenthesis"
      stack.delete stack.high # Discard the current scope.

    of tkSymbol:
      current.kids.add Node(kind: nkSymbol, sym: t.sym, line: t.line)

    of tkNumber:
      current.kids.add Node(kind: nkNumber, num: t.num, line: t.line)

    of tkString:
      current.kids.add Node(kind: nkString, str: t.str, line: t.line)

    of tkKeyword:
      current.kids.add Node(kind: nkKeyword, keyword: t.keyword, line: t.line)

    of tkBool:
      current.kids.add Node(kind: nkBool, boolVal: t.boolVal, line: t.line)

proc evaluate(s: State, n: Node): Node =

  proc evaluateKeywordCall(s: State, n: Node): Node {.nimcall.} =
    n.assert n.kind == nkList
    n.assert n.kids.len > 0
    n.assert n.kids[0].kind == nkKeyword
    let args = n.kids[1..^1]

    case n.kids[0].keyword

    of kwQuote:
      n.assert args.len == 1, "Quote takes exactly one argument"
      result = args[0]

    of kwVar:
      n.assert args.len == 2, "Var takes exactly two arguments"
      let sym = args[0]
      let value = args[1]
      n.assert sym.kind == nkSymbol, "First argument to var must be a symbol"
      n.assert not s.stacks[^1].symbols.hasKey(sym.sym), "Symbol already defined"
      s.stacks[^1].symbols[sym.sym] = value
      result = value

    of kwDo:
      for i in 0..<args.high:
        discard s.evaluate(args[i])
      result = s.evaluate(args[^1])

    of kwIf:
      n.assert args.len in [2, 3], "If takes two or three arguments"
      let condition = s.evaluate(args[0])
      n.assert condition.kind == nkBool, "First argument to if must be a boolean"
      if condition.boolVal:
        result = s.evaluate(args[1])
      else:
        if args.len >= 3:
          result = s.evaluate(args[2])

    of kwPlus:
      result = Node(kind: nkNumber, num: 0, line: n.line)
      for a in args:
        let a = s.evaluate(a)
        n.assert a.kind == nkNumber, "Argument to + must be a number"
        result.num += a.num

  case n.kind

  of nkNumber, nkString, nkKeyword, nkBool, nkFunction:
    result = n
    ## These don't need evaluation.

  of nkList:
    if n.kids.len == 0:
      result = n
    else:
      for k in n.kids.mitems:
        k = s.evaluate(k)
      let f = n.kids[0]
      let args = if n.kids.len > 1: n.kids[1..^1] else: @[]
      case f.kind
      of nkString, nkNumber, nkSymbol, nkList, nkBool:
        n.assert false, "First element of list was not callable"
      of nkKeyword:
        result = s.evaluateKeywordCall(n)
      of nkFunction:
        let newStack = Stack()
        n.assert args.len == f.funcParams.len, "Wrong number of arguments"
        for i, param in f.funcParams:
          newStack.symbols[param] = n.kids[i+1]
        s.stacks.add newStack
        result = f.funcBody
        s.stacks.delete s.stacks.high
        ## Non-empty list is interpreted as a function call.

  of nkSymbol:
    var found: Node
    for i in countdown(s.stacks.high, 0):
      let s = s.stacks[i]
      if s.symbols.hasKey(n.sym):
        found = s.symbols[n.sym]
        break
    n.assert found != nil, "Symbol not found: " & n.sym
    n.assert found.kind notin {nkSymbol}, "Symbol cannot point to another symbol"
    result = found
    ## Symbol's assigned value is looked up in the stack frames.

proc newState: State =
  result = State(stacks: @[Stack()])

proc `$`(n: Node): string =

  proc process(n: Node, result: var string) {.nimcall.} =
    case n.kind
    of nkSymbol:
      result.add n.sym
    of nkNumber:
      result.add $n.num
    of nkString:
      result.add '"' & n.str & '"'
    of nkKeyword:
      result.add $n.keyword
    of nkBool:
      result.add $n.boolVal
    of nkList:
      result.add "("
      for i, k in n.kids:
        if i > 0:
          result.add " "
        process k, result
      result.add ")"
    of nkFunction:
      result.add "<<<FUNCTION>>>"

  process n, result

let code = "(+ 1 2)".tokenize.toSeq.toNodes
echo code
echo newState().evaluate(code)
