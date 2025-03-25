when not defined(js): {.fatal: "This can only be run on the JS backend.".}

import std/[sequtils, strutils, parseutils, tables, sugar, enumerate, hashes, algorithm, dom]
import dawid/dawid

# TODO: generic functions
# TODO: generic types
# TODO: custom binary operators (just add a flag to the function nodes and check it when evaluating a function call)
# TODO: interfaces
# TODO: multimethods
# TODO: iterators
# TODO: list comprehension
# TODO: debug functino for printing with line, column, script name etc.
# TODO: consider having an itermediary representation (so we don't have to special-case syntax sugar like `@` for members)
# TODO: use square brackets for lists
# TODO: use ace extensions from API reference (such as error highlighting, beutify, autocomplete, code lens, etc)
# TODO: replace code editor with Monaco library
#[
Code editor alternatives:
Monaco	Used in VS Code, rich API, fast	Best for VS Code-like feel
CodeMirror	Lightweight, customizable	Good for embedding
Prism.js	Syntax highlighting only	Not a full editor
Highlight.js	Simple syntax highlighting	Read-only, no editing
Eclipse Theia	Extensible, VS Code-like	Heavier, cloud IDE-like
Orion	Eclipse-based, web-native	Less active development
JS-Editor	Minimal, browser-based	Simple and lightweight
]#
#
# TODO: more pretty error messages in the terminal
# TODO: hide nim generated stacktrace from the UI terminal (and from non-release builds in general)
# TODO: fix terminal/editor size (make it fit the screen size)
# TODO: UI should be more pretty
# TODO: use canvas for interactive 2D
# TODO: use webgl for 3D
# TODO: more sensible builtin names
# TODO: non-lisp syntax alternative
# TODO: make builtins less minimalistic and more readable:
# - ex: `\ () void (echo "lol")` should be `proc () -> void = echo "lol"`
# TODO: indexing like foo[i]
# TODO: make some operators binary: `(a = b)` instead of `(= a b)`

const
  browser = defined(js) and not defined(nodejs)

when browser:
  {.emit: """
    const term = new Terminal({cursorBlink: true, rows: 48, cols: 8, fontSize: 12});
    const fitAddon = new FitAddon.FitAddon();
    term.loadAddon(fitAddon);

    let editor;

    window.addEventListener("resize", () => {
      fitAddon.fit();
      if (editor) {
        editor.resize();
      }
    });
  """.}

proc clearTerminal =
  when browser:
    {.emit: """
      console.clear();
      term.clear();
    """.}

proc print(s: string) =
  echo s
  when browser:
    let cs = s.multiReplace({"\r": "", "\n": "\r\n"}).cstring
    {.emit: """term.write(`cs`);""".}
    {.emit: """term.write("\r\n");""".}

  # TODO: tests should be on node.js backed.
  # TODO: "when js" everywhere is stupid, either make it a wasm or a js program.
  # TODO: redirect stdout to terminal so every proc that writes to stdout will write to terminal instead.
  # TODO: remove emscripten completely
  # TODO: benchmark time differences between js and C

  # TODO: (quote (a b)) will end up trying to evaluate symbols, which is why we currently have to use strings for everything that shouldn't be "inserted" into the quoted list.
  # TODO: Force explicit discarding of values.
  # TODO: if-branches (with an explicit else) instead of if (less confusing visually )
  # TODO: Right now almost everything is passed by reference. This must change.
  # TODO: check return types of functions.
  # TODO: math functions (+, etc) should be binary operators *ONLY*.
  # TODO: range -> countup
  # TODO: dipatch by argument cound & type dynamically. this should be easy!
  # TODO: magic can just just functions assigned to global scope. this will make adding your own + operations for vectors (overloadable by arg type) easy, as from the POV of interpreter there will be no difference between real and hardcoded functions.
  # TODO: classes with inheritance and accessors.
  # TODO: SkookumScript like async routines.
  # TODO: try to typecheck in advance (before evaluation)
  # TODO: union types
  # TODO: need keywords (pseudosymbols that always evaluate to themselves)
  # TODO: Write lisp errors into the terminal explicitly

type
  Magic = enum
    mQuote = "quote"
    mEval = "eval"
    mVar = ":="
    # mDo = "do"
    mIf = "if"
    mPlus = "+"
    mLambda = "\\"
    mEcho = "echo"
    mRepr = "repr"
    mBlock = "block"
    mTable = "table"
    mTest = "must-equal"
    mType = "object-type"
    mAssign = "="
    mStrAdd = "str-add"
    mHigh = "high"
    mLow = "low"
    mRange = ".."
    mExpect = "expect"
    mToStr = "to-str"
    mIsList = "is-list"
    mEqual = "=="
    mNotEqual = "!="
    mNot = "not"
    mEmpty = "empty?"
    mArrayGet = "array-get"
    mFilterArray = "filter-array"
    mMinus = "-"
    mFor = "for"
    mLen = "len"
    mEqualPlus = "+="
    mEqualMinus = "-="
    mIsStr = "is-str"
    mClass = "class"
    mReturn = "return"
    mOr = "or"
    mAnd = "and"
    mContains = "contains"
    mTemplate = "template"
    mWhile = "while"
    # mMacro = "macro"
    mLesserOrEq = "<="
    mGreaterOrEq = ">="
    mLesser = "<"
    mGreater = ">"
    mAdd = "add"
    mEnum = "enum-type"
    mTry = "try"
    mExcept = "except"
    mFinally = "finally"
    mTableType = "table-type"
    mArrayType = "array-type"
    mCase = "case"
    mTableSet = "table-set!"
    mTableGet = "table-get"
    mTableHasKey = "table-has-key"
    mContinue = "continue"
    mBreak = "break"
    mJoinLists = "join-lists"
    # mEchoNoNewline = "echo-raw"

  TagKind = enum
    tkUnquote = "unquote"
    tkUnquoteSplicing = "unquote-splicing"

  Return = ref object of CatchableError
    value: Node

  Break = ref object of CatchableError
  Continue = ref object of CatchableError

  TokenKind = enum
    tkSymbol, tkNumber = "number-type", tkString = "string-type", tkOpen, tkClose, tkKeyword, tkBool, tkAtomicType,
      tkDot, tkTag, tkAnyType = "any-type", tkVoidType = "void-type", tkMember
  Token = ref object
    line: int # TODO: Add filename and column.
    case kind: TokenKind
    of tkDot: dot: string
    of tkOpen, tkClose, tkAnyType, tkVoidType: discard
    of tkSymbol: sym: string
    of tkNumber: num: float
    of tkString: str: string
    of tkKeyword: keyword: Magic
    of tkBool: boolVal: bool
    of tkAtomicType: atomicType: AtomicType
    of tkTag: tag: TagKind
    of tkMember: member: string

  AtomicType = enum
    atNumber = "number"
    atString = "string"
    atBool = "bool"

  NodeKind = enum
    nkSymbol, nkList, nkFunction, nkKeyword, nkTable, nkAtomicType, nkObjectType, nkEnumType,
      nkObject, nkAtomic, nkDot, nkTemplate, nkTableType, nkArrayType, nkAnyType, nkVoidType, nkVoidValue, nkClassType,
        nkClassObj, nkMember

  Node = ref object
    line: int
    tags: set[TagKind]
    enumType: Node      # For node values typed as enums. (ie. runtime inclusion checked)

    case kind: NodeKind

    of nkMember:
      member: string

    of nkClassType:
      className: string
      classParent: Node # niblable
      classFields: Table[string, Node]
      classMethods: Table[string, Node] # stores function nodes, not method nodes, because the class know which object the method will belong to anyway. it's just a prototype for a method node to instantiate. TODO: Consider changing the name to something else.

    of nkClassObj:
      classObjType: Node
      classObjFields: Table[string, Node]
      classObjMethods: Table[string, Node] # TODO: strings seem a little excessive for memory usage, we should be able to get away with integers/hashes.

    of nkAnyType, nkVoidType, nkVoidValue:
      discard

    of nkDot:
      dot: string

    of nkKeyword:
      keyword: Magic

    of nkAtomic:
      case atomicKind: AtomicType
      of atNumber: num: float
      of atString: str: string
      of atBool: boolean: bool

    of nkSymbol:
      sym: string

    of nkAtomicType:
      atomicType: AtomicType

    of nkList:
      kids: seq[Node]
      listType: Node    # Required

    of nkTable:
      tableVal: Table[Node, Node]
      tableType: Node   # Required

    of nkFunction:
      funcParams: seq[string]
      funcTypes: seq[Node]
      funcBody, funcReturnType: Node
      funcObj: Node     # For methods only. TODO: Don't require this on every function node.

    of nkTemplate:
      templateParams: seq[string]
      templateBody: Node

    of nkObjectType:
      objectTypeFields: Table[string, Node]
    of nkObject:
      objectType: Node
      objectFields: Table[string, Node]
    of nkEnumType:
      enumTypeValues: seq[Node]
    of nkTableType:
      tableKeyType, tableValueType: Node
    of nkArrayType:
      arrayValueType: Node

  StackKind = enum skBlock, skFunction
  # TODO: It should not be possible to break from template/macro stacks.
  # For this reason, we need a separate kind for each.

  Stack = ref object
    kind: StackKind
    symbols: Table[string, Node]
    currentClassObj: Node

  State = ref object
    stacks: seq[Stack]

template assert(t: Token or Node, cond: bool, s: string) {.dirty.} =
  if not cond:
    let inst = instantiationInfo()
    raiseAssert "\n\nError in lisp script, line " & $inst.line & " (script line " & $t.line & "): " & s

template assert(t: Token or Node, cond: bool) {.dirty.} =
  assert(t, cond, "Assertion failed")

proc isKind(n: Node, at: AtomicType): bool =
  result = false
  if n.kind == nkAtomic:
    result = n.atomicKind == atString
  # echo ("!!!", n.kind, result)

proc isInt(f: float): bool =
  f.int.float == f

proc toIndex(f: float): int =
  assert f.isInt:
    "Number can't be converted to an index"
  result = f.int

proc hash(n: Node): Hash {.noSideEffect.} =
  # TODO: monstrosity
  assert not n.isNil, "Attempting to hash a nil node"
  n.assert n.kind != nkSymbol, "Cannot hash a symbol"
  for name, f in n[].fieldPairs:
    when name != "line" and name != "enumType":
      # when name == "enumType":
      #   if f != nil:
      #     result = result !& f.hash
      # else:
      when f is ref:
        if f != nil:
          result = result !& f.hash
      else:
        result = result !& f.hash
  result = !$result

const
  mustBeEvaluatedBeforeComparison = {nkSymbol, nkDot, nkMember}

#[
TODO: Rename from `==` to `equalValue` or `=?` and replace in code
TODO: Don't use hash for comparison (performance reasons)
  (HOWEVER `==` and `hash` must be 100% consistent!)
]#
proc `==`(a, b: Node): bool {.noSideEffect.} =
  # TODO: Simplify with metaprogramming.

  if a.isNil and b.isNil:
    result = true

  elif a.isNil:
    result = b.isNil

  elif b.isNil:
    result = a.isNil

  elif a.kind != b.kind:
    result = false

  else:
    case a.kind

    of mustBeEvaluatedBeforeComparison:
      a.assert false, "Node must be evaluated before comparison: " & a.repr
      # discard

    of nkKeyword:
      result = a.keyword == b.keyword

    of nkList:
      result = a.kids == b.kids

    of nkFunction:
      result = a == b

    of nkTable:
      result = a.tableVal == b.tableVal

    of nkAtomicType:
      result = a.atomicType == b.atomicType

    of nkAtomic:
      result = a.atomicKind == b.atomicKind
      case a.atomicKind
      of atNumber: result = a.num == b.num
      of atString: result = a.str == b.str
      of atBool: result = a.boolean == b.boolean

    of nkObjectType:
      result = a.objectTypeFields == b.objectTypeFields

    of nkObject:
      result = a.objectFields == b.objectFields

    of nkEnumType:
      result = a.enumTypeValues == b.enumTypeValues

    of nkTableType:
      result = a.tableKeyType == b.tableKeyType and a.tableValueType == b.tableValueType

    of nkArrayType:
      result = a.arrayValueType == b.arrayValueType

    of nkAnyType, nkVoidType, nkVoidValue:
      result = true

    of nkClassType, nkClassObj, nkTemplate:
      result = system.`==`(a, b)

proc newNode(kind: AtomicType): Node =
  Node(kind: nkAtomicType, atomicType: kind)

proc ast(m: Magic): Node =
  Node(kind: nkKeyword, keyword: m)

proc ast(num: float): Node =
  Node(kind: nkAtomic, atomicKind: atNumber, num: num)

proc ast(b: bool): Node =
  Node(kind: nkAtomic, atomicKind: atBool, boolean: b)

proc ast(s: string): Node =
  Node(kind: nkAtomic, atomicKind: atString, str: s)

proc astList(nodes: seq[Node]): Node =
  Node(kind: nkList, kids: nodes)

proc copyTree(n: Node): Node =
  # Creates a deep copy.
  # TODO: Make this monster simpler to read. There must a way to make this automatic.
  # TODO: create a macro for this and put it in dawid.
  # TODO: or, maybe just go over the fields of n in a generic way.
  # TODO: use tuples for each node kind so we can get around the problem of not being able copy unreferenced fields of Node (because of the kind field).

  assert not n.isNil

  case n.kind

  of nkMember:
    result = Node(kind: nkMember, member: n.member)

  of nkClassType:
    # TODO: questionable, but might fix a bug with compariosn of class types.
    result = n

  of nkClassObj:
    result = Node(kind: nkClassObj, classObjType: copyTree(n.classObjType))
    result.classObjFields = collect(for k, v in n.classObjFields: {k: copyTree v})
    result.classObjMethods = collect(for k, v in n.classObjMethods: {k: copyTree v}) # TODO: callstack size shittery when uncommented

  of nkVoidType:
    result = Node(kind: nkVoidType)

  of nkVoidValue:
    result = Node(kind: nkVoidValue)

  of nkTableType:
    result = Node(kind: nkTableType, tableKeyType: copyTree(n.tableKeyType), tableValueType: copyTree(n.tableValueType))

  of nkArrayType:
    result = Node(kind: nkArrayType, arrayValueType: copyTree(n.arrayValueType))

  of nkEnumType:
    result = Node(kind: nkEnumType)
    result.enumTypeValues = n.enumTypeValues.mapIt(copyTree it)

  of nkAnyType:
    result = Node(kind: n.kind)

  of nkSymbol:
    result = Node(kind: nkSymbol, sym: n.sym)

  of nkKeyword:
    result = Node(kind: nkKeyword, keyword: n.keyword)

  of nkDot:
    result = Node(kind: nkDot, dot: n.dot)

  of nkAtomicType:
    result = Node(kind: nkAtomicType, atomicType: n.atomicType)

  of nkAtomic:
    result = Node(kind: nkAtomic, atomicKind: n.atomicKind)
    case n.atomicKind
    of atNumber: result.num = n.num
    of atString: result.str = n.str
    of atBool: result.boolean = n.boolean

  of nkList:
    result = Node(kind: nkList)
    result.kids = n.kids.mapIt(copyTree it)

  of nkTable:
    result = Node(kind: nkTable)
    result.tableVal = collect:
      for k, v in n.tableVal:
        {copyTree k: copyTree v}

  of nkFunction:
    result = Node(kind: nkFunction)
    result.funcParams = n.funcParams # strings.
    result.funcTypes = n.funcTypes.mapIt(copyTree it)
    result.funcReturnType = copyTree(n.funcReturnType)
    result.funcBody = copyTree(n.funcBody) # TODO: callstack size shittery when uncommented
    if not n.funcObj.isNil:
      result.funcObj = n.funcObj

  of nkTemplate:
    result = Node(kind: nkTemplate)
    result.templateParams = n.templateParams
    result.templateBody = copyTree(n.templateBody)
  of nkObjectType:
    result = Node(kind: nkObjectType)
    result.objectTypeFields = collect:
      for k, v in n.objectTypeFields:
        {k: copyTree v}
  of nkObject:
    result = Node(kind: nkObject)
    result.objectType = copyTree(n.objectType)
    result.objectFields = collect:
      for k, v in n.objectFields:
        {k: copyTree v}

  result.line = n.line
  result.tags = n.tags

proc evalSymbol(st: State, n: Node): Node =
  for i in countdown(st.stacks.high, 0):
    let s = st.stacks[i]
    if s.symbols.hasKey(n.sym):
      result = s.symbols[n.sym]
      break

  n.assert not result.isNil, "Symbol not found: " & n.sym
  n.assert result.kind != nkSymbol, "Symbol cannot point to another symbol: " & n.sym

  result.line = n.line ## Important!

template temporaryStack(kindx: StackKind, code) =
  let initialStacks = s.stacks.len

  let newStack = Stack(kind: kindx)
  s.stacks.add newStack

  try:
    code
  finally:
    assert s.stacks[^1] == newStack
    s.stacks.delete s.stacks.high # We want to make sure that the stack is balanced because errors might be catchable from the lisp code itself (in the future).

  assert s.stacks.len == initialStacks, "Stack imbalance"

proc getAtomicValueKind(n: Node): AtomicType =
  n.assert n.kind == nkAtomic, "Expected atomic value"
  n.atomicKind

const
  symbolCharacters = Letters + Digits + {
    '_', '?', '!', '+', '-', '*', '/', '<', '>', '=',
    '&', '|', '%', '^', '~', ':', '\\', '@', '.'} # TODO: Dot was added late, it might break something.

proc tokenize(s: string): seq[Token] =
  const keywords = collect(for it in Magic: $it)
  const atomicTypes = collect(for it in AtomicType: $it)

  var i = 0
  var line = 1

  template add(x: Token) =
    result.add x

  while i <= s.high:
    case s[i]
    of {'#', ';'}:
      i += s.skipUntil(Newlines, i)
      inc i
      inc line

    of Newlines:
      inc i
      inc line

    of Whitespace - Newlines:
      inc i

    of '(':
      inc i
      add Token(kind: tkOpen, line: line)

    of ')':
      inc i
      add Token(kind: tkClose, line: line)

    of '@':
      inc i
      var str: string
      i += s.parseWhile(str, symbolCharacters, i)
      add Token(kind: tkMember, member: str, line: line)

    of '^':
      inc i
      var str: string
      i += s.parseWhile(str, symbolCharacters, i)

      const tags = collect(for it in TagKind: $it)
      assert str in tags, "Invalid tag: " & str & " at line " & $line

      add Token(kind: tkTag, tag: parseEnum[TagKind](str), line: line)

    of '"':
      inc i
      var str: string

      while true:
        var tmp: string
        i += s.parseUntil(tmp, {'"', '\\'}, i)

        if s[i] == '\\':
          inc i
          case s[i]
          of '"': tmp.add '"'
          of 'n': tmp.add '\n'
          of 't': tmp.add '\t'
          else: raiseAssert "Invalid escape sequence: " & $s[i]
          inc i

        str.add tmp

        if s[i] == '"':
          inc i
          break

      add Token(kind: tkString, str: str, line: line)

    of Digits:
      var tmp: string
      i += s.parseWhile(tmp, Digits + {'.'}, i)
      add Token(kind: tkNumber, num: tmp.parseFloat, line: line)

    else:
      doAssert s[i] in symbolCharacters:
        "Error on line " & $line & ": Unexpected character: " & $s[i]

      var tmp: string
      i += s.parseWhile(tmp, symbolCharacters + {'.'}, i)

      assert tmp.len > 0

      if '.' in tmp and tmp[0] != '.':
        add Token(kind: tkDot, dot: tmp)

      elif tmp == "true":
        add Token(kind: tkBool, boolVal: true, line: line)

      elif tmp == "false":
        add Token(kind: tkBool, boolVal: false, line: line)

      # elif tmp == "none":
      #   add Token(kind: tkNone, line: line)

      elif tmp == "any":
        add Token(kind: tkAnyType, line: line)

      elif tmp == "void":
        add Token(kind: tkVoidType, line: line)

      elif tmp in atomicTypes:
        add Token(kind: tkAtomicType, atomicType: parseEnum[AtomicType](tmp), line: line)

      elif tmp in keywords:
        add Token(kind: tkKeyword, keyword: parseEnum[Magic](tmp), line: line)
      else:
        add Token(kind: tkSymbol, sym: tmp, line: line)

proc `$`(n: Node): string =
  # TODO: Simplify this monstrosity.

  proc process(n: Node, result: var string) {.nimcall.} =
    assert not n.isNil

    for t in n.tags:
      result.add "@" & $t & " "

    case n.kind

    of nkMember:
      result.add n.member

    of nkClassType:
      result.add "class " & n.className
      if n.classParent != nil:
        result.add " : " & $n.classParent
      result.add " {"
      for k, v in n.classFields:
        result.add k & ": " & $v & ", "
      for k, v in n.classMethods:
        result.add k & ": " & $v & ", "
      result.add "}"

    of nkClassObj:
      result.add "class " & $n.classObjType & " {"
      for k, v in n.classObjFields:
        result.add k & ": " & $v & ", "
      result.add "}"

    of nkVoidType:
      result.add $tkVoidType

    of nkVoidValue:
      result.add "{void}"

    of nkAtomicType:
      result.add $n.atomicType

    # of nkNone:
    #   result.add "none"

    of nkSymbol:
      result.add n.sym

    of nkKeyword:
      result.add $n.keyword

    of nkDot:
      result.add n.dot

    of nkAnyType:
      result.add $tkAnyType

    of nkTableType:
      result.add "table {"
      process n.tableKeyType, result
      result.add ", "
      process n.tableValueType, result
      result.add "}"

    of nkArrayType:
      result.add "array {"
      process n.arrayValueType, result
      result.add "}"

    of nkEnumType:
      result.add "<enum> {"
      for v in n.enumTypeValues:
        result.add $v & ", "
      result.add "}"

    # of nkMacro:
    #   result.add "<macro> {"
    #   for p in n.macroParams:
    #     result.add p & ", "
    #   result.add "}"

    of nkAtomic:
      case n.atomicKind
      of atNumber: result.add $n.num
      of atString: result.add n.str
      of atBool: result.add $n.boolean

    of nkObject:
      result.add "<object> {"
      for k, v in n.objectFields:
        result.add k & ": " & $v & ", "
      result.add "}"

    of nkObjectType:
      result.add "<type> {"
      for k, v in n.objectTypeFields:
        result.add k & ": " & $v & ", "
      result.add "}"

    of nkTable:
      result.add "{"
      for k, v in n.tableVal:
        process k, result
        result.add ": "
        process v, result
        result.add ", "
      result.add "}"

    of nkList:
      result.add "("
      for i, k in n.kids:
        if i > 0:
          result.add " "
        process k, result
      result.add ")"

    of nkFunction:
      result.add "<function>"

    of nkTemplate:
      result.add "<template>"

  assert n != nil
  process n, result

proc `$`(s: Stack): string =
  result.add "<" & $s.kind & "> ["
  result.add s.symbols.keys.toSeq.join(", ")
  result.add "]"

template expectNotNil(nx: Node) =
  let n = nx
  n.assert not n.isNil:
    "Expected a non-nil node"

template expectKind(nx: Node, kindx: NodeKind) =
  let n = nx
  let kind = kindx
  n.expectNotNil
  n.assert n.kind == kind:
    "Expected " & $kind & " but got " & $n.kind & " in " & $n[]

template expectAtomic(nx: Node, kindx: AtomicType) =
  let n = nx
  let kind = kindx
  n.expectKind nkAtomic
  n.assert n.atomicKind == kind:
    "Expected " & $kind & " but got " & $n.atomicKind & " in " & $n

proc expectValidEnumValue(n: Node) =
  n.assert n.enumType != nil, "Expected an enum value"
  n.enumType.expectKind nkEnumType
  # dump n.repr
  # dump n.enumType.enumTypeValues[0].repr
  n.assert n in n.enumType.enumTypeValues, "Invalid enum value: " & $n & ". Valid values: " &
      n.enumType.enumTypeValues.mapIt('`' & $it & '`').join(", ")

iterator treeNodes(root: Node): Node =
  root.expectKind nkList
  var stack: seq[Node] = root.kids
  while stack.len > 0:
    let n = stack.pop
    if n.kind == nkList:
      for k in n.kids:
        stack.add k
    yield n

proc getBool(n: Node): bool =
  n.expectAtomic atBool
  n.boolean

proc expectInt(n: Node) =
  n.expectAtomic atNumber
  n.assert n.num.isInt, "Expected an integer"

proc `[]`(n: Node, i: int): Node =
  n.expectKind nkList
  n.assert i in 0..n.kids.len:
    "Index " & $i & "not found in list: " & $n
  n.kids[i]

proc expectListLen(n: Node, len: int) =
  n.expectKind nkList
  # echo n.kids.mapIt(it.kind)
  n.assert n.kids.len == len:
    "Expected a list of " & $len & " values but got " & $n.kids.len

proc getType(n: Node): Node =
  # TODO: unify typed under the same Node field.
  if n.enumType != nil:
    result = n.enumType
  else:
    case n.kind
    of nkAtomic:
      result = newNode n.getAtomicValueKind
    of nkObject:
      result = n.objectType
    of nkList:
      result = n.listType
    of nkTable:
      result = n.tableType
    of nkVoidValue:
      result = Node(kind: nkVoidType)
    of nkClassObj:
      result = n.classObjType
    else:
      n.assert false:
        "Node does not have a typed value: " & $n

  n.assert result != nil, "Internal error, can't get the type of: " & n.repr

proc canConvertToEnum(n: Node, enumType: Node): bool =
  n.assert n.kind != nkSymbol: "Node must be evaluated before conversion"
  enumType.expectKind nkEnumType
  result = false
  for v in enumType.enumTypeValues:
    if n == v:
      result = true
      break

const
  typeKinds = {nkEnumType, nkAtomicType, nkObjectType, nkTableType, nkAnyType, nkVoidType, nkClassType, nkArrayType}

proc inheritsFrom(a, b: Node): bool =
  var a = a
  var b = b

  assert a.kind in {nkClassType, nkClassObj}
  assert b.kind in {nkClassType, nkClassObj}

  if a.kind == nkClassObj:
    a = a.classObjType

  if b.kind == nkClassObj:
    b = b.classObjType

  result = false

  # dump a[]
  # dump a.classParent.isNil

  if a == b:
    result = true
  else:
    var nextAncestor = a.classParent
    # dump nextAncestor.repr
    # dump b.repr
    # dump system.`==`(nextAncestor, b)
    while nextAncestor != nil:
      if nextAncestor == b:
        result = true
        break
      nextAncestor = nextAncestor.classParent

  dump result

proc canConvertToType(n: Node, newType: Node): bool =
  assert not n.isNil
  assert not newType.isNil
  assert newType.kind in typeKinds: "Type node expected, got: " & $n

  case newType.kind

  of nkClassType:
    result = inheritsFrom(n, newType)

  of nkAnyType:
    result = true

  of nkEnumType:
    result = canConvertToEnum(n, newType)

  of nkTableType:
    if n.kind == nkTable:
      result = true
      for k, v in n.tableVal:
        if not k.canConvertToType(newType.tableKeyType) or not v.canConvertToType(newType.tableValueType):
          result = false
          break
    else:
      result = false

  of nkArrayType:
    result = true
    if n.kind == nkList:
      for it in n.kids:
        if not it.canConvertToType(n.listType.arrayValueType):
          result = false
          break
    else:
      result = false

  else:
    result = n.getType == newType

proc convertValueToType(val, typ: Node): Node =
  # Implicit conversions.

  assert not val.isNil
  assert not typ.isNil

  val.assert not (val.kind != nkVoidValue and typ.kind == nkVoidType):
    "Unused (non-void) value must be discarded manually."

  val.assert val.canConvertToType typ:
    "Can't convert value from type " & $val.getType & " to type " & $typ & ": " & val.repr

  case typ.kind

  of nkEnumType:
    result = val.copyTree
    result.enumType = typ

  of nkList:
    result = Node(kind: nkList, listType: typ)
    for k in val.kids:
      result.kids.add convertValueToType(k, typ.listType)

  of nkTableType:
    var newTable: Table[typ.tableKeyType, typ.tableValueType]
    for k, v in val.tableVal:
      assert k notin newTable
      newTable[k] = v
    result = Node(kind: nkTable, tableVal: newTable, tableType: typ)

  of nkAnyType:
    result = val

  of nkClassType:
    assert val.classObjMethods.len > 0

    result = val.copyTree # TODO: questionable, but we don't want to change the type of an existing node. or any other of it's properties. Let's figure out a way of having a node kind that wraps other nodes without copying them such that their nominal type is changed.

    result.classObjType = typ

  else:
    val.assert typ == val.getType, "Internal error: " & $typ
    result = val

proc allInheritedClassFields(n: Node): Table[string, Node] =
  n.expectKind nkClassType
  result = n.classFields
  var nextAncestor = n.classParent
  while nextAncestor != nil:
    for k, v in nextAncestor.classFields:
      n.assert k notin result, "Field defined more than once: " & k
      result[k] = v
    nextAncestor = nextAncestor.classParent

const
  binaryKeywords = {
    mAssign, mVar, mTest, mPlus, mEqualPlus, mMinus, mEqualMinus, mEqual, mOr, mAnd, mRange,
    mLesserOrEq, mLesser, mGreaterOrEq, mGreater}

proc postProcessing(root: Node) =
  var stack = @[root]
  while stack.len > 0:
    let n = stack.pop
    if n.kind == nkList:
      stack.add n.kids
      if n.kids.len == 3:
        if n.kids[0].kind == nkKeyword:
          if n.kids[0].keyword in binaryKeywords:
            n.assert false, "Binary operator must be used like this: (a + b) instead of (+ a b)"
        elif n.kids[1].kind == nkKeyword:
          if n.kids[1].keyword in binaryKeywords:
            n.kids = @[n.kids[1], n.kids[0], n.kids[2]]

proc buildAst(tokens: openarray[Token]): Node =
  result = Node(kind: nkList, line: 1, kids: @[Node(kind: nkKeyword, keyword: mBlock, line: 1)])

  var stack = @[result]
  var currentTags: set[TagKind]

  for t in tokens:
    t.assert stack.len > 0, "Unmatched closing parenthesis"

    template add(nx: Node, newStack = false) =
      let n = nx
      assert n.line == 0
      assert n.tags.card == 0

      n.line = t.line

      n.tags = currentTags
      currentTags = {}

      stack[^1].kids.add n
      if newStack:
        stack.add n

    case t.kind

    of tkMember:
      add Node(kind: nkMember, member: t.member)

    of tkVoidType:
      add Node(kind: nkVoidType)

    of tkAnyType:
      add Node(kind: nkAnyType)

    of tkTag:
      t.assert t.tag notin currentTags, "Tag defined more than once for the same node: " & $t.tag
      currentTags.incl t.tag

    of tkDot:
      add Node(kind: nkDot, dot: t.dot)

    of tkAtomicType:
      add Node(kind: nkAtomicType, atomicType: t.atomicType)

    of tkOpen:
      add Node(kind: nkList), true

    of tkClose:
      t.assert stack.len > 0, "Unmatched closing parenthesis"
      stack.delete stack.high # Discard the current scope.

    # of tkNone:
    #   add Node(kind: nkNone)

    of tkSymbol:
      add Node(kind: nkSymbol, sym: t.sym)

    of tkNumber:
      add Node(kind: nkAtomic, atomicKind: atNumber, num: t.num)

    of tkString:
      add Node(kind: nkAtomic, atomicKind: atString, str: t.str)

    of tkKeyword:
      add Node(kind: nkKeyword, keyword: t.keyword)

    of tkBool:
      add Node(kind: nkAtomic, atomicKind: atBool, boolean: t.boolVal)

  # dump tokens.toSeq.repr

  tokens[0].assert stack.len == 1:
    "Unmatched opening parenthesis"

  postProcessing result

proc evaluate(s: State, n: Node): Node

proc evaluateKeywordCall(s: State, n: Node): Node {.nimcall.} =
  # TODO: fix crappy error-prone code

  n.assert n.kind == nkList
  n.assert n.kids.len > 0
  n.assert n.kids[0].kind == nkKeyword

  let args = n.kids[1..^1]

  case n.kids[0].keyword

  of mContinue:
    n.assert args.len == 0
    raise Continue()

  of mBreak:
    n.assert args.len == 0
    raise Break()

  of mTableHasKey:
    n.assert args.len == 2

    let table = s.evaluate args[0]
    let key = s.evaluate args[1]

    table.expectKind nkTable
    key.assert key.canConvertToType table.tableType.tableKeyType, "Invalid key type"

    result = ast: table.tableVal.hasKey(key)

  of mTableGet:
    n.assert args.len == 2

    let table = s.evaluate args[0]
    let key = s.evaluate args[1]

    table.expectKind nkTable
    key.assert key.canConvertToType table.tableType.tableKeyType, "Invalid key type"

    result = table.tableVal.getOrDefault(key)

    n.assert result != nil, "Key not found: " & $key

  of mTableSet:
    n.assert args.len == 3

    let table = s.evaluate args[0]
    let key = s.evaluate args[1]
    let val = s.evaluate args[2]

    table.expectKind nkTable
    key.assert key.canConvertToType table.tableType.tableKeyType, "Invalid key type"
    val.assert val.canConvertToType table.tableType.tableValueType, "Invalid value type"

    table.tableVal[key] = val

    result = Node(kind: nkVoidValue)

  of mCase:
    n.assert args.len >= 2

    let compared = s.evaluate args[0]

    for b in args[1..^1]:
      b.expectKind nkList
      b.assert b.kids.len >= 2

      let condition = b.kids[0]

      if condition.kind == nkList:
        for it in condition.kids:
          it.expectKind nkAtomic
          if it == compared:
            result = s.evaluate: astList @[mBlock.ast] & b.kids[1..^1]
            break
      else:
        condition.expectKind nkAtomic
        if compared == condition:
          result = s.evaluate: astList @[mBlock.ast] & b.kids[1..^1]

    n.assert result != nil, "No matching case branch found for: " & $compared

  of mArrayType:
    n.assert args.len == 1

    let valueType = s.evaluate args[0]
    args[0].assert valueType.kind in typeKinds: "Expected a type"

    result = Node(kind: nkArrayType, arrayValueType: valueType)

  of mTableType:
    n.assert args.len == 2

    let keyType = s.evaluate args[0]
    let valueType = s.evaluate args[1]

    args[0].assert keyType.kind in typeKinds: "Expected a type"
    args[1].assert valueType.kind in typeKinds: "Expected a type"

    result = Node(kind: nkTableType, tableKeyType: keyType, tableValueType: valueType)

  of mEnum:
    n.assert args.len >= 1

    result = Node(kind: nkEnumType)

    for a in args:
      let a = s.evaluate a
      result.enumTypeValues.add a

  of mExcept, mFinally:
    n.assert false: "Except and finally blocks are not allowed outside of a try block"

  of mTry:
    n.assert args.len >= 1

    let body = astList: @[mBlock.ast]
    var exceptBlock: Node
    var finallyBlock: Node

    for a in args:
      var didFind = false

      if a.kind == nkList:
        a.assert a.kids.len > 0: "Empty list is invalid"
        case a[0].keyword
        of mExcept:
          a.assert exceptBlock == nil: "Only one except block allowed"
          a.assert finallyBlock == nil: "Finally block must be the last one"
          exceptBlock = astList @[mBlock.ast] & a.kids[1..^1]
          didFind = true
        of mFinally:
          a.assert finallyBlock == nil: "Only one finally block allowed"
          finallyBlock = astList @[mBlock.ast] & a.kids[1..^1]
          didFind = true
        else:
          discard

      if not didFind:
        a.assert exceptBlock == nil: "Except block must be declared after the body"
        a.assert finallyBlock == nil: "Finally block must be declared after the body"
        body.kids.add a

    try:
      result = s.evaluate body

    except Return:
      raise

    except Exception:
      if exceptBlock != nil:
        result = s.evaluate exceptBlock
      else:
        raise # TODO: not sure, this was added way after the initial implementation

    finally:
      if finallyBlock != nil:
        result = s.evaluate finallyBlock

  of mAdd:
    n.assert args.len == 2, "Add takes exactly two arguments"

    let a = s.evaluate(args[0])
    let b = s.evaluate(args[1])

    a.expectKind nkList

    a.kids.add b

    result = Node(kind: nkVoidValue)

  of mJoinLists:
    n.assert args.len >= 2, "join-lists takes at least two arguments"

    result = Node(kind: nkList)

    for a in args:
      let a = s.evaluate a
      a.expectKind nkList
      result.kids.add a.kids

  of mGreater:
    n.assert args.len == 2, "Greater takes exactly two arguments"

    let a = s.evaluate(args[0])
    let b = s.evaluate(args[1])

    a.expectAtomic atNumber
    b.expectAtomic atNumber

    result = ast: a.num > b.num

  of mLesser:
    n.assert args.len == 2, "Lesser takes exactly two arguments"

    let a = s.evaluate(args[0])
    let b = s.evaluate(args[1])

    a.expectAtomic atNumber
    b.expectAtomic atNumber

    result = ast: a.num < b.num

  of mGreaterOrEq:
    n.assert args.len == 2, "GreaterOrEq takes exactly two arguments"

    let a = s.evaluate(args[0])
    let b = s.evaluate(args[1])

    a.expectAtomic atNumber
    b.expectAtomic atNumber

    result = ast: a.num >= b.num

  of mLesserOrEq:
    n.assert args.len == 2, "LessOrEq takes exactly two arguments"

    let a = s.evaluate(args[0])
    let b = s.evaluate(args[1])

    a.expectAtomic atNumber
    b.expectAtomic atNumber

    result = ast: a.num <= b.num

  of mWhile:
    n.assert args.len >= 2, "While takes at least two arguments"

    let condition = args[0]
    let body = astList: @[mBlock.ast] & args[1..^1]

    result = Node(kind: nkVoidValue)

    while s.evaluate(condition).getBool:
      temporaryStack skBlock:
        discard s.evaluate(body)

  # of mMacro:
  #   n.assert args.len == 2: "Macro takes exactly two arguments"

  #   let params = n.kids[0]

  #   params.expectKind nkList
  #   for p in params.kids:
  #     p.expectKind nkSymbol

  #   let body = ast: @[mBlock.ast] & args[1..^1]

  #   result = Node(kind: nkMacro, macroParams: params.kids.mapIt(it.sym), macroBody: body)

  of mTemplate:
    n.assert args.len >= 2

    let params = args[0]

    params.expectKind nkList
    for p in params.kids:
      p.expectKind nkSymbol

    let body = astList: @[mBlock.ast] & args[1..^1]

    result = Node(kind: nkTemplate, templateParams: params.kids.mapIt(it.sym), templateBody: body)

  of mContains:
    n.assert args.len == 2, "Contains takes exactly two arguments"

    let l = s.evaluate args[0]
    l.expectKind nkList

    let val = s.evaluate args[1]

    result = ast: val in l.kids

  of mFor:
    n.assert args.len >= 3, $n

    var keySym, valSym: Node

    if args[0].kind == nkList:
      keySym = args[0][0]
      valSym = args[0][1]
      keySym.expectKind nkSymbol
      valSym.expectKind nkSymbol
    else:
      args[0].expectKind nkSymbol
      valSym = args[0]
      valSym.expectKind nkSymbol

    let items = s.evaluate args[1]
    items.expectKind nkList

    let body = Node(kind: nkList, kids: @[Node(kind: nkKeyword, keyword: mBlock)])
    for n in args[2..^1]:
      body.kids.add n

    result = Node(kind: nkVoidValue)

    for i, item in items.kids:
      temporaryStack skBlock:
        try:
          s.stacks[^1].symbols[valSym.sym] = item
          if keySym != nil:
            s.stacks[^1].symbols[keySym.sym] = ast: i.float
          discard s.evaluate body
        except Break:
          break
        except Continue:
          continue

  of mEmpty:
    assert args.len == 1
    let l = s.evaluate args[0]

    # echo "#####"
    # echo "#####"
    # echo "#####"
    # echo "#####"
    # echo "#####"
    # dump l[]

    if l.kind == nkList:
      result = ast l.kids.len == 0
    elif l.isKind atString:
      result = ast l.str.len == 0
    else:
      n.assert false, "Empty can only be used with lists and strings"

  of mNot:
    assert args.len == 1
    let b = s.evaluate args[0]
    b.expectAtomic atBool
    result = ast: not s.evaluate(args[0]).boolean

  of mEqualPlus:
    assert args.len >= 2

    let sym = args[0]
    sym.expectKind nkSymbol

    let symVal = s.evaluate sym
    symVal.expectAtomic atNumber

    for a in args[1..^1]:
      let a = s.evaluate a
      a.expectAtomic atNumber

      symVal.num += a.num

    result = Node(kind: nkVoidValue)

  of mEqualMinus:
    assert args.len >= 2

    let sym = args[0]
    sym.expectKind nkSymbol

    let symVal = s.evaluate sym
    symVal.expectAtomic atNumber

    result = ast: 0.0

    for a in args[1..^1]:
      let a = s.evaluate a
      a.expectAtomic atNumber

      symVal.num -= a.num

    result = Node(kind: nkVoidValue)

  of mEqual:
    assert args.len == 2

    let a = s.evaluate(args[0])
    let b = s.evaluate(args[1])

    result = ast: a == b

    # if result.getBool:
    #   dump a.repr
    #   dump b.repr
    #   dump $a.hash
    #   dump $b.hash
    #   echo "+++++++"

  of mNotEqual:
    assert args.len == 2
    result = ast: s.evaluate(args[0]) != s.evaluate(args[1])

  of mToStr:
    assert args.len == 1
    result = ast: $s.evaluate(args[0])
    # echo ("!", args[0], result)

  of mAnd:
    assert args.len > 0

    result = ast: true
    for a in args:
      # dump a.kind
      if not s.evaluate(n).getBool:
        result.boolean = false
        break

  of mOr:
    assert args.len > 0

    result = ast: false
    for a in args:
      # dump a.kind
      if s.evaluate(a).getBool:
        result.boolean = true
        break

  of mReturn:
    assert args.len in {0, 1}

    let res =
      if args.len == 0:
        Node(kind: nkVoidValue)
      else:
        assert args.len == 1
        s.evaluate(args[0])

    n.assert res != nil

    raise Return(value: res)

  of mIsStr:
    assert args.len == 1
    result = ast: s.evaluate(args[0]).isKind atString

  of mIsList:
    assert args.len == 1
    result = ast s.evaluate(args[0]).kind == nkList

  of mExpect:
    assert args.len == 2

    result = Node(kind: nkVoidValue)

    let a = s.evaluate args[0]
    let b = s.evaluate args[1]

    n.assert a == b:
      "Expectation failed => " & $n & "\n" & $args[0] & " => " & $a & "\n" & $args[1] & " => " & $b

    # let b = s.evaluate args[0]
    # b.expectAtomic atBool
    # b.assert b.boolean, "Expectation failed => " & $n

  of mLen:
    assert args.len == 1

    let l = s.evaluate args[0]
    l.expectKind nkList

    result = ast l.kids.len.float

  of mLow:
    assert args.len == 1

    let l = s.evaluate args[0]
    l.expectKind nkList

    result = ast l.kids.low.float

  of mHigh:
    assert args.len == 1

    let l = s.evaluate args[0]
    l.expectKind nkList

    result = ast: l.kids.high.float

  of mRange:
    assert args.len == 2

    let a = s.evaluate args[0]
    a.expectInt

    let b = s.evaluate args[1]
    a.expectInt

    result = Node(kind: nkList)

    for i in a.num.int .. b.num.int:
      result.kids.add ast i.float

  of mStrAdd:
    assert args.len >= 2, $n.line

    let sym = args[0]
    sym.expectKind nkSymbol

    let symVal = s.evaluate(sym)
    symVal.expectAtomic atString

    for a in args[1..^1]:
      let str = s.evaluate a
      str.expectAtomic atString
      symVal.str.add str.str

    result = Node(kind: nkVoidValue)

  of mAssign:
    n.assert args.len == 2:
      "Assign takes exactly two arguments"

    let lsym = args[0]
    lsym.assert lsym.kind == nkSymbol:
      "First argument to assign must be a symbol"

    let lval = s.evaluate(lsym)
    let ltype = lval.getType

    let rval = s.evaluate(args[1])
    let rtype = rval.getType

    args[1].assert lval.kind notin typeKinds: "Rebinding types is not allowed"

    let converted = rval.copyTree.convertValueToType(ltype) # Copying the tree to avoid changing the type of the original value. TODO: This changes ref semantics to struct semantics (implicitly), which is probably bad.
 # dump lval[]
    lval[] = converted[] # FIXME: This is a very bad way to copy. Reference/struct semantics should be dependant on the type of the rvalue, and n ested. Use a dedicated (copy) for actually copying values.

    result = Node(kind: nkVoidValue)

  of mClass:
    n.assert args.len > 0, "Invalid class declaration"

    let name = args[0]
    name.expectKind nkSymbol # TODO: validate that the symbol is not already defined in current scope, using a proc that should then be used everywhere else in the old code!

    var parent = s.evaluate args[1]
    if parent.kind == nkVoidType:
      parent = nil
    else:
      parent.expectKind nkClassType # TODO: check for circular inheritance

    result = Node(kind: nkClassType, classParent: parent, className: name.sym)

    for a in args[2..^1]:
      a.expectKind nkList
      a[0].expectKind nkSymbol # TODO: or keyword

      # TODO: methods and fields must not share a common name!
      # We might need to store all kinds of members in the same table.

      case a[0].sym

      of "field":
        a.assert a.kids.len == 3, "Field declaration must have exactly 2 arguments"

        # TODO: this crap should share code with struct/object code for fields.

        let fieldName = a[1]
        fieldName.expectKind nkSymbol
        fieldName.assert fieldName.sym notin result.classFields, "Field already defined: " & fieldName.sym

        let fieldType = s.evaluate(a[2])
        fieldType.assert fieldType.kind in typeKinds, "Expected a type"

        result.classFields[fieldName.sym] = fieldType

      of "method":
        a.assert a.kids.len >= 2, "Method declaration must have at least two arguments"

        let methodName = a[1]
        methodName.expectKind nkSymbol
        methodName.assert methodName.sym notin result.classMethods, "Method already defined: " & methodName.sym

        let funcNode = s.evaluate a[2]
        funcNode.expectKind nkFunction

        result.classMethods[methodName.sym] = funcNode

      else:
        n.assert false, "Invalid class member: " & a[0].sym

    s.stacks[^1].symbols[name.sym] = result

    result = Node(kind: nkVoidValue)

  of mType:
    n.assert args.len == 1, "Type declaration has no arguments"

    let fields = args[0]
    n.assert fields.kind == nkList, "Type fields must be a list"

    result = Node(kind: nkObjectType)

    for f in fields.kids:
      f.assert f.kind == nkList, "Type fields must be lists"
      f.assert f.kids.len == 2, "Type fields must have exactly two elements"

      let name = f.kids[0]
      name.assert name.kind == nkSymbol, "Type field names must be symbols"
      name.assert name.sym notin result.objectTypeFields, "Field already defined: " & name.sym

      let typ = s.evaluate f.kids[1]
      typ.assert typ.kind in typeKinds: "Expected a type: " & $typ.kind

      result.objectTypeFields[name.sym] = typ

  of mTest:
    n.assert args.len == 2, "Test takes 2 arguments"
    result = Node(kind: nkVoidValue)
    # let label = s.evaluate(args[0])
    # label.assert label.kind == nkString, "First argument to test must be a string"
    let expected = s.evaluate(args[0])
    let actual = s.evaluate(args[1])

    # if expected != actual:
    #   dump expected.str
    #   dump actual.str
    #   # dump expected.str.toSeq
    #   # dump actual.str.toSeq

    let cond = expected == actual

    n.assert cond, "\n" & $n & "\n" & $args[0] & " => " & $expected & "\n" & $args[1] & " => " & $actual
    # echo "Test OK at line ", n.line

  of mTable:
    result = Node(kind: nkTable, tableType: Node(kind: nkTableType))
    result.tableType.tableKeyType = Node(kind: nkAnyType)
    result.tableType.tableValueType = Node(kind: nkAnyType)

    for a in args:
      n.assert a.kind == nkList, "Expected pairs of keys and values"
      n.assert a.kids.len == 2, "Expected pairs of keys and values"
      let key = s.evaluate(a.kids[0])
      let value = s.evaluate(a.kids[1])
      result.tableVal[key] = value

  of mBlock:
    # s.stacks.add Stack()
    temporaryStack skBlock:
      result = Node(kind: nkVoidValue)
      for a in args:
        result = s.evaluate(a)
    # s.stacks.delete s.stacks.high

  of mRepr:
    n.assert args.len == 1, "Repr takes exactly one argument"

    let arg = s.evaluate(args[0])

    # TODO: this kind of thing should be a separate proc
    let str =
      if arg.isKind atString:
        '"' & arg.str & '"'
      else:
        $arg

    print "[" & $n.line & "] " & $n & " => " & str

    result = Node(kind: nkVoidValue)

  # of mEchoNoNewline:
  #   n.assert args.len == 1, "Echo takes exactly one argument"

  #   print s.evaluate(args[0])

  #   result = Node(kind: nkVoidValue)

  of mEcho:
    n.assert args.len == 1, "Echo takes exactly one argument"
    result = Node(kind: nkVoidValue)

    let arg = s.evaluate(args[0])

    print $arg

  of mLambda:
    assert args.len >= 2 ## TODO: Make the entire proc work with the call node (as an argument) rather than with the keyword node.

    let params = args[0]
    params.expectKind nkList

    # TODO: Implement return type.
    let funcReturnType = s.evaluate args[1]
    args[1].assert funcReturnType.kind in typeKinds, "Expected a return type: " & $funcReturnType

    var funcParams: seq[string]
    var funcTypes: seq[Node]

    for p in params.kids:
      if p.kind == nkSymbol:
        funcParams.add p.sym
        funcTypes.add nil ## No type specified.
      else:
        n.assert p.kind == nkList, "Invalid lambda parameter: " & $p
        p.assert p.kids.len == 2, "Invalid lambda parameter: " & $p

        let name = p.kids[0]
        name.assert name.kind == nkSymbol, "Invalid lambda parameter: " & $p

        let typ = s.evaluate p.kids[1]
        typ.assert typ.kind in typeKinds, "Invalid lambda parameter: " & $p

        funcParams.add name.sym
        funcTypes.add typ

    let body = @[mBlock.ast].astList
    body.kids.add args[1..^1]

    n.assert not body.isNil
    n.assert not funcReturnType.isNil

    result = Node(kind: nkFunction, funcParams: funcParams, funcTypes: funcTypes, funcBody: body,
        funcReturnType: funcReturnType)

  of mQuote:
    n.assert args.len == 1, "Quote takes exactly one argument"
    result = args[0]

  of mEval:
    n.assert args.len == 1, "Eval takes exactly one argument"
    var a = args[0]
    if a.kind == nkSymbol:
      a = s.evaluate(a)
    n.assert a.kind == nkList, "Eval takes a list as an argument"
    result = s.evaluate(a)

  of mVar:
    n.assert args.len == 2, "Var takes exactly two arguments"

    let sym = args[0]
    n.assert sym.kind == nkSymbol, "First argument to var must be a symbol"
    n.assert not s.stacks[^1].symbols.hasKey(sym.sym), "Symbol already defined: " & sym.sym

    let value = s.evaluate args[1]
    # echo ("?", args[1], value)
    n.assert value.kind != nkSymbol: "This is why Common Lisp uses keywords instead of symbols for this purpose."

    s.stacks[^1].symbols[sym.sym] = value

    result = value

  # of mDo: # TODO: this is not needed when we have block.
  #   for a in args:
  #     result = s.evaluate(a)

  # TODO: this should create a temporary stack for the block.
  # (before evaluation so the first branch has access to variables defined in the condition expression)
  of mIf:
    n.assert args.len in [2, 3], "If takes two or three arguments"

    let condition = s.evaluate(args[0])
    n.assert condition.getAtomicValueKind == atBool, "First argument to if must be a boolean"

    if condition.boolean:
      result = s.evaluate(args[1])
    else:
      if args.len == 3:
        result = s.evaluate(args[2])
      else:
        result = Node(kind: nkVoidValue)

  of mFilterArray:
    n.assert args.len == 2, "filter-array takes exactly two arguments"

    let arr = s.evaluate args[0]
    arr.expectKind nkList

    let compare = s.evaluate args[1]
    compare.expectKind nkFunction

    result = Node(kind: nkList) # TODO: make this array typed

    for a in arr.kids:
      let res = s.evaluate: astList @[compare, a]
      res.expectKind nkAtomic
      res.expectAtomic atBool
      if res.boolean:
        result.kids.add a

  of mArrayGet:
    n.assert args.len == 2, "array-get takes exactly two arguments"

    let arr = s.evaluate args[0]
    arr.expectKind nkList

    let idx = s.evaluate args[1]
    idx.expectInt

    result = arr.kids[idx.num.int]

  of mMinus:
    n.assert args.len > 0

    let args = args.mapIt: s.evaluate it
    for a in args:
      a.expectAtomic atNumber

    if args.len == 1:
      result = ast: -s.evaluate(args[0]).num
    else:
      result = ast: s.evaluate(args[0]).num
      for a in args[1..^1]:
        result.num -= s.evaluate(a).num

  of mPlus:
    result = Node(kind: nkAtomic, atomicKind: atNumber, num: 0)
    for a in args:
      let a = s.evaluate(a)
      n.assert a.getAtomicValueKind == atNumber, "Argument to + must be a number"
      # n.assert a.kind == nkAtomic and a.atomicKind == atNumber, "Argument to + must be a number"
      result.num += a.num


proc evaluate(s: State, n: Node): Node =
  # ## TODO: questionable

  case n.kind

  of nkKeyword, nkFunction, nkTable, nkAtomicType, nkObjectType, nkObject, nkAtomic, nkTemplate, nkEnumType,
      nkTableType, nkArrayType, nkAnyType, nkVoidType, nkVoidValue, nkClassObj, nkClassType: #, nkMacro:
    result = n ## These values don't evaluate to anything other than themselves.

  of nkMember:
    for i in countdown(s.stacks.high, s.stacks.low):
      let s = s.stacks[i]
      if not s.currentClassObj.isNil:
        # TODO: a common proc for checking if a field belongs to a class type/obj
        let field = s.currentClassObj.classObjFields.getOrDefault(n.member)
        if not field.isNil:
          result = field
          break
    if result.isNil:
      n.assert false, "Member not found: " & n.member

  of nkDot:
    for i, sym in enumerate n.dot.split("."):
      if i == 0:
        result = s.evalSymbol(Node(kind: nkSymbol, sym: sym))
      else:
        case result.kind
        of nkObject:
          result = result.objectFields.getOrDefault(sym)
        of nkClassObj:
          # TODO: this is not readable
          var tmp = result.classObjFields.getOrDefault(sym)
          if tmp.isNil:
            tmp = result.classObjMethods.getOrDefault(sym)
            # if not tmp.isNil:
            #   tmp = tmp.copyTree
            #   tmp.funcObj = result
          result = tmp
        else:
          n.assert false, "Dot call on non-object: " & $result

      n.assert result != nil, "Dot call symbol not found: " & sym & " from " & n.dot & " where stacks " & $s.stacks

  of nkList:
    ## Non-empty list is interpreted as a function call.
    n.assert n.kids.len > 0:
      "Empty list cannot be interpreted as a routine call"

    let f = s.evaluate n.kids[0]

    let args = if n.kids.len > 1: n.kids[1..^1] else: @[]
    ## Args shouldn't be evaluated (yet) in case they are processed as an AST.

    case f.kind

    of nkSymbol, nkDot, nkMember:
      # TODO: create and call a procedure that checks if a node has already been evaluated. There should be a flag on Node for this.
      n.assert false, "This node should have been evaluated before getting passed here" ## These should have already been evaluated into a different kind of node!

    of nkAtomicType, nkObject, nkAtomic, nkTableType, nkArrayType, nkAnyType, nkVoidType, nkVoidValue, nkClassObj:
      n.assert false, "First element of list is not callable: " & $n & ", " & $f

    of nkEnumType:
      n.assert args.len == 1, "Enum type variable declaration requires exactly 1 argument"

      result = s.evaluate args[0]
      result.enumType = f
      result.expectValidEnumValue

    of nkList:
      n.expectListLen 2
      let idx = s.evaluate args[0]
      idx.expectInt
      result = f.kids[idx.num.int]

    of nkClassType:
      # Class constructor.

      # TODO: this must share code with the struct constructor

      # n.assert args.len >= 1, "Invalid class type declaration"

      let obj = Node(kind: nkClassObj, classObjType: f) # TODO: f is calle

      let allFields = allInheritedClassFields(f)

      for a in args:
        a.expectKind nkList
        a.expectListLen 2

        let fieldName = a[0]
        fieldName.expectKind nkSymbol
        fieldName.assert fieldName.sym in allFields, "Field not found in class type: " & fieldName.sym
        fieldName.assert fieldName.sym notin obj.classObjFields, "Field already defined: " & fieldName.sym

        let fieldValue = s.evaluate a[1]
        fieldValue.assert fieldValue.canConvertToType allFields[fieldName.sym], "Invalid field type"

        obj.classObjFields[fieldName.sym] = fieldValue

      for name in allFields.keys:
        n.assert name in obj.classObjFields, "Field missing: " & name

      var currentAncestor = obj.classObjType
      while currentAncestor != nil:
        # TODO: get non-method fields from ancestors
        # for field in currentAncestor.classMethods:
        #   if field notin obj.classObjMethods:
        #     let meth = currentAncestor.classObjMethods[field].copyTree
        #     meth.funcObj = obj
        #     obj.classObjMethods[field] = meth

        # for field in currentAncestor.classFields.keys:
        #   n.assert field notin obj.classObjFields, "Field already defined: " & field & " in class " &
        #       obj.classObjType.className
        #   obj.classObjFields[field] = currentAncestor.classFields[field]

        # # Check if all inherited fields were assigned
        # for fieldName in currentAncestor.classFields.keys:
        #   n.assert fieldName in obj.classObjFields, "Field missing: " & fieldName

        for methodName in currentAncestor.classMethods.keys:
          if methodName notin obj.classObjMethods:
            let meth = currentAncestor.classMethods[methodName].copyTree
            meth.funcObj = obj
            obj.classObjMethods[methodName] = meth

        currentAncestor = currentAncestor.classParent

      result = obj

    of nkObjectType:
      n.assert args.len == 1, "Object type variable declaration requires exactly 1 argument"
      let fields = args[0]
      fields.assert fields.kind == nkList, "Object fields must be a list"

      result = Node(kind: nkObject, objectType: f)

      for a in fields.kids:
        a.assert a.kind == nkList, "Expected pairs of keys and values"
        a.assert a.kids.len == 2, "Expected pairs of keys and values"

        let key = a.kids[0]
        key.assert key.kind == nkSymbol, "Object field names must be symbols, got: " & $key.kind
        key.assert key.sym notin result.objectFields, "Field already defined: " & key.sym

        let fieldTyp0 = f.objectTypeFields.getOrDefault key.sym
        key.assert not fieldTyp0.isNil, "Field not found in object type: " & key.sym

        let fieldTyp = s.evaluate fieldTyp0
        key.assert fieldTyp.kind in typeKinds:
          "Invalid field type: " & $fieldTyp.kind

        # let value = s.evaluate a.kids[1]
        let value = s.evaluate(a.kids[1]).convertValueToType(fieldTyp)

        assert value != nil, $a.kids[1]

        value.assert value.getType == fieldTyp:
          "Type mismatch for field `" & key.sym & "`. Expected " & $fieldTyp & ", got " & $value.getType

        result.objectFields[key.sym] = value

      for field in f.objectTypeFields.keys:
        n.assert field in result.objectFields, "Field missing: " & field

    of nkTable:
      let key = s.evaluate n.kids[1]
      result = f.tableVal.getOrDefault key
      n.assert result != nil, "Key not found: " & $key

    of nkKeyword:
      result = s.evaluateKeywordCall(n)

    of nkTemplate:
      n.assert args.len == f.templateParams.len:
        "Wrong number of arguments for template: " & $n

      let templateArgs = collect:
        for i, param in f.templateParams:
          {param: args[i]} # Intentionally unevaluated.

      proc update(s: State, n: var Node, templateArgs: Table[string, Node], parent: Node,
          indexInParent: int) {.nimcall.} =

        assert not n.isNil

        if tkUnquote in n.tags:
          n.tags.excl tkUnquote

          n.assert n.sym in templateArgs:
            "Template call has no argument to unquote: " & n.sym

          n = templateArgs[n.sym]

          assert not n.isNil

        elif tkUnquoteSplicing in n.tags:
          n.assert n.sym in templateArgs: "Template call has no argument to unquote-splicing: " & n.sym
          n.assert parent != nil: "Can't unquote-splice at the top level"
          n.assert not parent.isNil: "Can't unquote-splice at the top level"
          n.assert parent.kind == nkList: "Can't unquote-splice outside of a list"
          n.assert indexInParent in parent.kids.low .. parent.kids.high: "Invalid indexInParent"
          n.assert nkList == nkList: "Can't unquote-splice a non-list"

          n.tags.excl tkUnquoteSplicing

          let argToSplice = templateArgs[n.sym]
          # Don't evaluate, we're just supposed to modify the AST before we get to evaluating it.

          argToSplice.expectKind nkList

          # dump parent.repr
          # dump indexInParent
          # dump n.repr

          parent.kids.delete indexInParent # Removing this node. This somehow makes our `n: var Node` value nil *right after the statement executes*! (This is why we need to copy the kids for use below.)
          # parent.kids.insert kids, 0 # Replacing this node's position with it's own children.
          parent.kids.insert argToSplice.kids, indexInParent # Replacing this node's position with it's own children.

        elif n.kind == nkList:
          var i = n.kids.low
          while i <= n.kids.high: # The length can change during iteration because we insert insert quote-splicing exists.
            n.expectKind nkList
            update s, n.kids[i], templateArgs, n, i
            inc i

        else:
          # TODO: This is why lisp is normally made of only lists - it is very invonvenient to iterate and inspect all possible collections of nodes! (We should go back to using one list kind for everything.)
          discard

      var templateBody = f.templateBody.copyTree

      update s, templateBody, templateArgs, nil, -1

      result = s.evaluate templateBody

    of nkFunction:
      n.assert f.funcBody != nil
      n.assert f.funcReturnType != nil

      temporaryStack skFunction:
        try:
          s.stacks[^1].currentClassObj = f.funcObj # f.funcObj can be nil.

          n.assert args.len == f.funcParams.len:
            "Wrong number of arguments. Got " & $args.len & ", expected " &
              $f.funcParams.len

          for i, param in f.funcParams:
            let val = s.evaluate n.kids[i+1]
            let typ = f.funcTypes[i]

            if not typ.isNil:
              # TODO: maybe this part is missing an implicit type conversion (from args to params)
              if typ.kind != nkAnyType:
                n.assert val.getType == typ:
                  "Type mismatch for parameter " & param & ". Expected " & $f.funcTypes[
                    i] & ", got " & $val.getType

            s.stacks[^1].symbols[param] = val

          result = s.evaluate f.funcBody.copyTree
          # We ńeed to copy the tree because evaluation will change the function body's AST, which would persist between function calls (keeping local variables' values between calls)

          assert result != nil

        except Return as r:
          assert r.value != nil
          result = r.value

        assert result != nil
        result = result.convertValueToType(f.funcReturnType)
        assert result != nil

  of nkSymbol:
    result = s.evalSymbol(n)

  n.assert result != nil: "AAAAAAAAAAAA"

proc newState: State =
  result = State(stacks: @[Stack()])

# TODO: overloading on type and param count (test on vector operations)
# TODO: macros
# TODO: prolog
# TODO: math keyword
# TODO: case
# TODO: chess
# TODO: warcaby
# TODO: functions have access to variables calling them *always*!
# TODO: need int datatype for optimizaiton purposes (make float a separate kind)
# TODO: symbol binding (function calling itself must not call something else...) Solution: just remember the stack idx. The stack should be guaranteed by design to not have been removed.
# TODO: named arguments with keywords (:a 1 :b 2)

import std/[os]

type
  # TODO: make it a ref object, store compiletime data in tuples
  # TODO: convert string to cstring for efficiency with JS backend
  ScriptKind = enum testScript = "basic", exampleScript = "advanced"
  Script = object
    kind: ScriptKind
    name, code: string

proc collectScripts(dir: string): seq[Script] =
  collect:
    for f in walkDir(dir, checkDir = true, skipSpecial = true):
      if f.kind == pcFile:
        let name = f.path.splitFile.name
        let code = readFile f.path
        Script(name: name, code: code)

const
  scripts = block:
    var tests = collectScripts("src/tests")
    var examples = collectScripts("src/examples")
    for t in tests.mitems: t.kind = testScript
    for e in examples.mitems: e.kind = exampleScript
    (tests & examples).sortedByIt(it.name)

proc styled(s: string, color: string): string =
  "\x1b[" & color & "m" & s & "\x1b[0m"

proc runScript(name, code: string, modified: bool) =
  clearTerminal()
  try:
    if modified:
      print ("Runnning: " & name).styled("1;33") & " (modified)".styled("1;31")
    else:
      print ("Runnning: " & name).styled("1;33")
    let ast = code.tokenize.toSeq.buildAst
    discard newState().evaluate(ast)
    print "Script finished successfully.".styled("1;32")

  except Exception as e:
    print "Exception".styled("1;31;1")
    print e.msg.styled("1;31")
    print "Stack trace".styled("1;31;1")
    print e.getStackTrace
    raise

proc runTests =
  when not defined release:
    print "Running tests...".styled("1;34")

  for s in scripts:
    # if s.kind == testScript:
    print ("Testing: " & s.name).styled("1;35")
    let ast = s.code.tokenize.toSeq.buildAst
    discard newState().evaluate(ast)

  print "Tests finished.".styled("1;32")

proc main =
  print "Welcome to Dawid's toy language!".styled("1;33")
  runTests()
  when browser:
    {.emit: """
      editor = ace.edit("input");
      editor.session.setUseSoftTabs(true);
      editor.session.setUseWrapMode(true);
      editor.setHighlightActiveLine(true);
      editor.setTheme("ace/theme/solarized_dark");
      editor.session.setMode("ace/mode/clojure");
    """.}

when not browser:
  main()

when browser:
  include karax/prelude

  const
    firstScriptName = "core"

  var
    terminalInitialized = false
    currentScriptName = firstScriptName # TODO: use the Script object instead of mulitple globals!
                                        # currentScriptCode = cstring scripts.findIt(it.name == currentScriptName).code
    currentScriptModified = false

  proc resize =
    {.emit: """
      fitAddon.fit();
      editor.resize();
    """.}

  discard setInterval(resize, 100) # TODO: Karax doesn't call `fit` properly so this is a workaround.

  proc postRender =
    if not terminalInitialized:
      {.emit: """
        term.open(document.getElementById("terminal"));
        fitAddon.fit();
        if (editor) {
          editor.resize();
        }
        console.log("!!!")
      """.}
      setForeignNodeId "terminal"
      main()
      terminalInitialized = true

  proc renderer: VNode =
    buildHtml:
      body(class = "bg-black"):
        tdiv(class = "flex gap-1 h-screen p-3 gap-3"):
          tdiv(class = "flex-1 h-full flex flex-col gap-4"):
            tdiv(class = "flex gap-4"):

              select(id = "scripts-select", class = "w-full bg-white/10 text-white p-2 rounded-lg appearance-none"):
                for s in scripts:
                  option(value = s.name.cstring, selected = s.name == currentScriptName):
                    text s.name & " (" & $s.kind & ")"

                proc onChange(ev: Event, n: VNode) =
                  currentScriptName = $n.value # TODO: dumb string conversion
                  let script = scripts.findIt(it.name == currentScriptName)
                  # let input = getVNodeById("input")
                  let code = script.code.cstring
                  {.emit: """
                    editor.setValue(`code`);
                    editor.clearSelection();
                  """.}
                  # let newCode = script.code.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace(" ",
                  #     "&nbsp;").replace("\r\n", "<br>").replace("\n", "<br>") # TODO: extremely inefficient
                  # input.dom.innerHTML = cstring newCode # TODO: this might not work, better use something from standard library
                  # input.setInputText cstring script.code # Has to be done this way because karax sucks with value binding.
                  currentScriptModified = false

              button(class = "btn bg-blue-500 px-4 rounded-lg py-2"):
                text "Run"
                proc onClick =
                  # let input = getVNodeById("input")
                  var code: cstring
                  {.emit: """`code` = editor.getValue();""".}
                  # let code = $input.dom.innerText
                  dump code
                  runScript currentScriptName, $code, currentScriptModified # TODO: optimize string fuckery

            tdiv(class = "h-full w-full bg-black resize-none bg-white/10 text-xs rounded-lg", id = "input"):
              text scripts.findIt(it.name == firstScriptName).code.cstring

              proc onChange(ev: Event, n: VNode) =
                currentScriptModified = true

          tdiv(class = "h-full w-0.5 bg-white/10")
          tdiv(class = "flex-1 h-full max-w-1/2"):
            tdiv(id = "terminal", class = "h-full")
            # tdiv(id = "output", class = "h-full")

  setRenderer renderer, "ROOT", postRender
