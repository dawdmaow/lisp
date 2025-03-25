import tables, strutils, strformat, sequtils, typetraits, json, std/jsonutils, sugar, terminal

# TODO: Intermediate representation should be a string of lisp format.

type
  NodeKind = enum table, scalar
  Node = ref object
    case kind: NodeKind
    of table:
      tableType: string
      tableRows: seq[tuple[key: string, value: Node]]
    of scalar:
      scalarRepr: string
      scalarFg: ForegroundColor

proc center(text: string, width: int): string =
  var padding = width - text.len
  if padding < 0: padding = 0
  let leftPadding = padding div 2
  let rightPadding = padding - leftPadding
  return " ".repeat(leftPadding) & text & " ".repeat(rightPadding)

proc left(text: string, width: int): string =
  var padding = width - text.len
  if padding < 0: padding = 0
  return text & " ".repeat(padding)

proc right(text: string, width: int): string =
  var padding = width - text.len
  if padding < 0: padding = 0
  return " ".repeat(padding) & text

# Format similiat to that of "Dumpify" library from C#.
# EXAMPLE:
#                   Person
# +-------------+----------------------------------+
# | firstName   | "Moaid"                          |
# | lastName    | "Ibrahim"                        |
# | spouse      |              Person              |
# |             +-------------+--------------------+
# |             | firstName   | "Sara"             |
# |             | lastName    | "Ibrahim"          |
# |             | spouse      | *circular*         |
# |             | profession  | "Engineer"         |
# |             +-------------+--------------------+
# | profession  | "Doctor"                         |
# +-------------+----------------------------------+
#[
TODO: Let's switch to a buffer solution where we calculate in advance how many rows we will need.
Then just preallocate the entire buffer (width is constant) and fill the cells using x, y coords.
]#

proc toUnicodeTable(root: Node, totalWidth = 60, isMain = true): string =
  assert root.kind == table, root.repr

  template `->`(x: string) {.inject.} = result.add x
  template nl = result.add '\n'

  template `->`(s: Style) =
    -> ansiStyleCode(s.int)

  template `->`(c: ForegroundColor) =
    -> ansiForegroundColorCode(c, bright = false)

  template styled(code) =
    code
    -> ansiStyleCode(resetStyle.int)

  # template line(code) = (code; result.add '\n')

  # let innerWidth = totalWidth - 4

  let col1len = max(9, root.tableRows.mapIt(it.key.len).max)
  let col2len = totalWidth - col1len

  const vertical = "─"
  const horizontal = "│"
  const topLeft = "┌"
  const topRight = "┐"
  const bottomLeft = "└"
  const bottomRight = "┘"
  # const connectDown = "┳"
  # const connectUp = "┻"
  # const connectAll = "╋"

  if isMain:
    styled:
      # -> styleBright
      -> center(root.tableType, totalWidth)
    nl

  styled:
    -> fgBlue
    -> topLeft
    -> vertical.repeat(totalWidth - 2)
    -> topRight
    nl

  for (name, kid) in root.tableRows:
    styled:
      -> fgBlue
      -> horizontal
    -> " "
    styled:
      -> fgBlue
      -> name.left(col1len)
    -> " "
    styled:
      -> fgBlue
      -> horizontal
    -> " "
    case kid.kind
    of scalar:
      # let len = col2len - kid.scalarRepr.len
      # -> kid.scalarRepr.left(len)
      styled:
        -> kid.scalarFg
        -> kid.scalarRepr
      -> " ".repeat(col2len - kid.scalarRepr.len - 7)
      -> " "
      styled:
        -> fgBlue
        -> horizontal
      nl
    of table:
      # -> "..."
      # nl
      let len = col2len - 7
      styled:
        # -> styleBright
        -> kid.tableType.center(len)
      -> " "
      styled:
        -> fgBlue
        -> horizontal
      nl
      for l in kid.toUnicodeTable(totalWidth = len, isMain = false).splitLines[0..^2]:
        styled:
          -> fgBlue
          -> horizontal
        -> " "
        -> " ".repeat(col1len)
        -> " "
        styled:
          -> fgBlue
          -> horizontal
        -> " "
        -> l
        -> " "
        styled:
          -> fgBlue
          -> horizontal
        nl

  styled:
    -> fgBlue
    -> bottomLeft
    -> vertical.repeat(totalWidth - 2)
    -> bottomRight
    nl

    #[
    function convertNodeToTable(node, indent = 0, visited = new Set()):
      if node == null:
        return ""

      if visited.has(node):
        return "*circular*"

      visited.add(node)

      // First pass: calculate column widths
      let keyWidth = 0
      let valueWidth = 0
      let lines = []

      // Get node name or empty string if it's a nested node
      let nodeName = indent == 0 ? node.name : ""

      // Calculate the title line width if this is the root
      if indent == 0:
        keyWidth = max(keyWidth, length(nodeName))

      // Calculate widths from properties
      for key, value in node.properties:
        keyWidth = max(keyWidth, length(key))
        if value is Node:
          // Recursive call for nested node
          let nestedTable = convertNodeToTable(value, indent + 1, visited)
          let nestedLines = nestedTable.split('\n')
          valueWidth = max(valueWidth, maxLineLength(nestedLines))
        else:
          valueWidth = max(valueWidth, length(value) + 2)  // +2 for quotes

        // Build the table
        let result = []

        // Add title if this is the root node
        if indent == 0 and nodeName:
          result.append(centerText(nodeName, keyWidth + valueWidth + 3))  // +3 for borders

        // Add top border
        result.append(createBorder(keyWidth, valueWidth, indent))

        // Add properties
        for key, value in node.properties:
          if value is Node:
            // Handle nested node
            let nestedTable = convertNodeToTable(value, indent + 1, visited.clone())
            let nestedLines = nestedTable.split('\n')

            // Add the key with the nested node type
            result.append(formatRow(key, centerText(value.type, valueWidth), keyWidth))

            // Add nested table lines with proper indentation
            for line in nestedLines:
              result.append(formatNestedRow(line, keyWidth, valueWidth))
            else:
              // Handle simple value
              result.append(formatRow(key, `"${value}"`, keyWidth))

            // Add bottom border
            result.append(createBorder(keyWidth, valueWidth, indent))

            return join(result, '\n')
          ]#

        # proc toUnicodeTable(node: Node, indent = 0, width = 0): string =
        #   assert node.kind == table

        #   template `++`(x: string) =
        #     result.add x

        #   template line(code) =
        #     code
        #     result.add '\n'

        #   var width = width
        #   if width <= 0:
        #     width = 60

        #   assert node.tableType != ""
        #   let keysMaxLen = max(9, node.tableRows.mapIt(it.key.len).max)
        #   line:
        #     ++ node.tableType.center(width)
        #   line:
        #     ++ "X".repeat(width)
        #   line:
        #     ++ "X "
        #     ++ "Name".left(keysMaxLen)
        #     ++ " X "
        #     ++ "Value".left(width - keysMaxLen - 6)
        #     ++ "X"
        #   line:
        #     ++ "X".repeat(width)
        #   for (key, node) in node.tableRows:
        #     line:
        #       ++ "X "
        #       ++ key.left(keysMaxLen)
        #       ++ " X "
        #       case node.kind
        #       of scalar:
        #         ++ node.scalarRepr
        #       of table:
        #         var first = true
        #         for l in toUnicodeTable(node, indent = indent + 1, width = 30).left(width - keysMaxLen - 6).splitLines:
        #           line:
        #             if first:
        #               first = false
        #               ++ l
        #             else:
        #               ++ "X "
        #               ++ l.right(width - 2)
        #       ++ "X"

# Recursive function to render an object as a Unicode table.
# Must support printing inner values as tables as well.
# Every collection must be turned into a unicode table - most importantly objects and sequences.
proc toNode[T](x: T, indent = 0): Node =
  when T is object:
    result = Node(kind: table, tableType: $T)
    for k, v in x.fieldPairs:
      result.tableRows.add (k, v.toNode(indent + 1))
  elif T is ref object:
    result = Node(kind: table, tableType: $T)
    for k, v in x[].fieldPairs:
      result.tableRows.add (k, v.toNode(indent + 1))
  elif T is string:
    result = Node(kind: scalar, scalarRepr: '"' & x & '"', scalarFg: fgYellow)
  else:
    when compiles(for _, _ in x: discard):
      result = Node(kind: table, tableType: $T)
      for i, v in x:
        result.tableRows.add ($i, v.toNode(indent + 1))
    else:
      let fg =
        when T is SomeNumber:
          fgGreen
        elif T is enum:
          fgRed
        else:
          fgCyan
      result = Node(kind: scalar, scalarRepr: $x, scalarFg: fg)

template dumpTable*(x) =
  echo x.toNode.toUnicodeTable

# type
#   Vec = ref object
#     x, y: float
#   World = ref object
#     name = "My World"
#     # playerPos = Vec(x: 1, y: 2)
#     enemies = @[Vec(x: 1, y: 2), Vec(x: 3, y: 4)]

# let w = World()
# echo "----------------- 1"
# echo toJson(w).pretty
# echo "----------------- 2"
# echo toJson(toNode(w)).pretty
# echo "----------------- 3"
# echo w.toNode.toUnicodeTable
# echo "----------------- 4"
