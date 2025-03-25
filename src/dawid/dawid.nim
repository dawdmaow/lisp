# TODO: dumpify
# TODO: dump + file/line/column info
# TODO: assert that uses dumpify/dump

import deepcopy
export deepcopy

when not defined js:
  import dumps
  export dumps

proc shallowCopy*[T](x: ref T): ref T =
  new result
  result[] = x[]

template findIt*(xs, cond): untyped =
  var result: typeof(xs.items, typeOfIter)
  block outer:
    for it {.inject.} in xs:
      if cond:
        result = it
        break outer
    raiseAssert "findIt: not found"
  result
