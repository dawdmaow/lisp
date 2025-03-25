# Custom implentation of deepCopy.

proc copyAll*[T: object](result: var T, x: T) =
  result.reset
  result = x

proc copyAll*[T: ref](result: var T, x: T) =
  if result == nil: new result
  copyAll result[], x[]
