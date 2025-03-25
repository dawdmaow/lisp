# Package

version       = "0.1.0"
author        = "Dawid Kotliński"
description   = "A new awesome nimble package"
license       = "Proprietary"
srcDir        = "src"
bin           = @["lisp"]
binDir        = "out"
# backend       = "js" 


# Dependencies 

requires "nim >= 2.2.0", "karax"

# after build:
#   mvFile "out/lisp", "out/lisp.js"