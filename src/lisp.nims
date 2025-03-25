--backend: js

# when nimvm:
#   --backend: js

# when defined(js):
#   --"out": "out/lisp.js"

# # TODO: optimized version when release (-O1 etc.) (Mayben nim does this by default already?) (closure compiler flag in emcc)

# when not defined(testing):
#   --d: emscripten # For nimsuggest to show proper compilation errors.

# when defined(emscripten):
#   # This path will only run if -d:emscripten is passed to nim.

#   # --listCmd
#   discard

#   # --backend: cpp # Infinite loop in testing (stack call limit) (also cpp doesn't help with anything)

#   # --d: release # Without this there is a stack trace limit error (probably not due to infinite recursion but using too large locals in non-release build???)

#   --nimcache: "../.emscripten_cache"

#   --os: linux # Emscripten pretends to be linux.
#   --cpu: wasm32 # Emscripten is 32bits.
#   --cc: clang # Emscripten is very close to clang, so we ill replace it.

#   --clang.exe: emcc # Replace C
#   --clang.linkerexe: emcc # Replace C linker
#   --clang.cpp.exe: emcc # Replace C++
#   --clang.cpp.linkerexe: emcc # Replace C++ linker.

#   # --listCmd # List what commands we are running so that we can debug them.

#   --gc: orc # GC:orc is friendlier with crazy platforms.

#   --exceptions: goto # Goto exceptions are friendlier with crazy platforms.
#   --backend: c

#   # --exceptions: cpp # Goto exceptions are friendlier with crazy platforms.
#   # --backend: cpp

#   # --define: noSignalHandler # Emscripten doesn't support signal handlers.

#   --define: useMalloc
#   --opt: size
#   --threads: off

#   # -sEXPORTED_FUNCTIONS=_main,_evalCode -sINITIAL_HEAP=16777216

#   template pass(s: string) =
#     switch "passC": s
#     switch "passL": s

#   pass "-sEXPORTED_FUNCTIONS=_main,_evalCode"
#   pass "-sASSERTIONS=1"
#   pass "-sEXPORTED_RUNTIME_METHODS=ccall,cwrap"
#   pass "-sFORCE_FILESYSTEM=1"
#   # pass "-sASYNCIFY" # This creates calls tack size exceeded crashes.
#   # pass "-sASYNCIFY_STACK_SIZE=800000000"
#   # pass "-sSTACK_OVERFLOW_CHECK=2"

#   pass "-sNO_DISABLE_EXCEPTION_CATCHING=1"

#   pass "-gsource-map"

#   # pass "-sSTACK_SIZE=800000000" # TODO: this is an insane size, but the program did run out of stack space with lower numbers.
#   # pass "-sSTACK_SIZE=512000" # TODO: this is an insane size, but the program did run out of stack space with lower numbers.
#   pass "-sSTACK_SIZE=8000000" # TODO: this is an insane size, but the program did run out of stack space with lower numbers.

#   # --stackTrace: on
#   # --embedsrc: on

#   --passL: "-o out/index.html"
#   pass "-sUSE_WEBGL2=1"
#   # --passL: "-sSTACK_SIZE=512000"
#   # --passL: "-sSTACK_SIZE=10000000" # TODO: this is an insane size, but the program did run out of stack space with lower numbers.
#   # --passL: "-sSTACK_SIZE=1000000000" # TODO: this is an insane size, but the program did run out of stack space with lower numbers.
#   # --passL: "-sSTACK_SIZE=16" # TODO: this is an insane size, but the program did run out of stack space with lower numbers.
#   --passL: "--shell-file shell_new.html"
#   # --passL: "-sEXIT_RUNTIME=1"
#   # --passL: "-sEXIT_RUNTIME=0"

#   # --passL: "-o out/index.html -sUSE_WEBGL2=1 -sSTACK_SIZE=512000 --shell-file shell_new.html -sEXIT_RUNTIME=1"
#   # --passL: "-o out/test123.js -sUSE_WEBGL2=1 -sSTACK_SIZE=512000 -sEXIT_RUNTIME=1  -fsanitize=undefined -fsanitize=null"
# # else:
# #   --backend: js
# #   --d: js
