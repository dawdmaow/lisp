;; Macros tests
;; This script tests the macro system of the language.

(block
    (block
        ;; Template that runs `body` if `cond` is false (inverse of `if`)
        ;; ^unqote is used to unquote the body ("inlining" it in the code *without* evaluation!)
        ;; ^unquote-splicing is used to unquote the body ("inlining" it in the code *with* evaluation!), but it also unpacks all the elements such that they belong to the parent node (in this case, `block`).
        (unless := (template (cond body)
            (if (not ^unquote cond)
                (block
                    ^unquote-splicing body
                )
            )
        ))

        (x := false)
        (y := false)
        (counter := 0)

        (unless true ((y = true) (counter += 1))) ;; should not run
        (unless false ((x = true) (counter += 1))) ;; should run

        (counter must-equal 1)
        (y must-equal false)
        (x must-equal true)
    )

    (block
        ;; TODO: need a macro magic that will automatically 1) quote input 2) evaluate output before returning it

        ;; This is a macro that reverses the `input` expression nodes (not recursive, depth=1) and evaluates the expression.
        (reverse-expr := (\ (input) any
            (helper := (\ (input) any
                (if (not (is-list input))
                    (return input)
                )

                (result := (quote ()))
                ;; (echo result)
                (i := (high input))
                (while (i >= 0)
                    (add result (helper (input i)))
                    (i -= 1)
                )
                result
            ))

            (result := (helper input))

            (eval result)
        ))

        ((reverse-expr (quote (1 2 3 4 5 +))) must-equal 15) ;; This works because if we reverse the arguments, + is the first element, making the expression a function call.
    )
)