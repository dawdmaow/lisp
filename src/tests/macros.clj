(block
    (block
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

        (unless true ((y = true) (counter += 1)))
        (unless false ((x = true) (counter += 1)))

        (counter must-equal 1)
        (y must-equal false)
        (x must-equal true)
    )

    (block
        ;; (:= reverse-expr (macro (input)
        ;;     (:= result (quote ()))
        ;;     (:= i (high input))
        ;;     (while (>= i 0)
        ;;         (block
        ;;             (add result (reverse-expr (input i)))
        ;;             (i -= 1)
        ;;         )
        ;;     )
        ;; ))

        ;; TODO: need a macro magic that will automatically 1) quote input 2) evaluate output before returning it
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

        ((reverse-expr (quote (1 2 3 4 5 +))) must-equal 15)
        ((reverse-expr (quote (1 2 3 4 5 +))) must-equal 15)
    )
)