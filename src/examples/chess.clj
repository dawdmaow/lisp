(block
    (vec2 := (object-type ((x number) (y number))))
    (Team := (enum-type "white" "black"))

    (PieceKind := (enum-type "pawn" "rook" "knight" "bishop" "queen" "king"))
    (piece := (object-type (
        (kind PieceKind)
        (pos vec2)
        (team Team)
        (did-pawn-move bool)
    )))

    (State := (object-type (
        (board (table-type vec2 piece))
        (current-team Team)
        (turn number)
    )))

    (new-state := (\ () State
        (current-team := "white")
        (turn := 0)
        (board := (table))
        (for y (1 .. 8)
            (for x (1 .. 8)
                (if (not (contains (quote (1 2 7 8)) x))
                    (continue)
                )
                (pos := (vec2 ((x x) (y y))))
                (kind := (case x
                    ((2 7) "pawn")
                    ((1 8)
                        (case y
                            (1 "rook")
                            (2 "knight")
                            (3 "bishop")
                            (4 "queen")
                            (5 "king")
                            (6 "bishop")
                            (7 "knight")
                            (8 "rook")
                        )
                    ))
                )
                (did-pawn-move := false)
                (team := (if ((x == 1) or (x == 2)) "white" "black"))
                (piece := (piece (
                    (kind kind)
                    (pos pos) 
                    (team team)
                    (did-pawn-move did-pawn-move)
                )))
                (table-set! board pos piece)
            )
        )
        (State ((board board) (current-team current-team) (turn turn)))
    ))

    (piece-kind-char := (\ ((kind PieceKind) (team Team)) string
        (case team
            ("white"
                (case kind
                    ("pawn" "♙")
                    ("rook" "♖")
                    ("knight" "♘")
                    ("bishop" "♗")
                    ("queen" "♕")
                    ("king" "♔")
                )
            )
            ("black"
                (case kind
                    ("pawn" "♟")
                    ("rook" "♜")
                    ("knight" "♞")
                    ("bishop" "♝")
                    ("queen" "♛")
                    ("king" "♚")
                )
            )
        )
    ))
 
    (state-as-str := (\ ((state State)) string
        (result := "")
        (for y (1 .. 8)
            (for x (1 .. 8)
                (pos := (vec2 ((x x) (y y))))
                (case (table-has-key state.board pos)
                    (true
                        (piece := (table-get state.board pos))
                        (str-add result (piece-kind-char piece.kind piece.team))
                    )
                    (false
                        (str-add result ".")
                    )
                )
                (str-add result " ")
            )
            (str-add result "\n") 
        )
        result
    ))

    (state := (new-state))

    (tmp := (state-as-str state))
    (echo tmp)
)