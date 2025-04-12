(block
    (vec2 := (object-type ((x number) (y number)))) ;; 2D vector type
    (Team := (enum-type "white" "black")) ;; Team enum type

    (PieceKind := (enum-type "pawn" "rook" "knight" "bishop" "queen" "king")) 
    
    ;; Type for a chess piece
    (piece := (object-type ( 
        (kind PieceKind)
        (pos vec2)
        (team Team)
        (did-pawn-move bool)
    )))

    ;; Type for a chess board
    (State := (object-type (
        (board (table-type vec2 piece))
        (current-team Team)
        (turn number)
    )))

    ;; Function to create a new chess piece
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

    ;; Function to translate a piece to a character
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
 
    ;; Function to convert the state to a string representation
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

    (state := (new-state)) ;; Create a new chess state
    (tmp := (state-as-str state)) ;; Convert the state to a string representation
    (echo tmp) ;; Print the string representation of the state
)