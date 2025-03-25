(block
    (block
        (vec2 := (object-type ((x number) (y number))))
        (v := (vec2 ((x 10) (y 20))))
        (v.x must-equal 10)
        (v.y must-equal 20)
        (person := (object-type ((name string) (pos vec2))))
        (p := (person ((name "John") (pos v))))
        (p.name must-equal "John")
        (p.pos.x must-equal 10)
        (p.pos.y must-equal 20)
        (p.pos must-equal (vec2 ((x 10) (y 20))))
    )

    (block
        (piece-kind := (enum-type "pawn" "rook" "knight" "bishop" "queen" "king"))
        (x := (piece-kind "pawn"))
        (x must-equal "pawn")
        (did-raise := false)
        
        (block
            (y := (piece-kind "rook"))
            (y must-equal "rook")
            (y = "queen")
            (y must-equal "queen")
        )
        
        (try
            (x = "xyz")
            (except
                (did-raise = true)
            )
            (finally
                (x = "knight")
            )
        )
        (x must-equal "knight")
        (did-raise must-equal true)
    )

    (block
        (b := 2)
        ((1 + b) must-equal 3)
        (block
            (b := 10)
            ((1 + b) must-equal 11)
        )
        ((1 + b) must-equal 3)
    )

    (block
        (cond := false)
        ((if cond "prawda" "falsz") must-equal "falsz")
    )

    (block
        (f := (\ (a b) any (a + b)))
        ((f 10 20) must-equal 30)
        (((table ("a" "A") ("b" "B") ("c" "C")) "b") must-equal "B")
    )

    (block
        (f := (\ ((a number) (b number)) number (a + b)))
        ((f 100 200) must-equal 300)
    )

    (block
        (x := false)
        (y := false)
        (f := (\ () void (x = true) (y = true))) ;; Function with multiple statements without an explicit block.
        (f)
        (x must-equal true)
        (y must-equal true)
    )
)