;; Core tests for the language.

(block 
    (block
        (vec2 := (object-type ((x number) (y number)))) ;; Define a type with two fields: x and y.
        (v := (vec2 ((x 10) (y 20)))) ;; Create an instance of vec2 with x=10 and y=20.
        (v.x must-equal 10) ;; `must-equal` is a binary operator.
        (v.y must-equal 20)
        (person := (object-type ((name string) (pos vec2)))) ;; Define a type with a string field and a vec2 field.
        (p := (person ((name "John") (pos v)))) ;; Create an instance of person with name "John" and position v.
        (p.name must-equal "John")
        (p.pos.x must-equal 10)
        (p.pos.y must-equal 20)
        (p.pos must-equal (vec2 ((x 10) (y 20))))
    )

    (block
        (piece-kind := (enum-type "pawn" "rook" "knight" "bishop" "queen" "king")) ;; Define an enumeration type for chess pieces.
        (x := (piece-kind "pawn")) ;; Create an instance of piece-kind with value "pawn".
        (x must-equal "pawn")
        (did-raise := false) ;; Variable (will go out of scope at the end of the parent `block`).
        
        (block
            (y := (piece-kind "rook"))  
            (y must-equal "rook")
            (y = "queen") ;; Assign a new value to y. (The new value can't change the variable's type, for safety reasons.)
            (y must-equal "queen")
        )
        
        ;; Error catching test.
        (try
            (x = "xyz") ;; This is invalid because "xyz" is a string and `x` is a variable of type `piece-kind`.
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
        ((1 + b) must-equal 3) ;; `+` is a binary operator.
        (block
            (b := 10) ;; This `b` shadows the outer `b`.
            ((1 + b) must-equal 11)
        )
        ((1 + b) must-equal 3) ;; This `b` is the outer `b`.
    )

    (block
        (cond := false)
        ((if cond "prawda" "falsz") must-equal "falsz") ;; `if` is a ternary operator.
    )

    (block
        ;; Define a function `f` that takes two arguments and returns their sum.
        ;; `any` is a special pseudo-type that avoid type checking the value returned by the function.
        ;; `a` and `b` are implicitly untyped, look below to see typed function parameters.
        (f := (\ (a b) any (a + b))) 

        ((f 10 20) must-equal 30) ;; We call `f`.

        ;; We create a new table datatype with some key-value pairs and get the value of key "b".
        (((table ("a" "A") ("b" "B") ("c" "C")) "b") must-equal "B")
    )

    (block
        ;; Define a function `f` that takes two arguments of type number and returns their sum.
        ;; The types are checked at runtime.
        (f := (\ ((a number) (b number)) number (a + b)))
        ((f 100 200) must-equal 300)
    )

    (block
        (x := false)
        (y := false)
        (f := (\ () void (x = true) (y = true))) ;; Function with multiple statements without an explicit containing `block`.
        (f) ;; Call the function `f`.
        (x must-equal true)
        (y must-equal true)
    )
)