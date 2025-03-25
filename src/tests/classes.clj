(was-dog := false)

(class Animal void
    (field id number)

    (method speak (\ () void
        (echo "I am an animal")
        (was-dog = false)
    ))
)

(class Dog Animal
    (field name string)

    (method speak (\ () void
        (echo "I am a dog")
        (echo "My name is:")
        (echo @name)
        (was-dog = true)
    ))
)

(class DumbDog Dog)

(a := (Animal (id 1)))
(d := (Dog (name "Burek") (id 2)))

(a.id must-equal 1)
(d.id must-equal 2)

(a.speak)
(was-dog must-equal false)
(d.speak)
(was-dog must-equal true)
 
(a = d)

(a.id must-equal 2)

(was-dog = false)
(a.speak)
(was-dog must-equal true)

(was-dog = false)
(dd := (DumbDog (name "Maks") (id 3)))
(dd.speak)
(was-dog must-equal true)