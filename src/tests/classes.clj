;; This script tests the class system of the language.
;; It creates a base class Animal and a derived class Dog.
;; It also creates a derived class DumbDog that inherits from Dog.

(was-dog := false) ;; Global variable to track if the last animal was a dog


(class Animal void ;; Base class
    (field id number) ;; Field of type number

    (method speak (\ () void ;; Method with no arguments and no return value
        (echo "I am an animal")
        (was-dog = false) 
    ))
)

(class Dog Animal ;; Derived class
    (field name string)

    (method speak (\ () void
        (echo "I am a dog")
        (echo "My name is:")
        (echo @name)
        (was-dog = true)
    ))
)

(class DumbDog Dog) ;; Derived class from Dog

(a := (Animal (id 1))) ;; Create an instance of Animal (variable of type Animal)
(d := (Dog (name "Burek") (id 2))) ;; Create an instance of Dog (variable of type Dog)

(a.id must-equal 1)
(d.id must-equal 2)

(a.speak) ;; Call the speak method of Animal
(was-dog must-equal false)
(d.speak) ;; Call the speak method of Dog
(was-dog must-equal true)
 
(a = d) ;; Assign the instance of Dog to the variable a, changing it's type to Animal

(a.id must-equal 2)

(was-dog = false)
(a.speak)
(was-dog must-equal true)

(was-dog = false)
(dd := (DumbDog (name "Maks") (id 3))) ;; Create an instance of DumbDog (variable of type DumbDog)
(dd.speak)
(was-dog must-equal true)