;; HTML builder function (translated AST nodes tree to a HTML string).

(block
    ;; Tags that shouldn't be closed.
    (self-closing-tags := (quote (
        "area" "base" "br" "col" "embed" "hr" "img" "input" "keygen" "link" "menuitem" "meta" "param" "source" "track" "wbr"
    )))

    ;; This is the functino.
    (build-html := (\ (tree) any
        ;; Build HTML from a tree made of nested lists.
        
        (result := "")
        ;; HTML output is appended to this variable.

        (indent := (- 1))
        ;; We keep indentation for pretty formatting.

        (helper := (\ (l) any
            ;; This function recursively travels the tree.
            ;; This function won't be accessible from the outside of the build-html function.

            (for _ (0 .. indent)
                (str-add result "  ") ;; For pretty output.
            )
            ;; Padding for nested tags

            (expect (is-list l) true)
            (expect (len l) 3)
            ;; Every tag is a list with name, attributes and body.

            (tag := (to-str (l 0))) ;; First element is tag.
            (attributes := (l 1)) ;; Second element is attributes.
            (body := (l 2)) ;; Body is the last element.

            (str-add result "<" tag) ;; Opening the tag.

            (if (not (empty? attributes))
                (str-add result " ") 
            )
            ;; Apply HTML attributes.
            (for (idx el) attributes
                (expect (is-list el) true)
                (expect (len el) 2)
                
                (field-name := (el 0))
                (field-val := (el 1))
                
                (str-add result
                (to-str field-name)
                "=\""
                (to-str field-val)
                "\""
                )
                (if (!= idx (high attributes))
                    (str-add result " ")
                )
            ) 

            (str-add result ">") ;; Closing the bracket pair.

            (if (is-list body) ;; If body is a list, then it's a nested tag.
                (if (not (empty? body))
                    (block
                        (str-add result "\n")
                        
                        (for el body
                            (indent += 1)

                            (helper el) ;; Recursive call!
                            (str-add result "\n")

                            (indent -= 1)
                        )
                    )
                )
                (block
                    (expect (is-str body) true)  ;; Expecting text content for the tag.

                    (str-add result "\n")
                    (for _ (0 .. (indent + 1))
                        (str-add result "  ")
                    )
                    (str-add result body)
                    (str-add result "\n")
                )
            )

            (if (not (contains self-closing-tags tag)) ;; Close the opened tag (if it's not self-closing).
                (block
                    (for _ (0 .. indent)
                        (str-add result "  ")
                    )
                    (str-add result "</")
                    (str-add result tag)
                    (str-add result ">")
                )

                (block
                    (expect (empty? body) true) ;; If it's a self-closing tag, then it can't have a body anyway.
                )
            )
        ))

        (helper tree)
        
        result ;; Implicit return.
    ))

    (html :=
      (build-html
        (quote
          ("div" (("css" "bg-red-500 w-full") ("style" "opacity: 0.5")) (
            ("input" (("type" "text") ("name" "username")) ())
            ("input" (("type" "password") ("name" "password")) ())
            ("div" () (
              ("div" () "text")
            ))
          ))
        )
      )
    )

    (expected-html := "<div css=\"bg-red-500 w-full\" style=\"opacity: 0.5\">\n  <input type=\"text\" name=\"username\">\n  <input type=\"password\" name=\"password\">\n  <div>\n    <div>\n      text\n    </div>\n  </div>\n</div>")

    (html must-equal expected-html)
)