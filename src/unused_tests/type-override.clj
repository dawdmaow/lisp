;; TODO: Procs must be defined in their own namespace (not the same as variables) in order for this to work.

;; (:= foo (\ ((x number) (y string)) bool
;;     true
;; ))

;; (:= foo (\ ((x number) (y bool)) bool
;;     false
;; ))

;; (?= (foo 1 "a") true)
;; (?= (foo 1 true) false)