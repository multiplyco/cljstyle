;; Default alignment rules for cljstyle
;;
;; This file defines which forms should have their binding vectors aligned when
;; the :alignment :forms? option is enabled. Each entry maps a form symbol to a
;; set of child indices that should be aligned.
;;
;; For example: 'let #{0} means the first child of a let form (the binding
;; vector) should have its values aligned.

{let #{0}
 doseq #{0}
 for #{0}
 binding #{0}
 loop #{0}
 with-open #{0}
 with-redefs #{0}
 with-local-vars #{0}}
