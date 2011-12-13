(asdf:defsystem :recognize
  :description "Common Lisp implementation of single pointer gesture recognition from Rubine's 'The Automatic Recognition of Gestures'."
  :depends-on ("alexandria")
  :components ((:file "package")
               (:file "matrix")
               (:file "feature")
               (:file "feature-set")
               (:file "classifier"))
  :serial t)