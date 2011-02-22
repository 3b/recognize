(asdf:defsystem :recognize
  :depends-on ("alexandria")
  :components ((:file "package")
               (:file "matrix")
               (:file "feature")
               (:file "feature-set")
               (:file "classifier"))
  :serial t)