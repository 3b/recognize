(asdf:defsystem :recognize-demo
  :depends-on ("recognize" "glop" "cl-opengl")
  :components ((:module "demo"
                        :serial t
                        :components
                        ((:file "demo"))))
  :serial t)