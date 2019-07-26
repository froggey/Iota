(asdf:defsystem #:iota
  :description "LLVM to CL transpiler runtime"
  :version "1.0"
  :author "Henry Harrington <henry.harrington@gmail.com>"
  :license "MIT"
  :depends-on ("uiop" "babel" "nibbles" #-mezzano "lispbuilder-sdl" "alexandria")
  :serial t
  :components ((:module "runtime"
                :components
                ((:file "package")
                 (:file "llvm-runtime")
                 #+mezzano
                 (:file "mezzano-graphics")
                 #-mezzano
                 (:file "sdl-graphics")))))
