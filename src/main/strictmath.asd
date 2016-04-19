(asdf:defsystem strictmath
 :name "Strict Math"
 :version "0.1"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :components ((:file "package")
              (:file "base")
              (:file "rem-pio2")
              (:file "kcos")
              (:file "ksin")
              (:file "sin")
              (:file "cos"))
 :serial t
 :depends-on (#-travis :ieee-floats)) ; Don't load libraries for travis
