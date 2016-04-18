(asdf:defsystem strictmath
 :name "Strict Math"
 :version "0.1"
 :maintainer "Frank Duncan (frank@kank.com)"
 :author "Frank Duncan (frank@kank.com)"
 :components ((:file "package") (:file "base"))
 :serial t
 :depends-on (#-travis :ieee-floats)) ; Don't load libraries for travis
