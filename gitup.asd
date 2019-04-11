; vim: ft=lisp et
(in-package :asdf)
(defsystem "gitup"
  :depends-on
  (
   "cl-github-v3" ; github api.
   "prompt-for" ; type safe user input.
   "crypto-shortcuts" ; cryptography.
   )
  :pathname
  "src/"
  :components
  ((:file "gitup")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "gitup"))))
  (append (call-next-method) '((test-op "gitup.test"))))
