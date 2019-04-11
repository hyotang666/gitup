; vim: ft=lisp et
(in-package :asdf)
(defsystem :gitup.test
  :depends-on
  (:jingoh "gitup")
  :components
  ((:file "gitup"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :gitup)))