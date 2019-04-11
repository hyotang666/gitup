(defpackage :gitup
  (:use :cl :cl-github)
  (:export #:new))
(in-package :gitup)

(defun new (system)
  (setf system(asdf:find-system system)) ; as canonicalize.
  (when (clean-repository-p system)
    (multiple-value-bind(*username* *password*)(get-secret-info)
      (let*((initp)
	    (repository(or (find-repository system)
			   (setf initp (create system)))))
	(upload system repository initp)))))

(defun get-secret-info()
  (let((path #.(merge-pathnames "secret-info"
				(asdf:system-source-directory(asdf:find-system :gitup))))
       (key(ensure-key)))
    (if (probe-file path)
      (with-open-file(in path)
	(flet((get!(s)
		(crypto-shortcuts:decrypt (read s)
					  key)))
	  (values (get! in)(get! in))))
      (let((username(prompt-for:prompt-for 'string "username>> " :by #'read-line))
	   (password(prompt-for:prompt-for :secret "password>> ")))
	(with-open-file(out path :direction :output :if-does-not-exist :create)
	  (flet((put(value)
		  (print(crypto-shortcuts:encrypt value key)out)))
	    (put username)
	    (put password)))
	(values username password)))))

(defun ensure-key(&optional force)
  (let((path #.(merge-pathnames ".gitup"
				(asdf:system-source-directory(asdf:find-system :gitup)))))
    (if(and (probe-file path)
	    (not force))
      (with-open-file(s path)
	(values(read-line s)))
      (with-open-file(s path :direction :output :if-does-not-exist :create
			:if-exists :supersede)
	(let((key(random-string 16)))
	  (write-line key s)
	  key)))))

(defun random-string(length)
  (loop :with count = length
	:for char = (code-char(random 128))
	:when (alphanumericp char)
	:collect char :into temp :and :do (decf count)
	:when (zerop count) :do (return(coerce temp 'string))))

(defmacro with-system-source-directory((system)&body body)
  `(UIOP:WITH-CURRENT-DIRECTORY((ASDF:SYSTEM-SOURCE-DIRECTORY ,system))
     ,@body))

(defun find-repository(system)
  (find (asdf:coerce-name system)
	(list-repositories)
	:key (lambda(repo)
	       (getf repo :name))
	:test #'string=))

(defun create(system)
  (create-repository :name (asdf:coerce-name system)
		     :description (asdf:system-description system)
		     :public t
		     :has-wiki t))

(defun clean-repository-p(system)
  (let((string(with-system-source-directory(system)
		(uiop:run-program "git status | tail -1"
				  :output :string))))
    (or (eql 37 (search "clean" string :from-end t))
	(y-or-n-p "Not clean repository.~%~A~%Are you ok?"string))))

(defun upload(system repository initp)
  (with-system-source-directory(system)
    (when initp
      (uiop:run-program (concatenate 'string
				     "git remote add origin "
				     (getf repository :ssh-url))
			:output t))
    (uiop:run-program "git push -u origin master" :output t)))
