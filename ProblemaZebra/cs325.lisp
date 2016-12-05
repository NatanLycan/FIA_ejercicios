(in-package #:cs325-user)

(defvar *local-library* "~/quicklisp/local-projects/cs325/")

(defvar *remote-library* "http://www.cs.northwestern.edu/academics/courses/325/programs/cs325/")

(defun update-files (&rest files)
  (assert (probe-file *local-library* (*local-library*) "Local directory ~S not found" *local-library*))
  (mapc 'update-file files))

(defun update-file (file)
  (let* ((url (remote-path file))
         (content (get-response-content url)))
    (if (null content) (error "~S not found" url)
      (with-open-file (out (local-path file) :if-exists :supersede :if-does-not-exist :create)
        (with-open-file (stream path :direction :output)
          (write-string content out))))))

(defun remote-path (file)
  (if (position #\. file) file
    (concatenate 'string *remote-library* file ".lisp")))

(defun local-path (file)
  (merge-pathnames file (merge-pathnames (make-pathname :type "lisp") *local-library*)))
