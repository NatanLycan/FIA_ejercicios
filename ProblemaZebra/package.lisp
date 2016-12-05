
;;; 09-17-14 dropped clp-exts [CKR]
;;; 09-05-14 Created file [CKR]

(defpackage #:mops
  (:use #:common-lisp)
  (:export #:defmop #:definstance #:mop-p #:instance-p
           #:add-frame #:add-instance #:find-instances 
           #:->frame #:root-frames #:clear-memory
           #:absts-of #:specs-of #:all-absts-of 
           #:inherit-filler #:role-filler #:<- #:abstp
           #:show-frame #:show-memory
           #:slots-of #:slot-role #:slot-filler)
  )

(defpackage #:extend-match
  (:use #:common-lisp)
  (:export #:pat-match #:bind-variable #:add-extension #:instantiate-pattern #:match-and))

(defpackage #:lisp-unit
  (:use #:common-lisp)
  (:export #:define-test #:run-all-tests #:run-tests
           #:assert-eq #:assert-eql #:assert-equal #:assert-equalp
           #:assert-error #:assert-expands #:assert-false 
           #:assert-equality #:assert-prints #:assert-true
           #:fail
           #:get-test-code #:get-tests
           #:remove-all-tests #:remove-tests
           #:logically-equal #:set-equal #:unordered-equal
           #:use-debugger
           #:with-test-listener)
  )

(defpackage #:tables
  (:use #:common-lisp)
  (:export #:clear-table #:deftable #:in-table-p #:map-table #:remove-key)
  )

(defpackage #:ddr
  (:use #:common-lisp #:tables)
  (:export #:ask #:ask-trace #:find-blists #:init-kb #:show-trace #:tell 
           #:bind 
           #:replace-variables #:unify #:var-p #:var-name #:var-bound-p 
           #:var-value #:make-bindings
           #:define-retrieval-method #:define-storage-method
           #:pattern-args #:pattern-head)
  )

(cl:defpackage #:lisp-critic
  (:use  #:common-lisp #:tables #:extend-match)
  (:export #:critique #:critique-file #:critique-definition
           #:apply-critique-rule
           #:lisp-critic-version
           #:add-lisp-pattern #:define-lisp-pattern #:remove-lisp-pattern
           #:get-pattern #:get-response #:get-pattern-names
           #:response-args #:response-format-string
           #:clear-critique-db)
  )

(defpackage #:ddr-tests
  (:use #:common-lisp #:lisp-unit #:ddr)
  )
  
(defpackage #:shakey-tests
  (:use #:common-lisp #:lisp-unit #:ddr)
  )
  
(defpackage #:cs325-user
  (:use #:common-lisp #:lisp-unit #:lisp-critic #:mops))
