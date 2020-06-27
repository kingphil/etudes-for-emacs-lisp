(require 'eieio-base)

;; eieio-named
(defclass pekobj-named (eieio-named) ())

(ert-deftest test-pekobj-named ()
  "Basic test of 'eieio-named' class"
  (let* ((my-obj-name "pekobj-named")
	 (my-obj-rename "pekobj-renamed")
	 (pekobj (pekobj-named :object-name my-obj-name)))
    (progn
      (should (equal (oref pekobj :object-name) my-obj-name))
      (should-error (eieio-object-set-name-string pekobj 'must-be-string) :type 'wrong-type-argument)
      (eieio-object-set-name-string pekobj my-obj-rename)
      (should (equal (oref pekobj :object-name) my-obj-rename)))))

;; minimal syntax for a class
(defclass pekobj-minimal () ())

(cl-defmethod echo ((obj-name pekobj-minimal) arg)
  arg)

(ert-deftest test-pekobj-minimal ()
  "Test for absolute minimum syntax"
  (let ((pekobj (pekobj-minimal)))
    (should (equal (echo pekobj "hellooooo!") "hellooooo!"))))

;; object and tests for chapter 3.2"
(defclass pekobj-slot-options ()
  ((no-initform :initarg :no-initform)
   (initform :initarg :initform
	     :initform "default")
   (type-symbol :initarg :type-symbol
		:type symbol)
   (type-own-class :initarg :type-own-class
		   ;; not a literal "my-class-name" as suggested in the docs
		   :type pekobj-slot-options)))

(ert-deftest test-pekobj-slot-options-initform ()
  "chapter 3.2 slot options :initform"
  (let ((pekobj (pekobj-slot-options)))
    (should-not (slot-boundp pekobj :no-initform))
    (should (slot-boundp pekobj :initform))
    (should (equal (oref pekobj :initform) "default"))
    (slot-makeunbound pekobj :initform)
    (should-not (slot-boundp pekobj :initform))))

(ert-deftest test-pekobj-slot-options-type ()
  "chapter 3.2 slot options :type"
  (should (pekobj-slot-options :type-symbol 'this-is-fine))
  (should-error (pekobj-slot-options :type-symbol 2020) :type 'invalid-slot-type))

;; no real test here; asserts that it doesn't error out (that I got the concept and syntax right)
(ert-deftest test-pekobj-slot-options-type-my-class-name ()
  "chapter 3.2 slot options :type my-class-name"
  (let* ((pekobj-1 (pekobj-slot-options))
	 (pekobj-2 (pekobj-slot-options :type-own-class pekobj-1)))))
