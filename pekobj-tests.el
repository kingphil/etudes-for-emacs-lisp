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
		   :type pekobj-slot-options)
   ;; TODO: I don't understand where/how "alloc-instance" is used, considering I do understand
   ;;   how "alloc-class" is referenced; why are they different?
   (alloc-instance :initarg :alloc-instance
		   :allocation :instance)
   ;; "Class allocated slots do not need :initarg"
   (alloc-class	:allocation :class)))

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

(ert-deftest test-pekobj-slot-options-type-my-class-name ()
  "chapter 3.2 slot options :type my-class-name"
  (let* ((pekobj-1 (pekobj-slot-options))
	 ;; no real test here; asserts that it doesn't error out (that I got the concept and syntax right)
	 (pekobj-2 (pekobj-slot-options :type-own-class pekobj-1))
	 (minimal (pekobj-minimal)))
    (should-error (pekobj-slot-options :type-own-class 2020) :type 'invalid-slot-type)
    (should-error (pekobj-slot-options :type-own-class minimal) :type 'invalid-slot-type)))

(defclass pekobj-slot-options-child (pekobj-slot-options) ())

(ert-deftest test-pekobj-slot-options-type-my-class-name-inheritance ()
  "chapter 3.2 slot options :type my-class-name in relation to inheritance"
  (let* ((vader (pekobj-slot-options))
	 ;; a child can contain an instance of a parent class
	 (luke (pekobj-slot-options-child :type-own-class vader))
	 ;; a parent class can contain an instance of a child class
	 (portman (pekobj-slot-options :type-own-class luke)))))

(ert-deftest test-pekobj-slot-options-allocation ()
  "chapter 3.2 slot options :allocation"
  (let ((luke (pekobj-slot-options :alloc-instance "luke"))
	(leia (pekobj-slot-options :alloc-instance "leia")))
    (oset luke alloc-class "now leia!")
    (should (equal (oref luke :alloc-instance) "luke"))
    (should (equal (oref leia :alloc-instance) "leia"))
    (should (equal (oref luke alloc-class) "now leia!"))
    (should (equal (oref leia alloc-class) "now leia!"))))

(defclass pekobj-slot-options-accessor ()
  ((pek-init-arg :initarg :my-init-arg
		 :initform "default"
		 :accessor access-init-arg
		 :writer write-init-arg
		 :reader read-init-arg)))

(ert-deftest test-pekobj-slot-options-accessor ()
  (let ((pekobj (pekobj-slot-options-accessor :my-init-arg "override")))
    (should (equal (access-init-arg pekobj) "override"))
    (should (equal (oref pekobj pek-init-arg) "override"))
    (write-init-arg pekobj "eieio")
    (should (equal (read-init-arg pekobj) "eieio"))
    (should (equal (access-init-arg pekobj) "eieio"))
    (should (equal (oref pekobj pek-init-arg) "eieio"))))

;; note: skipping :custom, :label, and :group (for now)
