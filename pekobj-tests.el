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

(ert-deftest test-pekobj-basic-functionality ()
  "Test for basic functionality"
  (let ((pekobj (pekobj-minimal)))
    (should (pekobj-minimal-p pekobj))))

(defclass father ()
  ((parent :initform "vader")))
(cl-defmethod say ((vader father))
  "No, *I* am your father!")

(defclass mother ()
  ((parent :initform "portman")))
(cl-defmethod say ((portman mother))
  "Pass me another slice of pear, sand boy.")

(defclass luke-class (father mother) ())
(defclass leia-class (mother father) ())

(ert-deftest test-pekobj-inheritance ()
  (let ((luke (luke-class))
	(leia (leia-class)))
    ;; "If my-baseclass and my-interface had slots with the same name, then the
    ;; superclass showing up in the list first defines the slot attributes."
    (should (equal (oref luke parent) "vader"))
    (should (equal (oref leia parent) "portman"))
    (should (string-prefix-p "No," (say luke)))
    (should (string-prefix-p "Pass" (say leia)))))

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

(defclass pekobj-class-options-doc
  () ;; superclass-list
  () ;; slot-list
  "Old school documentation string"
  ;; &rest options-and-doc
  :documentation "eieio documentation string (old school has precedence)")

(ert-deftest test-pekobj-class-options-doc ()
  (should (string-prefix-p
	   "Old school"
	   (documentation-property 'pekobj-class-options-doc 'variable-documentation))))

(defclass pekobj-class-options-initform ()
  ((name :initarg :name
	 :initform nil
	 :type string))
  ;; the actual test is that this compiles; if this nil, compilation explodes
  :allow-nil-initform t)

(ert-deftest test-pekobj-class-options-initform ()
  (let ((pekobj (pekobj-class-options-initform)))
    (should (equal nil (oref pekobj name)))))

(defclass pekobj-class-options-abstract () ()
  :abstract t)

;; tricky: the error doesn't really throw a :type that can be caught by "should-error"
(ert-deftest test-pekobj-class-options-abstract ()
  (condition-case err
      (pekobj-class-options-abstract)
    ;; here we are handling the standard error known as "error" (and not throwing an error)
    (error (should (equal "Class pekobj-class-options-abstract is abstract"
			  (error-message-string err))))))

;; note: skipping :custom-groups
;; also skipping :method-invocation-order, which controls multiple inheritance

(ert-deftest test-pekobj-accessing-slots ()
  ;; define this class as an "inner class" so that the oset-default does not explode subsequent test runs
  (defclass pekobj-accessing-slots ()
    ((name :initarg :name
	   :initform "[anonymous]"
	   :type string
	   :documentation "The name of a person.")))
  (let ((pekobj (pekobj-accessing-slots)))
    (should (equal (oref-default pekobj-accessing-slots :name) "[anonymous]"))
    (should (equal (oref pekobj :name) "[anonymous]"))
    (should (equal (oref pekobj :name)
		   ;; both 'name and :name work here, as seen below
		   (slot-value pekobj :name)))
    (oset-default pekobj-accessing-slots :name "[unknown]")
    ;; existing instances have not changed
    (should (equal (slot-value pekobj 'name) "[anonymous]"))
    (let ((newobj (pekobj-accessing-slots)))
      (should (equal (oref newobj :name) "[unknown]")))
    ;; tests that the return value of oset is the value it sets
    (should (equal (oset pekobj :name "Philip") "Philip"))
    (should (equal (slot-value pekobj 'name) "Philip"))
    ;; although unspecified in the docs, set-slot-value also returns the value it sets
    (should (equal (set-slot-value pekobj ':name "pek") "pek"))
    (should (equal (oref pekobj :name) "pek"))
    (slot-makeunbound pekobj :name)
    (should-error (equal (oref pekobj :name) nil) :type 'unbound-slot)
    (should-not (slot-boundp pekobj :name))))

(defclass pekobj-accessing-slots-slot-lists ()
  ((mylist :initarg :mylist
	 :initform '(this is a test)
	 :type list)))

;; appending to an object attribute that is a list must happen so frequently that this convenience was needed
(ert-deftest test-pekobj-accessing-slots-slot-lists ()
  (let ((pekobj (pekobj-accessing-slots-slot-lists)))
    ;; the true value on the end is to instruct the method to append to the list
    (object-add-to-list pekobj :mylist '(of the emergency system) t)
    (should (equal (oref pekobj mylist)
		   '(this is a test (of the emergency system))))
    (object-remove-from-list pekobj :mylist '(of the emergency system))
    (should (equal (oref pekobj mylist) '(this is a test)))))

;; hard to imagine what this is for; aliasing? the example in the docs doesn't help much
;; does not seem to offer much over 'oref', as seen in the last lines
(ert-deftest test-pekobj-accessing-slots-with-slots ()
  (let ((pekobj (pekobj-accessing-slots-slot-lists)))
    (should (equal (with-slots (mylist) pekobj mylist) '(this is a test)))
    (should (equal (with-slots ((alias mylist)) pekobj alias) '(this is a test)))
    (with-slots ((alias mylist)) pekobj
      (setq alias '(convenience feature)))
    (should (equal (oref pekobj mylist) '(convenience feature)))
    (oset pekobj mylist '(not exactly more difficult))
    (should (equal (oref pekobj mylist) '(not exactly more difficult)))))
