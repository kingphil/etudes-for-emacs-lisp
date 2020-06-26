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
