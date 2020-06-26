(require 'eieio-base)

;; following convention of prefixing everything with "pekobj-"
;; minimal amount of code to define and exercise a class
(defclass pekobj-minimal-class (eieio-named) ())

(ert-deftest pekobj-test-minimal-class ()
  "Basic test of 'minimal' class"
  (let ((pekobj (pekobj-minimal-class :object-name "explicitly-named")))
    (should (equal (oref explicit :object-name) "explicitly-named"))))
