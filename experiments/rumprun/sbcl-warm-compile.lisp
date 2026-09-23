;;;; Compile SBCL's warm-initialization sources inside the rumprun guest,
;;;; following make-target-2.sh's compilation phase. The fasls land below
;;;; /core/export so the runtime exports them when the guest exits.
(defvar *objfile-prefix* "export/obj/from-self/")
(sb-fasl::!warm-load "src/cold/warm.lisp")
