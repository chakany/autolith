;;;; Complete SBCL warm initialization inside the rumprun guest and save the
;;;; warm core for export, following make-target-2.sh's load-and-dump phase.
(sb-fasl::!warm-load "make-target-2-load.lisp")
(setf (extern-alien "gc_coalesce_string_literals" char) 2)
;; Use the historical conventions that make-target-2.sh selects.
(setf sb-c::*merge-pathnames* t)
(setq sb-c::*name-context-file-path-selector* 'truename)
(setq sb-c::*check-consistency* nil)
(ensure-directories-exist "/core/export/")
(let ((sb-ext:*invoke-debugger-hook* (prog1 sb-ext:*invoke-debugger-hook*
                                        (sb-ext:enable-debugger))))
  (sb-ext:save-lisp-and-die "/core/export/sbcl.core"))
