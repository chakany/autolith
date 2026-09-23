;;;; Run Autolith's FiveAM suites inside the rumprun guest, on the core that
;;;; sbcl-autolith.lisp saved. Test sources come from /core/autolith/ and
;;;; test-only dependencies from /core/deps/.
(require :asdf)

(asdf:initialize-source-registry
 '(:source-registry (:tree "/core/deps/") (:directory "/core/autolith/")
   :ignore-inherited-configuration))
(asdf:initialize-output-translations
 '(:output-translations (t ("/core/cache/" :**/ :*.*.*))
   :ignore-inherited-configuration))

;; Tests allocate their own configuration roots below a writable home.
(ensure-directories-exist "/core/home/")
(sb-posix:setenv "HOME" "/core/home" 1)

(asdf:load-system :autolith/tests)
(let ((result (uiop:symbol-call :autolith :tests-run-cases
                                (uiop:symbol-call :autolith :tests-select))))
  (uiop:symbol-call :autolith :tests-report result *standard-output*
                    :case-timings-p nil)
  (format t "AUTOLITH-TESTS-DONE ~D cases ~D failed~%"
          (getf result :cases) (length (getf result :failures)))
  (finish-output))
(sb-ext:exit :code 0)
