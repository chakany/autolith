;;;; Load Autolith's ASDF system inside the rumprun guest from embedded
;;;; sources and save the loaded image for export. Contribs come from
;;;; SBCL_HOME; dependencies from /core/deps/; Autolith from /core/autolith/.
(require :asdf)

;; OpenSSL, when linked, is part of the static runtime rather than a
;; shared object that cl+ssl could open.
(pushnew :cl+ssl-foreign-libs-already-loaded *features*)

(asdf:initialize-source-registry
 '(:source-registry (:tree "/core/deps/") (:directory "/core/autolith/")
   :ignore-inherited-configuration))
(asdf:initialize-output-translations
 '(:output-translations (t ("/core/cache/" :**/ :*.*.*))
   :ignore-inherited-configuration))

(asdf:load-system :autolith)
(format t "AUTOLITH-LOADED ~A~%" (asdf:component-version (asdf:find-system :autolith)))
(finish-output)

;; The saved image keeps no compiled files, so tell ASDF that every loaded
;; system is final rather than letting a later load recompile it.
(map nil #'asdf:register-immutable-system (asdf:already-loaded-systems))

(ensure-directories-exist "/core/export/")
(sb-ext:save-lisp-and-die "/core/export/autolith.core")
