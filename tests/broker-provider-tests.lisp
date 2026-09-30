(in-package #:autolith)

;;;; -- Trusted Provider Transport Tests --

(-> test-broker-provider-failures () null)
(defun test-broker-provider-failures ()
  "Test broker failures retain safe categories across the real client boundary."
  (with-platform-capability (':local-sockets "broker provider failures")
    (let* ((root (platform-make-temporary-directory
                  *platform* (uiop:temporary-directory) "broker-failures-"))
           (pathname (merge-pathnames "broker.sock" root))
           (server
             (broker-server-create
              pathname
              (lambda (request write-frame)
                (let ((target (getf (rest request) ':target)))
                  (when (string= target "stream")
                    (funcall write-frame '(:broker-result :status :open :code 200)))
                  (cond
                    ((string= target "target")
                     (error 'broker-protocol-error
                            :reason ':target :message "private-token"))
                    ((string= target "payload")
                     (error 'broker-protocol-error
                            :reason ':payload :message "private-token"))
                    ((string= target "authentication")
                     (error 'authentication-error :message "private-token"))
                    (t
                     (error "private-token")))))))
           (thread nil))
      (unwind-protect
           (progn
             (broker-server-start server)
             (setf thread (make-thread (lambda () (broker-server-serve server))
                                       :name "Broker failure test"))
             (dolist (case '(("target" :target "trusted configuration")
                             ("payload" :payload "payload")
                             ("authentication" :authentication "autolith auth")
                             ("handler" :handler "complete")
                             ("stream" :handler "complete")))
               (let ((failure
                       (handler-case
                           (broker-client-request
                            ':provider-turn (first case) "{}"
                            (lambda (stream)
                              (multiple-value-bind (body status)
                                  (broker-response-open stream)
                                (declare (ignore status))
                                (read-char body)))
                            :socket-pathname pathname)
                         (broker-unavailable (condition) condition))))
                 (test-assert
                  (and (typep failure 'broker-unavailable)
                       (eq (broker-unavailable-reason failure) (second case)))
                  "client preserves opening and streaming failure categories")
                 (test-assert
                  (search (third case) (autolith-error-message failure))
                  "client supplies an actionable category-specific message")
                 (test-assert
                  (not (search "private-token" (autolith-error-message failure)))
                  "trusted condition data never reaches the agent"))))
        (broker-server-close server)
        (when thread (join-thread thread))
        (platform-delete-directory-tree *platform* root
                                        :validate t :if-does-not-exist ':ignore))))
  (dolist (frame '((:broker-result :status :failed)
                    (:broker-result :status :failed :reason :unknown)))
    (let ((output (make-in-memory-output-stream)))
      (broker-write-frame output frame)
      (test-assert
       (handler-case
           (progn
             (broker-response-open
              (flexi-streams:make-in-memory-input-stream
               (get-output-stream-sequence output)))
             nil)
         (broker-unavailable () (= (length frame) 3))
         (broker-protocol-error () (= (length frame) 5)))
       "legacy failures remain readable and unknown categories are rejected")))
  nil)

(-> test-broker-terminal-approval () null)
(defun test-broker-terminal-approval ()
  "Test a broker prompt reaches only the private trusted terminal endpoint."
  (with-test-configuration (configuration root)
    (declare (ignore configuration root))
    (let ((state-home
            (platform-make-temporary-directory *platform* #P"/tmp/" "approval.")))
      (unwind-protect
           (with-test-environment (("XDG_STATE_HOME" (namestring state-home)))
        (let* ((launcher-state (platform-launcher-root *platform* ':state))
               (directory (merge-pathnames "approval/session.test/" launcher-state))
               (socket-path (merge-pathnames "control.sock" directory))
               (listener nil)
               (worker nil)
               (received nil))
          (ensure-directories-exist directory)
          (sb-posix:chmod (namestring launcher-state) #o700)
          (sb-posix:chmod (namestring (merge-pathnames "approval/" launcher-state))
                          #o700)
          (sb-posix:chmod (namestring directory) #o700)
          (unwind-protect
               (progn
                 (setf listener (platform-local-listener *platform* socket-path))
                 (setf worker
                       (make-thread
                        (lambda ()
                          (let* ((socket (sb-bsd-sockets:socket-accept listener))
                                 (stream (sb-bsd-sockets:socket-make-stream
                                          socket :input t :output t
                                          :element-type '(unsigned-byte 8)
                                          :buffering ':none)))
                            (unwind-protect
                                 (let ((length 0))
                                   (dotimes (index 4)
                                     (setf length
                                           (+ (ash length 8) (read-byte stream))))
                                   (let ((bytes (make-array length
                                                            :element-type '(unsigned-byte 8))))
                                     (read-sequence bytes stream)
                                     (setf received
                                           (sb-ext:octets-to-string
                                            bytes :external-format ':utf-8)))
                                   (write-byte (char-code #\1) stream)
                                   (finish-output stream))
                              (close stream)
                              (sb-bsd-sockets:socket-close socket))))
                        :name "Broker approval fixture"))
                 (with-test-environment
                     (("AUTOLITH_BROKER_APPROVAL_SOCKET" (namestring socket-path)))
                   (test-assert (broker-terminal-approve "Fixture database query")
                                "the private relay can approve the exact action")
                   (test-assert (string= received "Fixture database query")
                                "the relay receives the complete action")
                   (test-assert
                    (not (broker-terminal-approve
                          (make-string (1+ *broker-approval-maximum-bytes*)
                                       :initial-element #\x)))
                    "an action too large to display is denied")))
            (when listener
              (ignore-errors (sb-bsd-sockets:socket-close listener)))
            (when worker
              (ignore-errors (join-thread worker)))
            (when (probe-file socket-path)
              (delete-file socket-path)))))
        (platform-delete-directory-tree *platform* state-home
                                        :validate t :if-does-not-exist ':ignore))))
  nil)

(-> test-broker-provider-transport () null)
(defun test-broker-provider-transport ()
  "Test trusted provider selection, credential scope, and streamed output."
  (with-test-configuration (configuration)
    (let* ((registration (provider-registration-find "chatgpt"))
           (model "gpt-6.1-sol")
           (payload
             (json-encode
              (json-object "model" model
                           "conversation_id" "broker-test-1"
                           "prompt_cache_key" nil
                           "turn_state" nil
                           "force_refresh" *json-decoded-false*
                           "request" (json-object "model" model))))
           (credentials
             (make-instance 'oauth-credentials
                            :access-token "broker-secret-token"
                            :refresh-token nil
                            :id-token nil
                            :account-id "broker-account"
                            :expires-at nil
                            :source-path #P"/private/tmp/broker-test-auth"))
           (frames nil)
           (captured-headers nil))
      (test-call-with-function-replacements
       (list
        (list 'call-with-credentials
              (lambda (manager function &key force-refresh)
                (declare (ignore manager force-refresh))
                (funcall function credentials)))
        (list 'dexador:post
              (lambda (url &key headers content &allow-other-keys)
                (declare (ignore url content))
                (setf captured-headers headers)
                (values (make-string-input-stream
                         (format nil "data: first~%~%"))
                        200 nil))))
       (lambda ()
         (broker-provider-stream
          configuration "chatgpt" payload
          (lambda (frame) (push frame frames)))))
      (setf frames (nreverse frames))
      (test-assert
       (equal (first frames)
              '(:broker-result :status :open :code 200))
       "broker opens a provider stream with status only")
      (test-assert
       (equal (second frames)
              (list ':broker-chunk ':text (format nil "data: first~%~%")))
       "broker forwards provider text in bounded chunks")
      (test-assert (equal (third frames) '(:broker-end))
                   "broker terminates a complete provider stream")
      (test-assert
       (some (lambda (header)
               (search "broker-secret-token" (rest header)))
             captured-headers)
       "only the trusted provider transport receives the credential")
      (multiple-value-bind (resolved-provider conversation force-refresh)
          (broker-provider--resolve configuration "chatgpt"
                                    (broker-provider--payload payload))
        (declare (ignore conversation force-refresh))
        (test-assert
         (eq (model-provider-registration resolved-provider) registration)
         "trusted provider retains its selected registration"))
      (test-assert
       (not (search "broker-secret-token"
                    (with-output-to-string (stream)
                      (prin1 frames stream))))
       "broker response frames never contain the credential")
      (test-assert
       (handler-case
           (progn
             (broker-provider-stream
              configuration "chatgpt"
              (json-encode
               (json-object "model" model
                            "conversation_id" "broker-test-2"
                            "prompt_cache_key" nil
                            "turn_state" nil
                            "force_refresh" *json-decoded-false*
                            "request" (json-object "model" model)
                            "endpoint" "https://attacker.invalid"))
              (lambda (frame) (declare (ignore frame))))
             nil)
         (broker-protocol-error () t))
       "broker rejects an agent-supplied endpoint")))
  nil)

(-> test-broker-trusted-configuration () null)
(defun test-broker-trusted-configuration ()
  "Test the broker uses launcher roots and ignores agent initialization."
  (with-test-configuration (agent root)
    (let ((config-home (merge-pathnames "config/" root))
          (data-home (merge-pathnames "data/" root))
          (state-home (merge-pathnames "state/" root))
          (cache-home (merge-pathnames "cache/" root)))
      (with-test-environment
          (("XDG_CONFIG_HOME" (namestring config-home))
           ("XDG_DATA_HOME" (namestring data-home))
           ("XDG_STATE_HOME" (namestring state-home))
           ("XDG_CACHE_HOME" (namestring cache-home)))
        (let ((broker (broker--configuration)))
          (configuration-ensure-directories broker)
          (test-assert
           (equal (config :config-root broker)
                  (platform-launcher-root *platform* ':config))
           "broker config comes from the launcher root")
          (test-assert
           (not (equal (configuration-user-init-path broker)
                       (configuration-user-init-path agent)))
           "broker initialization is separate from the agent")
          (test-assert
           (null (broker--load-trusted-init broker))
           "broker starts without executing agent initialization")
          (let ((captured nil))
            (test-call-with-function-replacements
             (list (list 'main-authenticate
                         (lambda (trusted selection method)
                           (setf captured (list trusted selection method))
                           nil)))
             (lambda () (broker-authenticate "chatgpt" "device")))
            (test-assert
             (and (equal (rest captured) '("chatgpt" "device"))
                  (equal (config :state-root (first captured))
                         (platform-launcher-root *platform* ':state)))
             "trusted terminal authentication uses launcher state"))))))
  nil)

(-> test-broker-agent-credential-denial () null)
(defun test-broker-agent-credential-denial ()
  "Test direct credential access and interactive login fail in the agent."
  (with-test-configuration (configuration)
    (let* ((registration (provider-registration-find "chatgpt"))
           (model (provider-model-name
                   (first (provider-registration-models registration))))
           (provider (provider-create
                      (configuration-copy configuration :model model)
                      :registration registration)))
      (with-test-environment (("AUTOLITH_AGENT_SANDBOX" "active"))
        (test-assert
         (handler-case
             (progn
               (call-with-credentials
                (provider-credential-manager provider)
                (lambda (credentials)
                  (declare (ignore credentials))
                  (error "Agent reached a credential.")))
               nil)
           (authentication-error () t))
         "agent cannot enter a direct credential request scope")
        (test-assert
         (handler-case
             (progn
               (provider-authenticate provider)
               nil)
           (authentication-error () t))
         "agent cannot run interactive authentication"))))
  nil)

(-> test-broker-agent-provider-route () null)
(defun test-broker-agent-provider-route ()
  "Test an active agent consumes broker transport without loading credentials."
  (with-platform-capability (':local-sockets "broker provider route")
    (with-test-configuration (configuration)
      (let* ((registration (provider-registration-find "chatgpt"))
             (model (provider-model-name
                     (first (provider-registration-models registration))))
             (selected (configuration-copy configuration :model model))
             (provider (provider-create selected :registration registration))
             (conversation (conversation-create selected))
             (socket-root
               (platform-make-temporary-directory
                *platform* (uiop:temporary-directory)
                "autolith-broker-provider-"))
             (socket-pathname (merge-pathnames "broker.sock" socket-root))
             (captured-request nil)
             (captured-response nil)
             (server
               (broker-server-create
                socket-pathname
                (lambda (request write-frame)
                  (setf captured-request request)
                  (let ((payload
                          (broker-provider--payload
                           (getf (rest request) ':payload))))
                    (unless (and (string= (getf (rest request) ':target)
                                          "chatgpt")
                                 (string= (json-get payload "model") model))
                      (error "Agent sent an unexpected broker target."))
                    (funcall write-frame
                             '(:broker-result :status :open :code 200))
                    (funcall write-frame
                             (list ':broker-chunk ':text
                                   (format nil "data: broker-result~%")))
                    (funcall write-frame '(:broker-end))))))
             (thread nil))
        (unwind-protect
             (progn
               (broker-server-start server)
               (setf thread
                     (make-thread (lambda () (broker-server-serve server))
                                  :name "Broker provider route test"))
               (with-test-environment
                   (("AUTOLITH_AGENT_SANDBOX" "active")
                    ("AUTOLITH_BROKER_SOCKET" (namestring socket-pathname)))
                 (test-call-with-function-replacements
                  (list
                   (list 'call-with-credentials
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error "Agent tried to load credentials.")))
                   (list 'provider-request-object
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (values (json-object "model" model) nil)))
                   (list 'cl-llm-provider-api::provider-execute-request
                         (lambda (provider request &key transport
                                                    &allow-other-keys)
                           (declare (ignore provider))
                           (multiple-value-bind (body status)
                               (funcall transport request)
                             (setf captured-response
                                   (list status (read-line body)))
                             (make-instance 'provider-result)))))
                  (lambda ()
                    (provider-attempt-turn
                     provider conversation
                     :tool-namespaces #()
                     :event-callback (lambda (&rest values)
                                       (declare (ignore values)))))))
               (test-assert
                (equal captured-response '(200 "data: broker-result"))
                "agent provider attempt uses the broker stream")
               (test-assert
                (and captured-request
                     (string= (getf (rest captured-request) ':target)
                              "chatgpt"))
                "agent sends its registered target to the broker")
          (broker-server-close server)
          (when thread (join-thread thread))
          (platform-delete-directory-tree *platform* socket-root
                                          :validate t
                                          :if-does-not-exist ':ignore)))))
  nil))

(-> test-broker-native-compaction () null)
(defun test-broker-native-compaction ()
  "Test Codex compaction uses broker credentials and bounded response frames."
  (with-test-configuration (configuration)
    (let* ((registration (provider-registration-find "chatgpt"))
           (model (provider-model-name
                   (first (provider-registration-models registration))))
           (payload (json-encode
                     (json-object
                      "model" model
                      "conversation_id" "broker-compaction"
                      "prompt_cache_key" nil
                      "turn_state" nil
                      "force_refresh" *json-decoded-false*
                      "request" (json-object "model" model))))
           (frames nil)
           (credential (make-instance 'oauth-credentials
                                      :access-token "compaction-secret"
                                      :refresh-token nil
                                      :id-token nil
                                      :account-id "broker-account"
                                      :expires-at nil
                                      :source-path #P"/private/tmp/broker-test-auth")))
      (test-call-with-function-replacements
       (list
        (list 'call-with-credentials
              (lambda (manager function &key force-refresh)
                (declare (ignore manager force-refresh))
                (funcall function credential)))
        (list 'provider-open-native-compaction
              (lambda (provider request &key credentials conversation)
                (declare (ignore provider conversation))
                (test-assert
                 (and (eq credentials credential)
                      (json-string= (json-get request "model") model))
                 "compaction transport receives broker credentials")
                (values "{\"output\":[]}" 200 nil))))
       (lambda ()
         (broker-provider-compact
          configuration "chatgpt" payload
          (lambda (frame) (push frame frames)))))
      (setf frames (nreverse frames))
      (test-assert
       (equal frames
              '((:broker-result :status :open :code 200)
                (:broker-chunk :text "{\"output\":[]}")
                (:broker-end)))
       "broker forwards only the compaction status and body")
      (test-assert
       (not (search "compaction-secret"
                    (with-output-to-string (stream) (prin1 frames stream))))
       "compaction frames contain no broker credential")))
  nil)

(-> test-broker-agent-native-compaction () null)
(defun test-broker-agent-native-compaction ()
  "Test a sandboxed compaction reads broker output without loading a token."
  (with-platform-capability (':local-sockets "broker native compaction")
    (with-test-configuration (configuration)
      (let* ((registration (provider-registration-find "chatgpt"))
             (model (provider-model-name
                     (first (provider-registration-models registration))))
             (selected (configuration-copy configuration :model model))
             (provider (provider-create selected :registration registration))
             (conversation (conversation-create selected))
             (socket-root
               (platform-make-temporary-directory
                *platform* (uiop:temporary-directory)
                "autolith-broker-compact-"))
             (socket-pathname (merge-pathnames "broker.sock" socket-root))
             (captured-operation nil)
             (captured-body nil)
             (server
               (broker-server-create
                socket-pathname
                (lambda (request write-frame)
                  (setf captured-operation
                        (getf (rest request) ':operation))
                  (funcall write-frame
                           '(:broker-result :status :open :code 200))
                  (funcall write-frame
                           '(:broker-chunk :text "{\"output\":[]}"))
                  (funcall write-frame '(:broker-end)))))
             (thread nil))
        (unwind-protect
             (progn
               (broker-server-start server)
               (setf thread
                     (make-thread (lambda () (broker-server-serve server))
                                  :name "Broker native compaction test"))
               (with-test-environment
                   (("AUTOLITH_AGENT_SANDBOX" "active")
                    ("AUTOLITH_BROKER_SOCKET" (namestring socket-pathname)))
                 (test-call-with-function-replacements
                  (list
                   (list 'call-with-credentials
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (error "Agent tried to load credentials.")))
                   (list 'provider-native-compaction-request-object
                         (lambda (&rest arguments)
                           (declare (ignore arguments))
                           (json-object "model" model)))
                   (list 'provider--decode-native-compaction-response
                         (lambda (provider body &key status headers)
                           (declare (ignore provider headers))
                           (setf captured-body
                                 (list status (read-line body)))
                           nil)))
                  (lambda ()
                    (provider-attempt-native-compaction
                     provider conversation :tool-namespaces #()))))
               (test-assert (eq captured-operation ':provider-compaction)
                            "agent requests broker native compaction")
               (test-assert
                (equal captured-body '(200 "{\"output\":[]}"))
                "agent decodes only the broker compaction body"))
          (broker-server-close server)
          (when thread (join-thread thread))
          (platform-delete-directory-tree *platform* socket-root
                                          :validate t
                                          :if-does-not-exist ':ignore)))))
  nil)

(-> test-broker-provider-model-discovery () null)
(defun test-broker-provider-model-discovery ()
  "Test trusted discovery returns only bounded public model metadata."
  (with-test-configuration (configuration)
    (let* ((registration (provider-registration-find "openrouter"))
           (original (provider-registration-model-discovery registration))
           (frames nil))
      (unwind-protect
           (progn
             (setf (slot-value registration 'model-discovery)
                   (lambda (trusted-configuration)
                     (test-assert (eq trusted-configuration configuration)
                                  "discovery uses broker configuration")
                     (list (list ':name "fixture-model"
                                 ':context-window 32000
                                 ':reasoning-efforts '("low" "high")))))
             (broker-provider-discover
              configuration "openrouter" ""
              (lambda (frame) (push frame frames)))
             (setf frames (nreverse frames))
             (test-assert
              (equal frames
                     '((:broker-result :status :models :models
                        ((:name "fixture-model" :description ""
                          :context-window 32000
                          :reasoning-efforts ("low" "high"))))))
              "broker returns model metadata without credentials")
             (test-assert
              (handler-case
                  (progn
                    (broker-provider-discover
                     configuration "openrouter" "https://attacker.invalid"
                     (lambda (frame) (declare (ignore frame))))
                    nil)
                (broker-protocol-error () t))
              "model discovery rejects agent supplied endpoint data"))
        (setf (slot-value registration 'model-discovery) original))))
  nil)

(-> test-broker-agent-model-discovery () null)
(defun test-broker-agent-model-discovery ()
  "Test a sandboxed agent receives a broker model list over its socket."
  (with-platform-capability (':local-sockets "broker model discovery")
    (let* ((registration (provider-registration-find "openrouter"))
           (socket-root
             (platform-make-temporary-directory
              *platform* (uiop:temporary-directory)
              "autolith-broker-models-"))
           (socket-pathname (merge-pathnames "broker.sock" socket-root))
           (captured-request nil)
           (server
             (broker-server-create
              socket-pathname
              (lambda (request write-frame)
                (setf captured-request request)
                (funcall write-frame
                         '(:broker-result :status :models :models
                           ((:name "fixture-model" :description ""
                             :context-window 32000
                             :reasoning-efforts ("low" "high"))))))))
           (thread nil))
      (unwind-protect
           (progn
             (broker-server-start server)
             (setf thread
                   (make-thread (lambda () (broker-server-serve server))
                                :name "Broker model discovery test"))
             (with-test-environment
                 (("AUTOLITH_BROKER_SOCKET" (namestring socket-pathname)))
               (let ((models (broker-provider--agent-discover registration)))
                 (test-assert
                  (equal (mapcar #'provider-model-name models)
                         '("fixture-model"))
                  "agent receives validated model metadata")))
             (test-assert
              (and (eq (getf (rest captured-request) ':operation)
                       ':provider-models)
                   (string= (getf (rest captured-request) ':target)
                            "openrouter")
                   (string= (getf (rest captured-request) ':payload) ""))
              "agent grants broker no endpoint or credential data"))
        (broker-server-close server)
        (when thread (join-thread thread))
        (platform-delete-directory-tree *platform* socket-root
                                        :validate t
                                        :if-does-not-exist ':ignore))))
  nil)
