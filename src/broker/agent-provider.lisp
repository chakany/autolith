(in-package #:autolith)

;;;; -- Agent Provider Broker Route --

(-> broker-provider--agent-target (model-provider) string)
(defun broker-provider--agent-target (provider)
  "Return the broker registration name bound to PROVIDER."
  (let ((registration (model-provider-registration provider)))
    (unless registration
      (error 'broker-unavailable
             :message "The provider has no broker registration."
             :reason ':configuration))
    (provider-registration-name registration)))

(-> broker-provider--agent-payload
    (model-provider conversation json-object boolean)
    string)
(defun broker-provider--agent-payload
    (provider conversation request force-refresh)
  "Encode one projected request without endpoint, header, or credential data."
  (json-encode
   (json-object
    "model" (config :model (provider-configuration provider))
    "conversation_id" (conversation-identifier conversation)
    "prompt_cache_key" (conversation-prompt-cache-key conversation)
    "turn_state" (conversation-turn-state conversation)
    "force_refresh" (if force-refresh t *json-decoded-false*)
    "request" request)))

(-> broker-provider--agent-attempt
    (subscription-provider conversation
     &key (:tool-namespaces vector) (:event-callback function)
          (:force-refresh boolean) (:goal-context (option string))
          (:compaction-p boolean))
    provider-result)
(defun broker-provider--agent-attempt
    (provider conversation
     &key tool-namespaces event-callback force-refresh goal-context compaction-p)
  "Execute one projected provider attempt through the trusted broker stream."
  (multiple-value-bind (request delivery)
      (provider-request-object provider conversation tool-namespaces
                               :goal-context goal-context
                               :compaction-p compaction-p)
    (broker-client-request
     ':provider-turn
     (broker-provider--agent-target provider)
     (broker-provider--agent-payload provider conversation request force-refresh)
     (lambda (stream)
       (cl-llm-provider-api::provider-execute-request
        provider request
        :event-callback event-callback
        :transport
        (lambda (projected-request)
          (declare (ignore projected-request))
          (multiple-value-bind (body status)
              (broker-response-open stream)
            (values body status nil)))
        :completion (lambda () (context-delivery-complete delivery)))))))

(defmethod provider-attempt-turn :around
    ((provider subscription-provider) (conversation conversation)
     &key tool-namespaces event-callback force-refresh goal-context compaction-p)
  "Route sandboxed provider attempts through the broker without loading a token."
  (if (agent-sandbox-active-p)
      (broker-provider--agent-attempt
       provider conversation
       :tool-namespaces tool-namespaces
       :event-callback event-callback
       :force-refresh force-refresh
       :goal-context goal-context
       :compaction-p compaction-p)
      (call-next-method)))

(defmethod provider-attempt-native-compaction :around
    ((provider codex-subscription-provider) (conversation conversation)
     &key tool-namespaces force-refresh)
  "Route native compaction through the broker in a sandboxed agent."
  (if (agent-sandbox-active-p)
      (let ((request (provider-native-compaction-request-object
                      provider conversation tool-namespaces)))
        (broker-client-request
         ':provider-compaction
         (broker-provider--agent-target provider)
         (broker-provider--agent-payload
          provider conversation request force-refresh)
         (lambda (stream)
           (multiple-value-bind (body status)
               (broker-response-open stream)
             (unless (= status 200)
               (provider--codex-signal-status-failure
                provider status :headers nil :raw-body body))
             (provider--decode-native-compaction-response
              provider body :status status :headers nil)))))
      (call-next-method)))

(-> broker-provider--agent-discover (provider-registration) list)
(defun broker-provider--agent-discover (registration)
  "Receive bounded model metadata discovered by the trusted broker."
  (broker-client-request
   ':provider-models
   (provider-registration-name registration)
   ""
   (lambda (stream)
     (let ((frame (management-repl-read-frame
                   stream *broker-maximum-frame-size*)))
       (unless (and (listp frame)
                    (eql (ignore-errors (list-length frame)) 5)
                    (equal (subseq frame 0 4)
                           '(:broker-result :status :models :models))
                    (listp (fifth frame))
                    (<= (length (fifth frame)) 1024))
         (error 'broker-protocol-error
                :message "The broker returned invalid model metadata."
                :reason ':response))
       (provider--normalize-models (fifth frame) :allow-empty-p t)))))
