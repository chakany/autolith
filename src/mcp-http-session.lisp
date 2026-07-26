(in-package #:autolith)


;;;; -- Streamable HTTP Session Compatibility --

(defun mcparen::mcp-http--stage-initialize-session-header
    (transport response-session
     &key request-session initialize-request-p)
  "Validate and stage an initialization session header.

A repeated response header matching the session carried by a subsequent
request is tolerated for compatibility with the reference Python MCP SDK.
A missing, invalid, or changed session identifier still follows the normal
protocol checks."
  (when initialize-request-p
    (bordeaux-threads:with-lock-held
        ((mcparen::mcp-http-transport-state-lock transport))
      (setf (mcparen::mcp-http-transport-pending-session-identifier transport)
            nil)))
  (when (and initialize-request-p request-session)
    (error 'mcparen:mcp-protocol-error
           :message
           "An MCP initialize request carried an existing session identifier."
           :method "initialize"
           :payload nil))
  (when response-session
    (unless (mcparen::mcp-http--valid-session-identifier-p response-session)
      (error 'mcparen:mcp-protocol-error
             :message "The MCP server returned an invalid session identifier."
             :method nil
             :payload nil))
    (cond
      (initialize-request-p
       (bordeaux-threads:with-lock-held
           ((mcparen::mcp-http-transport-state-lock transport))
         (setf
          (mcparen::mcp-http-transport-pending-session-identifier transport)
          response-session)))
      ((and request-session
            (equal response-session request-session)))
      (t
       (error 'mcparen:mcp-protocol-error
              :message
              "The MCP server returned a new session identifier outside initialization."
              :method nil
              :payload nil))))
  nil)
