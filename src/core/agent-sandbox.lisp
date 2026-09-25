(in-package #:autolith)

;;;; -- Agent Sandbox Awareness --

;;; The stable launcher starts the agent inside a sandbox the pristine
;;; recovery image computes, and records that in AUTOLITH_AGENT_SANDBOX. The
;;; sandbox is the boundary for everything the agent runs, so inside it
;;; commands need no sandbox of their own, run with full access by default,
;;; and receive a private home, because the user's home is hidden.

(-> agent-sandbox-active-p () boolean)
(defun agent-sandbox-active-p ()
  "Return whether this process runs inside the launcher's agent sandbox."
  (equal (uiop:getenv "AUTOLITH_AGENT_SANDBOX") "active"))

(-> agent-sandbox-default-permission-mode (keyword) keyword)
(defun agent-sandbox-default-permission-mode (fallback)
  "Return the command permission mode to use when none is chosen or saved:
full access inside the agent sandbox, which already confines every command,
and FALLBACK outside it."
  (if (agent-sandbox-active-p)
      ':full-access
      fallback))
