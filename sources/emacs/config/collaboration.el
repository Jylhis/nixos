;;; collaboration.el --- Collaboration tools integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for collaboration tools like Slack, Jira, and Confluence.

;;; Code:

(use-package org-jira
  :ensure t
  :after org
  :config
  ;; The user needs to configure JIRA access.
  ;; This can be done by setting the `jiras-wiki-pre-process-rules`
  ;; and `jiras-agile-pre-process-rules` variables, or by using
  ;; the command `org-jira-customize`.
  ;; It's recommended to use customize to set the JIRA URL, username, etc.
  ;; Example:
  ;; (setq jiras-server-list
  ;;       '((:name "my-jira"
  ;;          :url "https://my-jira.atlassian.net"
  ;;          :user "user@example.com")))
  ;;
  ;; For password, it's recommended to use the Emacs auth-source mechanism.
  ;; You can create a ~/.authinfo.gpg file with contents like:
  ;; machine my-jira.atlassian.net login user@example.com password <API_TOKEN>
  (require 'org-jira))

(provide 'collaboration)
;;; collaboration.el ends here
