;;; platform.el --- Platform detection and utilities -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Platform detection utilities for cross-platform Emacs configuration.
;; Provides constants and functions to detect different operating systems
;; and environments, enabling platform-specific configuration loading.

;;; Code:

(defconst platform-android-p
  (and (eq system-type 'gnu/linux)
       (or (getenv "TERMUX_VERSION")
           (string-match-p "android" (or (getenv "PREFIX") ""))))
  "Non-nil if running on Android (via Termux or similar).")

(defconst platform-macos-p
  (eq system-type 'darwin)
  "Non-nil if running on macOS.")

(defconst platform-linux-p
  (and (eq system-type 'gnu/linux)
       (not platform-android-p))
  "Non-nil if running on GNU/Linux (excluding Android).")

(defconst platform-windows-p
  (memq system-type '(cygwin windows-nt ms-dos))
  "Non-nil if running on Windows.")

(defconst platform-gui-p
  (display-graphic-p)
  "Non-nil if running in GUI mode.")

(defconst platform-terminal-p
  (not (display-graphic-p))
  "Non-nil if running in terminal mode.")

(defun platform-has-feature-p (feature)
  "Check if platform supports FEATURE.
FEATURE can be: 'notifications, 'clipboard, 'fonts, 'native-comp, 
'treesitter, 'image-support, 'process-support, etc."
  (pcase feature
    ('notifications (or platform-macos-p platform-linux-p))
    ('clipboard (not platform-android-p))
    ('fonts platform-gui-p)
    ('native-comp (and (fboundp 'native-comp-available-p)
                       (native-comp-available-p)))
    ('treesitter (treesit-available-p))
    ('image-support (display-images-p))
    ('process-support (not platform-windows-p))
    ('mouse-support platform-gui-p)
    ('external-browser (not platform-android-p))
    (_ nil)))

(defmacro platform-when (condition &rest body)
  "Execute BODY when platform CONDITION is true."
  (declare (indent 1) (debug t))
  `(when ,condition ,@body))

(defmacro platform-unless (condition &rest body)
  "Execute BODY unless platform CONDITION is true."
  (declare (indent 1) (debug t))
  `(unless ,condition ,@body))

(defmacro platform-cond (&rest clauses)
  "Platform-specific conditional similar to `cond'.
Each clause is (PLATFORM-CONDITION BODY...)."
  (declare (debug (((&rest form) &rest form))))
  `(cond ,@clauses))

(defun platform-get-info ()
  "Return alist with platform information for debugging."
  `((system-type . ,system-type)
    (android-p . ,platform-android-p)
    (macos-p . ,platform-macos-p)
    (linux-p . ,platform-linux-p)
    (windows-p . ,platform-windows-p)
    (gui-p . ,platform-gui-p)
    (terminal-p . ,platform-terminal-p)
    (termux-version . ,(getenv "TERMUX_VERSION"))
    (prefix . ,(getenv "PREFIX"))
    (window-system . ,window-system)
    (emacs-version . ,emacs-version)))

(provide 'platform)
;;; platform.el ends here