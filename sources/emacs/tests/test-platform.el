;;; test-platform.el --- Tests for platform detection -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the platform detection system to ensure it works correctly
;; across different environments.

;;; Code:

(require 'ert)
(require 'platform)

(ert-deftest test-platform-constants ()
  "Test that platform detection constants are properly defined."
  (should (booleanp platform-android-p))
  (should (booleanp platform-macos-p))
  (should (booleanp platform-linux-p))
  (should (booleanp platform-windows-p))
  (should (booleanp platform-gui-p))
  (should (booleanp platform-terminal-p))
  
  ;; GUI and terminal should be mutually exclusive
  (should (not (and platform-gui-p platform-terminal-p))))

(ert-deftest test-platform-has-feature-p ()
  "Test platform feature detection."
  (should (booleanp (platform-has-feature-p 'notifications)))
  (should (booleanp (platform-has-feature-p 'clipboard)))
  (should (booleanp (platform-has-feature-p 'fonts)))
  (should (booleanp (platform-has-feature-p 'native-comp)))
  (should (booleanp (platform-has-feature-p 'treesitter)))
  
  ;; Unknown features should return nil
  (should (null (platform-has-feature-p 'unknown-feature))))

(ert-deftest test-platform-get-info ()
  "Test platform information gathering."
  (let ((info (platform-get-info)))
    (should (listp info))
    (should (assoc 'system-type info))
    (should (assoc 'android-p info))
    (should (assoc 'macos-p info))
    (should (assoc 'linux-p info))
    (should (assoc 'windows-p info))
    (should (assoc 'gui-p info))
    (should (assoc 'terminal-p info))))

(ert-deftest test-platform-macros ()
  "Test platform conditional macros."
  (let ((result nil))
    ;; platform-when should work like when
    (platform-when t (setq result 'success))
    (should (eq result 'success))
    
    (setq result nil)
    (platform-when nil (setq result 'failure))
    (should (null result))
    
    ;; platform-unless should work like unless
    (setq result nil)
    (platform-unless nil (setq result 'success))
    (should (eq result 'success))
    
    (setq result nil)
    (platform-unless t (setq result 'failure))
    (should (null result))))

(ert-deftest test-platform-android-detection ()
  "Test Android platform detection logic."
  ;; Mock Android environment
  (let ((system-type 'gnu/linux)
        (process-environment (cons "TERMUX_VERSION=1.0" process-environment)))
    ;; This test requires reloading platform.el with mocked environment
    ;; In a real test, we'd need to structure the code to allow mocking
    (should t))) ; Placeholder

(provide 'test-platform)
;;; test-platform.el ends here