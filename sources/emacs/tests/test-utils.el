;;; test-utils.el --- Tests for utils.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for utility functions in utils.el using ERT.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'utils)

;; Set up minimal org-mode variables for testing
(defvar org-directory (expand-file-name "~/Documents")
  "Directory for org files.")
(defvar org-agenda-files nil
  "List of org agenda files.")

;; Test helper functions
(defun test-utils--create-temp-directory-structure ()
  "Create a temporary directory structure for testing.
Returns the path to the temporary directory."
  (let* ((temp-dir (make-temp-file "emacs-test-" t))
         (subdir (file-name-as-directory (expand-file-name "subdir" temp-dir)))
         (hidden-dir (file-name-as-directory (expand-file-name ".hidden" temp-dir)))
         (nested-hidden-dir (file-name-as-directory (expand-file-name ".git/objects" temp-dir))))
    
    ;; Create directories
    (make-directory subdir t)
    (make-directory hidden-dir t)
    (make-directory nested-hidden-dir t)
    
    ;; Create org files in various locations
    (with-temp-file (expand-file-name "test1.org" temp-dir)
      (insert "* Test 1\n"))
    (with-temp-file (expand-file-name "test2.org" subdir)
      (insert "* Test 2\n"))
    (with-temp-file (expand-file-name "hidden.org" hidden-dir)
      (insert "* Hidden\n"))
    (with-temp-file (expand-file-name "nested-hidden.org" nested-hidden-dir)
      (insert "* Nested Hidden\n"))
    
    ;; Create non-org files
    (with-temp-file (expand-file-name "test.txt" temp-dir)
      (insert "Not an org file\n"))
    (with-temp-file (expand-file-name "README.md" subdir)
      (insert "# README\n"))
    
    temp-dir))

(defun test-utils--cleanup-temp-directory (temp-dir)
  "Cleanup temporary directory TEMP-DIR."
  (when (and temp-dir (file-exists-p temp-dir))
    (delete-directory temp-dir t)))

;; Tests for my/find-org-files-recursively
(ert-deftest test-my/find-org-files-recursively-basic ()
  "Test that my/find-org-files-recursively finds org files and ignores hidden folders."
  (let ((temp-dir (test-utils--create-temp-directory-structure)))
    (unwind-protect
        (let ((org-files (my/find-org-files-recursively temp-dir)))
          ;; Should find exactly 2 org files (test1.org and subdir/test2.org)
          (should (= (length org-files) 2))
          (should (cl-some (lambda (file) (string-match-p "test1\\.org$" file)) org-files))
          (should (cl-some (lambda (file) (string-match-p "test2\\.org$" file)) org-files))
          ;; Should not find hidden files (.hidden/hidden.org, .git/objects/nested-hidden.org)
          (should-not (cl-some (lambda (file) (string-match-p "hidden\\.org$" file)) org-files))
          (should-not (cl-some (lambda (file) (string-match-p "nested-hidden\\.org$" file)) org-files))
          ;; All returned files should exist and be org files
          (should (cl-every #'file-exists-p org-files))
          (should (cl-every (lambda (file) (string-match-p "\\.org$" file)) org-files)))
      (test-utils--cleanup-temp-directory temp-dir))))

(ert-deftest test-my/find-org-files-recursively-nonexistent-directory ()
  "Test that my/find-org-files-recursively returns nil for nonexistent directory."
  (let ((result (my/find-org-files-recursively "/nonexistent/directory")))
    (should (null result))))

(ert-deftest test-my/find-org-files-recursively-empty-directory ()
  "Test that my/find-org-files-recursively returns nil for empty directory."
  (let ((temp-dir (make-temp-file "emacs-test-empty-" t)))
    (unwind-protect
        (let ((result (my/find-org-files-recursively temp-dir)))
          (should (null result)))
      (test-utils--cleanup-temp-directory temp-dir))))

(ert-deftest test-my/find-org-files-recursively-nil-input ()
  "Test that my/find-org-files-recursively handles nil input gracefully."
  (let ((result (my/find-org-files-recursively nil)))
    (should (null result))))

(ert-deftest test-my/find-org-files-recursively-file-input ()
  "Test that my/find-org-files-recursively returns nil when given a file instead of directory."
  (let ((temp-file (make-temp-file "emacs-test-file-" nil ".org")))
    (unwind-protect
        (let ((result (my/find-org-files-recursively temp-file)))
          (should (null result)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;; Tests for my/update-org-agenda-files
(ert-deftest test-my/update-org-agenda-files-with-temp-directory ()
  "Test that my/update-org-agenda-files correctly updates agenda files from org-directory."
  (let ((temp-dir (test-utils--create-temp-directory-structure))
        (original-org-directory org-directory)
        (original-org-agenda-files org-agenda-files))
    (unwind-protect
        (progn
          ;; Set up test environment
          (setq org-directory temp-dir)
          (setq org-agenda-files nil)
          
          ;; Test the function
          (my/update-org-agenda-files)
          
          ;; Verify results: should find exactly 2 org files and update agenda
          (should (= (length org-agenda-files) 2))
          (should (cl-every #'file-exists-p org-agenda-files))
          (should (cl-every (lambda (file) (string-match-p "\\.org$" file)) org-agenda-files))
          ;; Should include both test1.org and test2.org
          (should (cl-some (lambda (file) (string-match-p "test1\\.org$" file)) org-agenda-files))
          (should (cl-some (lambda (file) (string-match-p "test2\\.org$" file)) org-agenda-files)))
      
      ;; Cleanup: restore original state
      (setq org-directory original-org-directory)
      (setq org-agenda-files original-org-agenda-files)
      (test-utils--cleanup-temp-directory temp-dir))))

(ert-deftest test-my/update-org-agenda-files-with-nonexistent-directory ()
  "Test that my/update-org-agenda-files preserves agenda files when org-directory doesn't exist."
  (let ((original-org-directory org-directory)
        (original-org-agenda-files org-agenda-files))
    (unwind-protect
        (progn
          ;; Set up test environment with nonexistent directory
          (setq org-directory "/nonexistent/directory")
          (setq org-agenda-files '("dummy-file.org"))
          
          ;; Test the function
          (my/update-org-agenda-files)
          
          ;; org-agenda-files should remain unchanged when no files found
          (should (equal org-agenda-files '("dummy-file.org"))))
      
      ;; Cleanup: restore original state
      (setq org-directory original-org-directory)
      (setq org-agenda-files original-org-agenda-files))))

;; Add a test for error conditions
(ert-deftest test-my/find-org-files-recursively-symlink-handling ()
  "Test that my/find-org-files-recursively handles symbolic links correctly."
  :tags '(slow filesystem)
  (skip-unless (eq system-type 'gnu/linux))  ; Only run on GNU/Linux systems
  (let ((temp-dir (make-temp-file "emacs-test-symlink-" t)))
    (unwind-protect
        (progn
          ;; Create a test file and a symlink to it
          (with-temp-file (expand-file-name "real.org" temp-dir)
            (insert "* Real org file\n"))
          (let ((link-path (expand-file-name "link.org" temp-dir)))
            (make-symbolic-link (expand-file-name "real.org" temp-dir) link-path)
            (let ((result (my/find-org-files-recursively temp-dir)))
              ;; Should find both the real file and the symlink (truename resolves symlinks)
              (should (>= (length result) 1))
              (should (cl-some (lambda (file) (string-match-p "real\\.org$" file)) result)))))
      (test-utils--cleanup-temp-directory temp-dir))))

(provide 'test-utils)
;;; test-utils.el ends here