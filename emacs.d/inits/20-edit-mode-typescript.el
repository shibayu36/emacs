(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(with-eval-after-load 'typescript-mode
  (dolist (key '("{" "}" "(" ")" ":" ";" ","))
    (define-key typescript-mode-map key nil)))

(require 'tide)
(with-eval-after-load 'tide
  (define-key typescript-mode-map (kbd "C-@") 'tide-jump-to-definition)
  (define-key typescript-mode-map (kbd "M-@") 'tide-jump-back))

(defun run-js-mocha-describe-test ()
  (interactive)
  (let* ((topdir (git-root-directory))
         (test-grep-args nil))
    (save-excursion
      (when (or
             (re-search-backward "\\bdescribe(\s*[\"']\\(.*?\\)[\"']" nil t)
             (re-search-forward "\\bdescribe(\s*[\"']\\(.*?\\)[\"']" nil t))
        (setq test-grep-args (match-string 1))))
    (if test-grep-args
        (quickrun
         :source
         `((:command . "$(npm bin)/karma")
           (:default-directory . ,topdir)
           (:exec . (,(concat "%c run --no-refresh -- --grep " test-grep-args))))))))


(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode t)
            (company-mode-on)
            (local-set-key (kbd "C-c t") 'run-js-mocha-describe-test)))
