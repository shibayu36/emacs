(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(with-eval-after-load 'typescript-mode
  (dolist (key '("{" "}" "(" ")" ":" ";" ","))
    (define-key typescript-mode-map key nil)))

(require 'tide)
(with-eval-after-load 'tide
  (define-key tide-mode-map (kbd "C-@") 'tide-jump-to-definition)
  (define-key tide-mode-map (kbd "M-@") 'tide-jump-back))

(defun run-js-mocha-describe-test ()
  (interactive)
  (let* ((topdir (vc-root-dir))
         (test-grep-args nil))
    (save-excursion
      (when (or
             ;; 直近もしくは直後でdescribe('テスト名')となっている場所を探し、テスト名を抜き出す
             (re-search-backward "\\bdescribe(\s*[\"']\\(.*?\\)[\"']" nil t)
             (re-search-forward "\\bdescribe(\s*[\"']\\(.*?\\)[\"']" nil t))
        (setq test-grep-args (match-string 1))))
    (if test-grep-args
        ;; テスト名があったらquickrunを用いて
        ;; $(npm bin)/karma run -- --grep 'テスト名'
        ;; のようなコマンドを実行する
        (quickrun
         :source
         `((:command . "$(npm bin)/karma")
           (:default-directory . ,topdir)
           (:exec . (,(concat "%c run -- --grep " test-grep-args))))))))

;;; typescriptでのalignルール
(add-hook
 'align-load-hook
 (lambda ()
   (add-to-list
    'align-rules-list
    '(typescript-equal-delimiter
      (regexp . "\\(\\s-*\\)=")
      (repeat . t)
      (modes  . '(typescript-mode))))
   (add-to-list
    'align-rules-list
    '(typescript-equal-delimiter
      (regexp . ":\\(\\s-*\\)")
      (repeat . t)
      (modes  . '(typescript-mode))))))

(defun shibayu36/typescript-mode-hook ()
  (tide-setup)
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode t)
  (company-mode-on)
  (local-set-key (kbd "C-c t") 'run-js-mocha-describe-test))

(add-hook 'typescript-mode-hook 'shibayu36/typescript-mode-hook)

;;; tsxの設定
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (shibayu36/typescript-mode-hook)
              (local-set-key (kbd "C-c t") 'run-js-mocha-describe-test)
              )))

;;; typescriptでのコンパイルルール
;; (require 'compile)
;; (setq compilation-error-regexp-alist
;;       (append
;;        '(;; d:/h...ript/sample.ts (13,175):
;;          ("^\\(.*\\)(\\([0-9]+\\),[0-9]+):" 1 2))
;;        compilation-error-regexp-alist))
;; (add-hook
;;  'typescript-mode-hook
;;  (lambda ()
;;    (set (make-local-variable 'compile-command)
;;         (format "$(npm bin)/tsc -p %s"
;;                 (vc-root-dir)))))

;;; tslintのエラールール
;; (setq compilation-error-regexp-alist
;;       (append
;;        '(;; src/ts/app.ts[5, 12]: Missing semicolon
;;          ("^\\(.*\\)\\[\\([0-9]+\\), [0-9]+]:" 1 2))
;;        compilation-error-regexp-alist))
