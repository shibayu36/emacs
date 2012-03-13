;;初期設定
(require 'yasnippet)
(require 'anything-c-yasnippet)
(setq anything-c-yas-space-match-any-greedy t) ;スペース区切りで絞り込めるようにする デフォルトは nil
(global-set-key (kbd "C-c y") 'anything-c-yas-complete) ;C-c yで起動 (同時にお使いのマイナーモードとキーバインドがかぶるかもしれません)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/") ;snippetsのディレクトリを指定

(defun yas/perl-package-name ()
  (let ((file-path (file-name-sans-extension (buffer-file-name))))
    (if (string-match "lib/" file-path)
        (replace-regexp-in-string "/" "::"
                                  (car (last (split-string file-path "/lib/"))))
      (file-name-nondirectory file-path))))