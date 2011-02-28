;;初期設定
(require 'yasnippet)
(require 'anything-c-yasnippet)
(setq anything-c-yas-space-match-any-greedy t) ;スペース区切りで絞り込めるようにする デフォルトは nil
(global-set-key (kbd "C-c y") 'anything-c-yas-complete) ;C-c yで起動 (同時にお使いのマイナーモードとキーバインドがかぶるかもしれません)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/") ;snippetsのディレクトリを指定
