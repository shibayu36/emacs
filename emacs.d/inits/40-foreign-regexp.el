(require 'foreign-regexp)

(custom-set-variables
 ;; 正規表現、perlかrubyを選択
 '(foreign-regexp/regexp-type 'perl) ;; Choose by your preference.
 '(reb-re-syntax 'foreign-regexp)) ;; Tell re-builder to use foreign regexp.
