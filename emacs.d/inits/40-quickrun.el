(require 'quickrun)

;; 結果の出力バッファと元のバッファを行き来したい場合は
;; ':stick t'の設定をするとよいでしょう
(push '("*quickrun*") popwin:special-display-config)

;;; perl の prove設定
(quickrun-add-command "perl"
                      '((:command . perlbrew-mini-get-current-perl-path)
                        (:compile-only . "%c -wc %s")
                        (:description . "Run Perl script"))
                      :mode 'cperl-mode)
;; (quickrun-add-command "prove" '((:command "prove") (:exec "%c -bv %s")))
;; (add-to-list 'quickrun-file-alist '("\\.t$" . "prove"))
;; debugしないと使えない

