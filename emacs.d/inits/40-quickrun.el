;; (require 'quickrun)

;; 結果の出力バッファと元のバッファを行き来したい場合は
;; ':stick t'の設定をするとよいでしょう
;; (push '("*quickrun*" :height 0.4 :stick t) popwin:special-display-config)

;;; perl の prove設定
;; (quickrun-add-command "perl"
;;                       '((:command . perlbrew-mini-get-current-perl-path)
;;                         (:compile-only . "%c -wc %s")
;;                         (:description . "Run Perl script"))
;;                       :mode 'cperl-mode)
;; (quickrun-add-command "prove"
;;                       '((:command . (lambda ()
;;                                       (concat
;;                                        perlbrew-mini-perls-dir
;;                                        perlbrew-mini-current-version
;;                                        "/bin/prove")))
;;                         (:exec . "%c -bv %s")))
;; (quickrun-add-command "prove"
;;                       '((:command . "~/perl5/perlbrew/perls/perl-5.8.9/bin/prove")
;;                         (:exec . "%c -bv %s")))
;; (add-to-list 'quickrun-file-alist '("\\.t$" . "prove"))
;; debugしないと使えない

