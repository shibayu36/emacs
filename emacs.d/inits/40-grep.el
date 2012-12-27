;; igrep.el
;; (require 'igrep)
;; (igrep-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -0u8"))
;; (igrep-find-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -0u8"))
;; (setq igrep-find-use-xargs nil)

(require 'wgrep)
(setq wgrep-enable-key "r")

(require 'grep-a-lot)
(grep-a-lot-advise igrep)
