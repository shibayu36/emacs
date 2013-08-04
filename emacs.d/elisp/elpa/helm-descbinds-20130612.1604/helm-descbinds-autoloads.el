;;; helm-descbinds-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-descbinds helm-descbinds-uninstall helm-descbinds-install
;;;;;;  helm-descbinds-mode) "helm-descbinds" "helm-descbinds.el"
;;;;;;  (20989 58198))
;;; Generated autoloads from helm-descbinds.el

(defvar helm-descbinds-mode nil "\
Non-nil if Helm-Descbinds mode is enabled.
See the command `helm-descbinds-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-descbinds-mode'.")

(custom-autoload 'helm-descbinds-mode "helm-descbinds" nil)

(autoload 'helm-descbinds-mode "helm-descbinds" "\
Use `helm' for `describe-bindings'

\(fn &optional ARG)" t nil)

(autoload 'helm-descbinds-install "helm-descbinds" "\
Use `helm-descbinds' as a replacement of `describe-bindings'.

\(fn)" t nil)

(autoload 'helm-descbinds-uninstall "helm-descbinds" "\
Restore original `describe-bindings'.

\(fn)" t nil)

(autoload 'helm-descbinds "helm-descbinds" "\
Yet Another `describe-bindings' with `helm'.

\(fn &optional PREFIX BUFFER)" t nil)

;;;***

;;;### (autoloads nil nil ("helm-descbinds-pkg.el") (20989 58198
;;;;;;  217199))

;;;***

(provide 'helm-descbinds-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-descbinds-autoloads.el ends here
