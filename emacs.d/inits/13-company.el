(require 'company)
(require 'popup)
;; (global-company-mode +1)

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-h") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)

(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(setq company-idle-delay 0.3)

(copy-face 'popup-menu-face 'company-tooltip)
(copy-face 'popup-menu-face 'company-tooltip-common)
(copy-face 'popup-menu-selection-face 'company-tooltip-selection)
(copy-face 'popup-menu-selection-face 'company-tooltip-common-selection)
(copy-face 'popup-menu-summary-face 'company-tooltip-annotation)
(copy-face 'popup-menu-selection-face 'company-tooltip-annotation-selection)
(copy-face 'popup-scroll-bar-background-face 'company-scrollbar-bg)
(copy-face 'popup-scroll-bar-foreground-face 'company-scrollbar-fg)
(set-face-attribute
 'company-preview nil
 :foreground "darkgray"
 :background nil
 :underline t)
(set-face-attribute
 'company-preview-common nil
 :foreground "darkgray"
 :background nil
 :underline t)
