;;; wb-line-number.el --- window-based line number displaying
;;
;; Copyright (C) 2001 Naoki Nakamura
;;
;; Author:  Naoki Nakamura <naoki.y.nakamura@nifty.com>
;; Created: 12 Jan 2002
;; Version: 1.5.5 , 17 Mar 2003
;; Keywords: line, number, display, window
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; Window-based line number mode, which displays each number of line
;; at the left window. Be sure that this is *not* a minor mode.
;; So far, This elisp does not work well under Xemacs...

;;; Installation:
;;
;; Add this file (byte-compiling it is recommended) to your load-path.
;; Then add one of these set of lines (or similar ones) to your config:
;;
;; (require 'wb-line-number)
;;
;; Type "M-x wb-line-number-toggle" to toggle this mode manually.

;;; Optional Setting:
;;
;; (setq truncate-partial-width-windows nil) ; use continuous line
;; (set-scroll-bar-mode nil)                 ; no scroll bar, even in x-window system
;;

;;; Change Log:
;;
;; From 1.5.4 to 1.5.5
;;
;; - Fixed a bug about selective-display.
;; - New command wb-line-number-enable.
;; - New command wb-line-number-disable.
;; - New hook wb-line-number-before-enable-hook.
;; - New hook wb-line-number-after-enable-hook.
;; - New hook wb-line-number-before-disable-hook.
;; - New hook wb-line-number-after-disable-hook.
;;
;; From 1.5.3 to 1.5.4
;;
;; - New advice wb-line-number-adv-bef-pop-to-buffer.
;;
;; From 1.5.2 to 1.5.3
;;
;; - New face to display scroll bar, instead of #.
;;
;; From 1.5.1 to 1.5.2
;;
;; - New advice wb-line-number-adv-ard-one-window-p.
;; - Omitted some code.
;;
;; From 1.5 to 1.5.1
;;
;; - Fixed bugs about window resetting.
;;
;; From 1.4.3 to 1.5
;;
;; - Enabled multi-window use.
;; - New command wb-line-number-split-window-horizontally.
;; - New advice wb-line-number-adv-bef-switch-to-buffer.
;; - New advice wb-line-number-adv-aft-delete-frame.
;;
;; From 1.4.2 to 1.4.3
;;
;; - Changed some algorithm for speed up, especially in large file.
;;
;; From 1.4.1 to 1.4.2
;;
;; - Added (featurep 'comint) as a part of condition.
;; - Added condition-case statement for more safety.
;;
;; From 1.4 to 1.4.1
;;
;; - Fixed a tiny bug about continuous line displaying.
;;
;; From 1.3 to 1.4
;;
;; - Enabled realtime refresh in shell-mode.
;; - Changed some algorithm for speed up.
;;
;; From 1.2 to 1.3
;;
;; - Enabled multiframe use.
;;
;; From 1.1.2 to 1.2
;;
;; - New advice wb-line-number-adv-ard-execute-kbd-macro.
;; - New advice wb-line-number-adv-ard-call-last-kbd-macro.
;;
;; From 1.1.1 to 1.1.2
;;
;; - Fixed a bug that needed an advice for delete-windows-on.
;;
;; From 1.1 to 1.1.1
;;
;; - Fixed a bug that didn't save window conditions in some case.
;;
;; From 1.0 to 1.1
;;
;; - Fixed a bug that was making emacs20 unstable.
;; - Changed some algorithm for speed up.
;;

;;; Code:

(provide 'wb-line-number)

;;; user option
(defcustom wb-line-number-scroll-bar t
  "*Non-nil means to display text-based scroll bar."
  :type  'boolean
  :group 'wb-line-number)
(defcustom wb-line-number-before-enable-hook nil
  "*Hook run before wb-line-number-enable."
  :type  'hook
  :group 'wb-line-number)
(defcustom wb-line-number-after-enable-hook nil
  "*Hook run after wb-line-number-enable."
  :type  'hook
  :group 'wb-line-number)
(defcustom wb-line-number-before-disable-hook nil
  "*Hook run before wb-line-number-disable."
  :type  'hook
  :group 'wb-line-number)
(defcustom wb-line-number-after-disable-hook nil
  "*Hook run after wb-line-number-disable."
  :type  'hook
  :group 'wb-line-number)
(defface wb-line-number-face
  '((t (:foreground "hotpink")))
  "Face used for each number of lines."
  :group 'face
  :group 'wb-line-number)
(defface wb-line-number-scroll-bar-face
  '((t (:foreground "black" :background "hotpink")))
  "Face used for scroll bar."
  :group 'face
  :group 'wb-line-number)
(defcustom wb-line-number-text-width 6
  "*Width of line number text."
  :type  'number
  :group 'wb-line-number)

;;; internal value
(defvar wb-line-number-window-width     10)
(defvar wb-line-number-real-width        0)
(defvar wb-line-number-format-string   nil)
(defvar wb-line-number-spaces          nil)
(defvar wb-line-number-available-list  nil)
(defvar wb-line-number-status-alist    nil)
(defvar wb-line-number-string-alist    nil)
(defvar wb-line-number-skip            nil)

(defvar wb-line-number-e21-margin
  (if (and (<= 21 emacs-major-version)
           window-system) " " ""))

;;; compatibility
(if (or (string-match "XEmacs" emacs-version)
        (string-match "Lucid" emacs-version))
    (fset 'wb-line-number-edges 'window-pixel-edges)
  (fset 'wb-line-number-edges 'window-edges))

;;; command
(defun wb-line-number-toggle ()
  "Toggle wb-line-number mode.
When wb-line-number is enabled, each line number will appear at the
left window."
  (interactive)
  (if (wb-line-number-enabled-p)
      (wb-line-number-disable)
    (wb-line-number-enable)))

(defun wb-line-number-enable ()
  "Enable wb-line-number mode.
When wb-line-number is already enabled, do nothing."
  (interactive)
  (unless (wb-line-number-enabled-p)
    (run-hooks 'wb-line-number-before-enable-hook)
    (setq wb-line-number-window-width (+ wb-line-number-text-width
                                         (cond
                                          ((and (<= 21 emacs-major-version)
                                                window-system
                                                (boundp 'scroll-bar-mode)
                                                scroll-bar-mode)            5)
                                          ((and (<= 21 emacs-major-version)
                                                window-system)              3)
                                          ((<= 21 emacs-major-version)      2)
                                          ((and window-system
                                                (boundp 'scroll-bar-mode)
                                                scroll-bar-mode)            3)
                                          (t 2)))
          wb-line-number-format-string (concat "%" (int-to-string
                                                    wb-line-number-text-width) "d\n")
          wb-line-number-spaces (concat (make-string wb-line-number-text-width ? ) "\n"))
    ;; hooks
    (or (member 'wb-line-number-post-function post-command-hook)
        (add-hook 'post-command-hook 'wb-line-number-post-function))
    (or (member 'wb-line-number-reset-function window-size-change-functions)
        (add-hook 'window-size-change-functions 'wb-line-number-reset-function))
    (or (not (featurep 'comint))
        (member 'wb-line-number-post-function comint-output-filter-functions)
        (add-hook 'comint-output-filter-functions 'wb-line-number-post-function t))
    ;; advices
    (ad-enable-regexp   "^wb-line-number-adv-")
    (ad-activate-regexp "^wb-line-number-adv-")
    ;; create
    (wb-line-number-create-left-window (selected-window))
    (wb-line-number-reset-function)
    ;; run hook
    (run-hooks 'wb-line-number-after-enable-hook)))

(defun wb-line-number-disable ()
  "Disable wb-line-number mode.
When wb-line-number is not enabled, do nothing."
  (interactive)
  (when (wb-line-number-enabled-p)
    (run-hooks 'wb-line-number-before-disable-hook)
    (wb-line-number-delete-left-window (selected-window))
    (run-hooks 'wb-line-number-after-disable-hook)))

(defun wb-line-number-split-window-horizontally ()
  "Split current window equally according to split-window-horizontally,
and execute wb-line-number-toggle at the rightmost of the two."
  (interactive)
  (let ((r-win (split-window (selected-window) nil t)))
    (when (and r-win
               (> (window-width r-win) (* 2 wb-line-number-window-width)))
      (shrink-window (/ wb-line-number-window-width 2) t)
      (save-selected-window
        (select-window r-win)
        (wb-line-number-toggle)))))

;;; predicate
(defun wb-line-number-enabled-p (&optional win)
  "Return t if wb-line-number is enabled."
  (if (wb-line-number-get-neighbor-left-window (or win (selected-window)))
      t nil))

;;; advice
(defadvice other-window (after wb-line-number-adv-aft-other-window)
  (when (wb-line-number-window-p (selected-window))
    (other-window (if (and (number-or-marker-p (ad-get-arg 0))
                           (< (ad-get-arg 0) 0))
                      -1 1)
                  (ad-get-arg 1))))
(defadvice delete-other-windows (around wb-line-number-adv-ard-delete-other-windows)
  (let* ((lwin-list (wb-line-number-get-all-window 'wb))
         (create (not (not lwin-list))))
    (while lwin-list
      (set-window-dedicated-p (car lwin-list) nil)
      (setq lwin-list (cdr lwin-list)))
    ad-do-it
    (set-window-dedicated-p (selected-window) nil)
    (and create
         (wb-line-number-create-left-window (selected-window)))))
(defadvice delete-window (around wb-line-number-adv-ard-delete-window)
  (let* ((win-list (wb-line-number-get-all-window))
         (win    (or (ad-get-arg 0) (selected-window)))
         (lwin   (wb-line-number-get-neighbor-left-window win))
         (del    (and lwin
                      (not (cddr (assq lwin wb-line-number-available-list))))))
    (when (or (and lwin       (<= 3 (length win-list)))
              (and (not lwin) (<= 2 (length win-list))))
      (and del (wb-line-number-delete-window-without-advice lwin))
      ad-do-it)
    ;; clean left win
    (let ((win-list (wb-line-number-get-all-window)))
      (while win-list
        (when (and (wb-line-number-buffer-p (window-buffer (car win-list)))
                   (wb-line-number-get-neighbor-left-window (car win-list)))
          (wb-line-number-delete-window-without-advice (car win-list)))
        (setq win-list (cdr win-list))))))
(defadvice delete-windows-on (around wb-line-number-adv-ard-delete-windows-on)
  ad-do-it
  (when (and (wb-line-number-buffer-p (window-buffer (selected-window)))
             (window-dedicated-p (selected-window))
             (not (wb-line-number-window-p (selected-window))))
    (set-window-dedicated-p (selected-window) nil)
    (wb-line-number-create-left-window (selected-window))))
(defadvice execute-kbd-macro (around wb-line-number-adv-ard-execute-kbd-macro)
  (let ((wb-line-number-skip t))
    ad-do-it))
(defadvice call-last-kbd-macro (around wb-line-number-adv-ard-call-last-kbd-macro)
  (let ((wb-line-number-skip t))
    ad-do-it))
(defadvice switch-to-buffer (before wb-line-number-adv-bef-switch-to-buffer)
  (and (window-dedicated-p (selected-window))
       (not (wb-line-number-window-p (selected-window)))
       (set-window-dedicated-p (selected-window) nil)))
(defadvice delete-frame (after wb-line-number-adv-aft-delete-frame)
  (wb-line-number-reset-function))
(defadvice one-window-p (around wb-line-number-adv-ard-one-window-p)
  ;; this advice may be debatable...
  (if (and (= 1 (length wb-line-number-available-list))
           (= 2 (length (car wb-line-number-available-list))))
      (setq ad-return-value t)
    ad-do-it))
(defadvice pop-to-buffer (before wb-line-number-adv-bef-pop-to-buffer)
  (let ((l (wb-line-number-get-all-window)))
    (while l
      (when (and (wb-line-number-window-p (car l))
                 (not (window-dedicated-p (car l))))
        (set-window-dedicated-p (car l) t))
      (when (and (not (wb-line-number-window-p (car l)))
                 (window-dedicated-p (car l)))
        (set-window-dedicated-p (car l) nil))
      (setq l (cdr l)))))

;;; internal function
(defun wb-line-number-window-p (win)
  (and (window-live-p win)
       (= wb-line-number-real-width (window-width win))
       (wb-line-number-buffer-p (window-buffer win))))
(defun wb-line-number-buffer-p (buf)
  (save-match-data
    (string-match "^ \\*wb-line-number-.*\\*$" (buffer-name buf))))
(defun wb-line-number-neighbor-p (win lwin)
  (and (eq (window-frame win) (window-frame lwin))
       (=  (nth 0 (wb-line-number-edges win))
           (nth 2 (wb-line-number-edges lwin)))
       (>= (nth 1 (wb-line-number-edges win))
           (nth 1 (wb-line-number-edges lwin)))
       (<  (nth 1 (wb-line-number-edges win))
           (nth 3 (wb-line-number-edges lwin)))))
(defvar wb-line-number-frame-id-alist
  (list (cons (window-frame (selected-window)) 0)))
(defun wb-line-number-frame-id (win)
  (or (cdr (assq (window-frame win) wb-line-number-frame-id-alist))
      (let ((n (1+ (apply 'max (mapcar 'cdr wb-line-number-frame-id-alist)))))
        (setq wb-line-number-frame-id-alist
              (cons (cons (window-frame win) n)
                    wb-line-number-frame-id-alist))
        n)))
(defun wb-line-number-get-buffer-name (win)
  (concat " *wb-line-number-"
          (int-to-string (wb-line-number-frame-id win))      "-"
          (int-to-string (nth 0 (wb-line-number-edges win))) "-"
          (int-to-string (nth 1 (wb-line-number-edges win))) "*"))
(defun wb-line-number-get-neighbor-left-window (win &optional normal)
  (let ((win-list (wb-line-number-get-all-window))
        (ret-win  nil))
    (while win-list
      (when (and (wb-line-number-neighbor-p win (car win-list))
                 (or normal (wb-line-number-window-p (car win-list))))
        (setq ret-win  (car win-list)
              win-list nil))
      (setq win-list (cdr win-list)))
    ret-win))

(defun wb-line-number-create-left-window (l-win)
  (let ((r-win     nil) ; newly created window
        (win-start (window-start l-win))
        (frm       (window-frame l-win))
        l-buf)
    (setq window-min-width wb-line-number-window-width
          r-win   (split-window l-win wb-line-number-window-width t)
          wb-line-number-real-width (window-width l-win))
    (set-window-start  r-win win-start)
    (setq l-buf (get-buffer-create (wb-line-number-get-buffer-name l-win)))
    (set-window-buffer l-win l-buf)
    (set-buffer l-buf)
    (setq truncate-lines t)
    (set-window-dedicated-p l-win t  )
    (set-window-dedicated-p r-win nil)
    (select-window r-win)))

(defun wb-line-number-delete-left-window (win)
  (let* ((l-win (wb-line-number-get-neighbor-left-window win))
         (size  (window-width win))
         win-list)
    (when l-win
      (set-window-dedicated-p l-win nil)
      (bury-buffer (window-buffer l-win))
      (delete-window l-win)
      ;; adjust window size
      (when (= size (window-width win))
        (setq win-list (wb-line-number-get-all-window 'normal))
        (while win-list
          (when (and (= (nth 0 (wb-line-number-edges win))
                        (nth 2 (wb-line-number-edges (car win-list))))
                     (= (nth 1 (wb-line-number-edges win))
                        (nth 1 (wb-line-number-edges (car win-list)))))
            (save-selected-window
              (select-window (car win-list))
              (shrink-window wb-line-number-window-width t))
            (setq win-list nil))
          (setq win-list (cdr win-list)))))))

(defun wb-line-number-get-all-window (&optional arg)
  (let ((win-list nil)
        (out-list nil)
        (first-win (if (window-minibuffer-p (selected-window))
                       (next-window (selected-window) 0 t)
                     (selected-window))))
    (while (not (eq (car win-list) first-win))
      (setq win-list (cons (next-window (car win-list) 0 t)
                           win-list)))
    (cond ((eq arg 'wb)
           (while win-list
             (when (wb-line-number-window-p (car win-list))
               (setq out-list (cons (car win-list) out-list)))
             (setq   win-list (cdr win-list))))
          ((eq arg 'normal)
           (while win-list
             (unless (wb-line-number-window-p (car win-list))
               (setq out-list (cons (car win-list) out-list)))
             (setq   win-list (cdr win-list))))
          (t (setq out-list win-list)))
    out-list))
(defun wb-line-number-delete-window-without-advice (win)
  (let (ret)
    (ad-disable-regexp  "^wb-line-number-adv-")
    (ad-activate-regexp "^wb-line-number-adv-")
    (set-window-dedicated-p win nil)
    (bury-buffer (window-buffer win))
    (setq ret (delete-window win))
    (ad-enable-regexp   "^wb-line-number-adv-")
    (ad-activate-regexp "^wb-line-number-adv-")
    ret))
(fset 'wb-line-number-one-window-p-without-advice
      (symbol-function 'one-window-p))

(defun wb-line-number-reset-function (&rest arg)
  (or (window-minibuffer-p (selected-window))
      ;; reset available window list and top list
      (let ((win-list (wb-line-number-get-all-window 'normal))
            lwin-list)
        ;; adjust resized left win
        (while win-list
          (when (and (not (wb-line-number-one-window-p-without-advice))
                     (wb-line-number-buffer-p (window-buffer (car win-list)))
                     (not (= wb-line-number-real-width
                             (window-width (car win-list)))))
            (let* ((win (car win-list))
                   (dif (- wb-line-number-real-width (window-width win))))
              (when (and (< 0 (nth 0 (wb-line-number-edges win)))
                         (string= (buffer-name (window-buffer win))
                                  (wb-line-number-get-buffer-name win)))
                ;; resized window is rightmost, so target is neighbor-left
                (setq win (wb-line-number-get-neighbor-left-window win t)
                      dif (- dif)))
              (save-selected-window
                (select-window win)
                (enlarge-window dif t))))
          (setq win-list (cdr win-list)))
        ;; clean invalid left window
        (setq win-list (wb-line-number-get-all-window))
        (while win-list
          (when (and (wb-line-number-buffer-p (window-buffer (car win-list)))
                     (wb-line-number-get-neighbor-left-window (car win-list)))
            (wb-line-number-delete-window-without-advice (car win-list)))
          (setq win-list (cdr win-list)))
        ;; set available window for each left win
        (setq win-list  (wb-line-number-get-all-window 'normal)
              lwin-list (wb-line-number-get-all-window 'wb)
              wb-line-number-available-list ())
        (while lwin-list
          (let* ((lwin (car lwin-list))
                 (lbuf (get-buffer (wb-line-number-get-buffer-name lwin)))
                 (top-win-alist   ())
                 (win-list-sorted ())
                 (top-list        ()))
            ;; correct left buffer name
            (unless (string= (buffer-name (window-buffer lwin))
                             (wb-line-number-get-buffer-name lwin))
              (if lbuf
                  (progn
                    (set-window-dedicated-p lwin nil)
                    (kill-buffer (window-buffer lwin))
                    (set-window-buffer lwin lbuf))
                (save-current-buffer
                  (set-buffer (window-buffer lwin))
                  (rename-buffer (wb-line-number-get-buffer-name lwin)))))
            (set-window-dedicated-p lwin t)
            (let ((w-list win-list)
                  top win)
              (while w-list
                (setq win (car w-list)
                      top (nth 1 (wb-line-number-edges win)))
                (set-window-dedicated-p win nil)
                ;; just neighbor?
                (when (wb-line-number-neighbor-p win lwin)
                  (setq top-win-alist (cons (cons top win) top-win-alist)
                        top-list      (cons top top-list)))
                (setq w-list (cdr w-list))))
            (if (not top-list)
                (wb-line-number-delete-window-without-advice lwin)
              (setq top-list (sort top-list '>))
              (while top-list
                (setq win-list-sorted
                      (cons (cdr (assq (car top-list) top-win-alist))
                            win-list-sorted)
                      top-list (cdr top-list)))
              (setq wb-line-number-available-list
                    (cons (cons lwin win-list-sorted)
                          wb-line-number-available-list)))
            (setq lwin-list (cdr lwin-list)))))
      (wb-line-number-post-function)))

(defun wb-line-number-post-function (&rest args)
  (condition-case nil
      (or
       wb-line-number-skip
       (let ((win-alist wb-line-number-available-list))
         (while (wb-line-number-window-p (selected-window))
           (select-window (next-window (selected-window) 0 1)))
         (while (wb-line-number-buffer-p (current-buffer)) (bury-buffer))
         (when win-alist
           (sit-for 0)                  ; force redisplay
           (let* ((deactivate-mark   nil)
                  (inhibit-redisplay   t)
                  (todo-last nil)
                  (this-stat-alist ())
                  (win-str-alist   ()))
             (while win-alist
               (let ((win-list      (cdar win-alist))
                     (todo-this     nil)
                     (left-str      "")
                     (last-mode-str ""))
                 (while win-list
                   (let* ((win (car win-list))
                          (buf (window-buffer win))
                          (top (nth 1 (wb-line-number-edges win)))
                          (this-mod-tick (buffer-modified-tick buf))
                          (cache-list
                           (cdr (assq win wb-line-number-string-alist)))
                          (last-stat (assq win wb-line-number-status-alist))
                          (this-stat
                           (list win
                                 (window-start win)
                                 (window-end win)
                                 this-mod-tick
                                 buf
                                 (cdr (assq 'truncate-lines
                                            (buffer-local-variables buf)))
                                 (window-height win)
                                 (window-width  win)))
                          this-disp-list)
                     ;; cache available?
                     (if (and (equal this-stat last-stat)
                              cache-list)
                         ;; then, use stored string
                         (setq this-stat-alist (cons last-stat this-stat-alist)
                               this-disp-list  cache-list)
                       ;; else, calculate and create string
                       (setq todo-this t
                             this-stat-alist (cons this-stat this-stat-alist)
                             this-disp-list (wb-line-number-make-string
                                             this-stat
                                             (if (and cache-list
                                                      last-stat
                                                      (= this-mod-tick
                                                         (nth 2 (cdr last-stat)))
                                                      (eq buf
                                                          (nth 3 (cdr last-stat))))
                                                 cache-list nil))))
                     (setq left-str      (concat left-str last-mode-str
                                                 (car this-disp-list))
                           last-mode-str (cadr this-disp-list)
                           win-str-alist (cons (cons win this-disp-list) win-str-alist)
                           win-list      (cdr win-list))))
                 (when todo-this
                   (setq todo-last t)
                   (wb-line-number-left-redisplay (caar win-alist)
                                                  left-str
                                                  last-mode-str))
                 (setq win-alist (cdr win-alist))))
             (when todo-last
               (setq wb-line-number-status-alist this-stat-alist
                     wb-line-number-string-alist win-str-alist  ))))))
    (error nil)))

(defun wb-line-number-left-redisplay (win str mode-str)
  (save-excursion
    (set-buffer (window-buffer win))
    (setq mode-line-format
          (list (concat wb-line-number-e21-margin (substring mode-str 0 -1))))
    (erase-buffer)
    (insert str)
    (force-mode-line-update)
    (bury-buffer (current-buffer))))

;;; internal function of wb-line-number-make-string
(defun wb-line-number-invisible-overlay-p (st ed)
  (let ((ov (overlays-in st ed))
        (ret nil))
    (while ov
      (if (overlay-get (car ov) 'invisible)
          (setq ret t
                ov nil)
        (setq ov (cdr ov))))
    ret))

(defun wb-line-number-make-string (status-list &optional cache-list)
  (save-excursion
    (save-window-excursion
      (let ((win   (nth 0 status-list))
            (start (nth 1 status-list))
            (wh (1- (nth 6 status-list)))
            (left-list ())
            (left-str "")
            (mode-str "")
            (y 1)
            (skip   nil)
            (linear nil)
            base-line max-line botm-m rows end start-bol)
        ;; first step
        (set-buffer (window-buffer win))
        (goto-char start)
        (setq start-bol (line-beginning-position))
        (cond (cache-list
               (let ((c-mode-str  (nth 1 cache-list))
                     (c-max-line  (nth 2 cache-list))
                     (c-start-bol (nth 3 cache-list))
                     (c-base-line (nth 4 cache-list))
                     (c-linear    (nth 5 cache-list)))
                 (setq base-line (+ (* (count-lines c-start-bol start-bol)
                                       (if (> c-start-bol start-bol) -1 1))
                                    c-base-line)
                       left-list (list (if (bolp)
                                           (format wb-line-number-format-string base-line)
                                         wb-line-number-spaces))
                       max-line c-max-line
                       mode-str c-mode-str)
                 (when (and (or ;; continuation line?
                                truncate-partial-width-windows
                                truncate-lines
                                ;; or scrolled up one line?
                                (and c-linear
                                     (= 1 (- c-base-line base-line))
                                     (bolp)))
                            (> max-line (+ base-line wh))
                            (not selective-display)
                            (not (wb-line-number-invisible-overlay-p start (point-max))))
                   ;; avoiding needless vertical-motion
                   (setq rows   wh
                         botm-m 0
                         skip   t))))
              ;; no cache
              (t
               (setq base-line (+ (count-lines 1 start-bol) 1)
                     left-list (list (if (bolp)
                                         (format wb-line-number-format-string base-line)
                                       wb-line-number-spaces)))
               ;; get number of max-line
               (save-restriction
                 (widen)
                 (goto-char (point-max))
                 (setq max-line (+ base-line (count-lines start (line-beginning-position)))
                       mode-str (format wb-line-number-format-string max-line))
                 (when (fboundp 'replace-regexp-in-string)
                   (setq mode-str (replace-regexp-in-string " " "-" mode-str)))
                 (put-text-property 0 (length mode-str) 'face 'modeline mode-str))))
        ;; go!
        (if (or skip
                (progn
                  ;; get rows, bottom margin, end point
                  (goto-char start)
                  (setq rows   (vertical-motion wh win)
                        botm-m (max 0 (- wh rows 1))
                        end    (line-beginning-position))
                  (= rows (count-lines start end))))
            ;; quick version
            (progn
              (while (< y (- wh botm-m))
                (setq left-list (cons (format wb-line-number-format-string
                                              (+ base-line y))
                                      left-list)
                      y (1+ y)))
              (setq linear t))
          ;; else, slow speed version
          (goto-char start)
          (vertical-motion 1 win)
          (let* ((inc 0)
                 (fc (lambda() (setq inc (1+ inc)))))
            ;; invisible text?
            (when (or (wb-line-number-invisible-overlay-p start end)
                      selective-display)
              (setq fc (lambda() (count-lines start (point))))) ;; ok, milk run
            (while (< y (- wh botm-m))
              (setq left-list (cons (if (bolp)
                                        (format wb-line-number-format-string
                                                (+ base-line (funcall fc)))
                                      wb-line-number-spaces)
                                    left-list)
                    y (1+ y))
              (vertical-motion 1 win))))
        (setq left-str (apply 'concat (reverse left-list)))
        (put-text-property 0 (length left-str) 'face 'wb-line-number-face left-str)
        ;; setup for text-based scroll bar
        (when wb-line-number-scroll-bar
          (let* ((i    (max 0 (- (truncate (/ (* base-line wh) max-line)) botm-m)))
                 (dest (min (+ i (ceiling  (/ (* wh wh) max-line))) rows)))
            (while (<= i dest)
              (put-text-property (* (1+ wb-line-number-text-width) i)
                                 (1+ (* (1+ wb-line-number-text-width) i))
                                 'face 'wb-line-number-scroll-bar-face left-str)
              (setq i (1+ i)))))
        ;; return list
        (list (concat left-str (make-string botm-m ?\n))
              mode-str
              max-line
              start-bol
              base-line
              linear)))))

;;; wb-line-number.el ends here
