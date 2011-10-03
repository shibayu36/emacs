;;; clipboard-to-kill-ring.el

;;; * 概要
;;; OS Xのクリップボードを定期的に監視してkill-ringに入れます
;;; deferred.elが必要です
;;;
;;; * 使い方
;;; (clipboard-to-kill-ring t)
;;; で監視を始めます
;;; clipboard-to-kill-ring:interval を設定するとポーリングの間隔を変えられます．
;;; デフォルトで1秒です．

(require 'deferred)

(defcustom clipboard-to-kill-ring:interval 1.0
  "ポーリング間隔")

(defvar clipboard-to-kill-ring:timer nil)

(defun clipboard-to-kill-ring:observe ()
  (let ((clip (x-cut-buffer-or-selection-value)))
    (when (and clip (not (string= (car kill-ring) clip)))
      (kill-new clip)
      (message clip))))

(defun clipboard-to-kill-ring (enable)
  "enableがtrueのときクリップボードの監視を始めます"
  (if (and enable clipboard-to-kill-ring:timer)
      (clipboard-to-kill-ring nil))
  (if enable
      (setq clipboard-to-kill-ring:timer (run-with-timer
                                              clipboard-to-kill-ring:interval
                                              clipboard-to-kill-ring:interval
                                              'clipboard-to-kill-ring:observe))
    (when clipboard-to-kill-ring:timer
      (cancel-timer clipboard-to-kill-ring:timer)
      (setq clipboard-to-kill-ring:timer nil))))

(provide 'clipboard-to-kill-ring)
