;;; split-root.el --- root window splitter

;; Copyright (C) 2006 Nikolaj Schumacher <bugs * nschum , de>

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Usage

;; Use `split-root-window' to split the root window.  This creates a pair of
;; windows, a new one, and one containing your previous window configuration
;; (as long as it fits).

;;; Known Issues

;; Splitting the root window actually destroys all windows and recreates them.
;; Therefore all windows will be considered dead after splitting.

(require 'cl)

(defun window-settings (window)
  "Stores WINDOW's buffer, point and window-start in a list"
  (list (window-buffer window) (window-point window) ;;(window-start window)
        (eq (selected-window) window)))

(defun set-window-settings (conf &optional window)
  "Sets buffer, point and window-start to conf, given by ``window-settings''."
  (set-window-buffer window (car conf))
;;   (set-window-start window ( conf))
  ;; set point last, so its guaranteed to stay the same
  (set-window-point window (cadr conf))
  (when (caddr conf) (select-window window t)))

(defun calculate-window-tree (tree)
  "Calculate a version of `window-tree' for use by `split-root-window'."
  (if (windowp tree)
      (let ((edges (window-edges tree)))
        `(,(window-settings tree)
          ,(- (nth 2 edges) (nth 0 edges))
          ,(- (nth 3 edges) (nth 1 edges))))
    (let* (;;(width 0) (height 0)
           (pos (cadr tree))
           (width (- (nth 2 pos) (nth 0 pos)))
           (height (- (nth 3 pos) (nth 1 pos)))
           (horflag (not (car tree)))
           (children (mapcar '(lambda (child)
                                (let ((size (calculate-window-tree child)))
                                  size)) (cddr tree))))
      `(,horflag ,width ,height ,@children))))

(defun build-window-tree (tree window)
  "Restore window settings from a tree created by `calculate-window-tree'."
  (if (atom (car tree))
      ;; tree
      (let ((horflag (car tree))
            (children (cdddr tree)))
        ;; create windows for children
        (while (cdr children)
          ;; recursive descent
          (let* ((child (car children))
                 (child-width (cadr child))
                 (child-height (caddr child))
                 (next-window
                  (split-window window
                                (if horflag child-width child-height)
                                horflag)))
            (build-window-tree child window)
            (setq window next-window)
            (pop children)))
        (build-window-tree (car children) window))
    ;; single window
    (set-window-settings (car tree) window)))

(defun split-root-scale-window-tree (tree width height)
  "Scale a result of `calculate-window-tree' to fit into smaller space."
  (if (atom (car tree))
      ;; tree
      (let* ((horflag (car tree))
             (w (cadr tree))
             (h (caddr tree))
             (children (cdddr tree)))
        (if (car tree)
            (split-root-scale-window-tree-h tree width height)
          (split-root-scale-window-tree-v tree width height)))
    ;; single window
    `(,(car tree) ,width ,height)))

(defun split-root-scale-window-tree-h (tree width height)
  "`split-root-scale-window-tree' for horizontally split tree nodes."
  (let* ((w (cadr tree))
         (children (cdddr tree))
         result)
    (assert (not (eq w 0)))
    (assert (not (eq width 0)))
    (assert (not (eq h 0)))
    (assert (not (eq height 0)))
    (while children
      (let ((child (car children)))
        (pop children)
        (let* ((child-width (cadr child))
               (new-child-width
                (if (null children)
                    ;; last child
                    width
                  (round (/ (float (* child-width width))
                            w)))))
          (message "width %s" new-child-width)
          ;; and resize the child, if big enough
          (unless (< new-child-width window-min-width)
            ;; subtract what we used up
            (setq w (- w child-width))
            (setq width (- width new-child-width))
            (push (split-root-scale-window-tree child new-child-width height)
                  result)))))
    `(t ,width ,height ,@(nreverse result))))

(defun split-root-scale-window-tree-v (tree width height)
  "`split-root-scale-window-tree' for vertically split tree nodes."
  (let* ((h (caddr tree))
         (children (cdddr tree))
         result)
    (assert (not (eq w 0)))
    (assert (not (eq width 0)))
    (assert (not (eq h 0)))
    (assert (not (eq height 0)))
    (while children
      (let ((child (car children)))
        (pop children)
        (let* ((child-height (caddr child))
               (new-child-height
                (if (null children)
                    ;; last child
                    height
                  (round (/ (float (* child-height height))
                            h)))))
          ;; and resize the child, if big enough
          (message "height %s" new-child-height)
          (unless (< new-child-height window-min-height)
            ;; subtract what we used up
            (setq h (- h child-height))
            (setq height (- height new-child-height))
            (push (split-root-scale-window-tree child width new-child-height)
                  result)))))
    `(nil ,width ,height ,@(nreverse result))))

;;;###autoload
(defun split-root-window (&optional size horflag top-left)
  "Split a window of SIZE lines/columns from the root window.
If optional argument horflag is non-nil, split side by side and put size
columns in the new window.
If optional argument TOP-LEFT is non-nil the window will appear at the
top/left, otherwise of the bottom/right."
  (interactive)
  (let ((tree (calculate-window-tree (car (window-tree)))))
    (delete-other-windows)
    (let* ((edges (window-edges))
           (width (- (nth 2 edges) (nth 0 edges)))
           (height (- (nth 3 edges) (nth 1 edges)))
           (sz (or size (/ (if horflag width height) 2)))
           (remaining (- (if horflag width height) sz))
           (old-window (selected-window))
           (new-window old-window))
      (if top-left
          (setq new-window (split-window new-window sz horflag))
        (setq old-window (split-window new-window remaining horflag)))
      (build-window-tree
       (if horflag
           (split-root-scale-window-tree tree remaining height)
               (split-root-scale-window-tree tree width remaining)) new-window)
      old-window
    )))

(provide 'split-root)
