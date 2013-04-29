(require 'elscreen)
(elscreen-start)

(if window-system
    (define-key elscreen-map (kbd "C-j") 'iconify-or-deiconify-frame)
  (define-key elscreen-map (kbd "C-j") 'suspend-emacs))

;;; なんかうまくいってないので今度考える
(defvar elscreen-my-title-maps '()) ; タイトルの変換をする

(defun my-elscreen-truncate-screen-name (screen-name truncate-length &optional padding)
  (let ((truncate-length (max truncate-length 4)))
    (cond
     ((> (string-width screen-name) truncate-length)
      (concat (truncate-string-to-width screen-name truncate-length nil) "~"))
     (padding
      (truncate-string-to-width screen-name truncate-length nil ?\ ))
     (t
      screen-name))))

(defun elscreen-frame-title-update ()
  (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
    (let*
        ((screen-list (sort (elscreen-get-screen-list) '<))
         (screen-to-name-alist (elscreen-get-screen-to-name-alist))
         (tab-width (elscreen-e21-tab-width))
         (title (mapconcat
                 (lambda (screen)
                   (format
                    "%s"
                    (let ((label
                           (elscreen-e21-tab-escape-%
                            (my-elscreen-truncate-screen-name
                             (reduce (lambda (x f) (funcall f x)) elscreen-my-title-maps
                                     :initial-value (get-alist screen screen-to-name-alist))
                             tab-width t))))
                      (if (eq screen (elscreen-get-current-screen))
                          (concat "【" label "】") label))))
                 screen-list " ")))
      (if (fboundp 'set-frame-name)
          (set-frame-name title)
        (setq frame-title-format title)))))

(eval-after-load "elscreen"
  '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))

;; elscreen-my-title-maps の使い方例

(defun skype--elscreen-title-name-map (x)
  (if (string-match "Skype\\(Chat\\|Message\\):\\[\\(.*\\)\\]$" x)
      (let* ((title (match-string 2 x))
             (buf (get-buffer x))
             (missed (if buf (skype--chat-missed-p
                              (buffer-local-value 'skype-chat-handle buf))
                       nil)))
        (concat (if missed "★" "☆") title))
    x))
(add-to-list 'elscreen-my-title-maps 'skype--elscreen-title-name-map)

