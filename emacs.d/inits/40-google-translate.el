(require 'google-translate)
(require 'google-translate-default-ui)

(defvar google-translate-english-chars "[:ascii:]"
  "これらの文字が含まれているときは英語とみなす")
(defun google-translate-enja-or-jaen (&optional string)
  "regionか現在位置の単語を翻訳する。C-u付きでquery指定も可能"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (thing-at-point 'word))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))

(push '("\*Google Translate\*" :height 0.5 :stick t) popwin:special-display-config)
