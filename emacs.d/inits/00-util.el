(defun chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))
