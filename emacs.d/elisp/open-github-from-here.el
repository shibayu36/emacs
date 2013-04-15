;;; open-github-from-here.el ---

;; Copyright (C) 2013 by Yuki SHIBAZAKI

;; Author: Yuki SHIBAZAKI <shibayu36@gmail.com>
;; URL:
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun open-github-from-here:chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

(defun open-github-from-here:git-project-p ()
  (string=
   (open-github-from-here:chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(defun open-github-from-here ()
  (interactive)
  (cond ((and (open-github-from-here:git-project-p) (use-region-p))
         (shell-command
          (format "open-github-from-file %s %d %d"
                  (file-name-nondirectory (buffer-file-name))
                  (line-number-at-pos (region-beginning))
                  (line-number-at-pos (region-end)))))
        ((open-github-from-here:git-project-p)
         (shell-command
          (format "open-github-from-file %s %d"
                  (file-name-nondirectory (buffer-file-name))
                  (line-number-at-pos))))))

(provide 'open-github-from-here)

;;; open-github-from-here.el ends here
