;;; anything-find-project-resources.el --- finding any resource of a project
;; Time-stamp: <2009-02-19 12:41:12 sakurai>

;; Author: SAKURAI, Masashi <m.sakurai@kiwanami.net>
;; Version: 0.1
;; Keywords: anything, files, project
;; Prefix: anything-find-resource--

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;;
;; The command `anything-find-resource' emulates the eclipse command
;; "Open Resource" [Shift-Ctrl-R].  The project root directory is
;; automatically detected by `anything-find-resource--project-filep'
;; upwards in the directory tree from the directory of
;; `buffer-file-name'.  If the editing file does not belong to some
;; project, namely could not find the project root directory, this
;; command enumerates files in current directory.

;; Maybe following elisps are similar to this program.
;; FindFileInProject : http://www.emacswiki.org/emacs/FindFileInProject
;; project-root.el : http://www.emacswiki.org/emacs/AnythingSources
;; jde-find-class-source : http://www.emacswiki.org/emacs/JdeeVsEclipse

;;; Configuration:
;;
;; (require 'anything)
;; (require 'anything-find-project-resources)
;; (global-set-key (kbd "M-r") 'anything-find-resource)

;;; History:
;; 
;; -- Version 0.1 (2009-02-19)
;;    Initial Version

(require 'cl)

;;; Code:

(defvar anything-find-resource--ignore-files
  '("*.class" "*.o" "*.jar" "*.zip" "*.png" "*.jpg" "*.gif")
  "Collecting a file list, these elements are used by 'find' command.
Example:  \"*.class\"  -->  ! -name \"*.class\"")

(defvar anything-find-resource--ignore-dirs
  '("/." "xrefdb/" "bin/" "classes/")
  "Collecting a file list, these elements are used by 'find' command.
Example:  \"xrefdb/\"  -->  ! -wholename \"*xrefdb/*\"")

(defvar anything-find-resource--project-root-files
  '("build.xml" "prj.el" ".project" "pom.xml"
    "Makefile" "configure" "Rakefile" 
    "NAnt.build")
"This list is employed to find project root directory at 
`anything-find-resource--project-filep'. 
  General: Makefile, configure
  Java: build.xml, prj.el, .project, pom.xml
  Ruby: Rakefile
  C#: NAnt.build .")

(defun anything-find-resource--project-filep (file)
  "Determine whether the given file is a project build file.
The current implementation checks
`anything-find-resource--project-root-files'."
  (find file 
        anything-find-resource--project-root-files
        :test 'string=))

(defun anything-find-resource--find-project-root-dir (dir)
  "Determine whether the given directory is project root dir.
This function checks parent directories recursively. If this
function found the user's home directory or the system root directory, 
returns nil."
  (let* ((expanded-dir (expand-file-name dir))
        (file (some 'anything-find-resource--project-filep (directory-files expanded-dir))))
    (if file expanded-dir
      (if (or 
           (string= expanded-dir "/")
           (string= expanded-dir (expand-file-name "~/"))
           ) nil
        (anything-find-resource--find-project-root-dir 
         (concat (file-name-as-directory dir) ".."))))))

(defun anything-find-resource--build-find-options ()
  "Return the option arguments for find command
from `anything-find-resource--ignore-dirs' and
`anything-find-resource--ignore-files'."
  (labels ((ao (arg1 arg2 lst) (cons arg1 (cons arg2 lst))))
    (ao "-type" "f" 
        (reduce ; i'd rewrite here to "-prune"...
         (lambda(lst i) (ao "!" "-name" (cons i lst)))
         anything-find-resource--ignore-files 
         :initial-value 
         (reduce
          (lambda(lst i) (ao "!" "-wholename" (cons (concat "*" i "*") lst)))
          anything-find-resource--ignore-dirs
          :initial-value nil)))))

(defun anything-find-resource--collect-files-nonproject-directory (dir buffer)
  "Collect all files in the current directory."
  (message dir)
  (apply 'call-process 
         "find" nil buffer nil dir
         (append '("-maxdepth" "1")
                 (anything-find-resource--build-find-options))))

(defun anything-find-resource--collect-files-project-directory (dir buffer) 
  "Collect file entries those are belonging to DIR project."
  (message dir)
  (apply 'call-process 
         "find" nil buffer nil dir
         (anything-find-resource--build-find-options)))

(defvar anything-find-resource--dirname-max-length 40
  "Max length for directory name.")

(defun anything-find-resource--transformer (files)
  "Transforme display text like following: 
 [/project dir/subdirectory../filename] -> [subdirectory : bold filename]"
  (let* (list 
        (max-len 0)
        (cut-len (if anything-c-resource-directory 
                     (length anything-c-resource-directory)  0))
        (entries
         (mapcar 
          (lambda (file)
            (let (entry filename dir search)
              (setq file (expand-file-name file))
              (if (string-match "^\\(.*\\)\\/\\([^/]+\\)$" file)
                  (progn
                    (setq filename (match-string 2 file))
                    (let ((pdir (match-string 1 file)))
                      (setq dir (if (< cut-len (length pdir))
                                    (substring pdir cut-len) "")))
                    (setq search (substring file cut-len)))
                (setq filename (substring file cut-len))
                (setq dir "")
                (setq search filename))
              (setq entry (list filename dir search))
              (setq max-len (max max-len (length dir)))
              (put-text-property 0 (length filename) 'face 'bold filename)
              entry))
          files)))
    (when (< anything-find-resource--dirname-max-length max-len)
      (setq max-len anything-find-resource--dirname-max-length))
    (loop for entry in entries
          do 
          (push 
           (cons 
            (format (format "%%%ds : %%s" max-len)
                    (let ((dir (cadr entry)))
                      (if (< max-len (length dir))
                          (substring dir (- max-len)) dir))
                     (car entry))
            (caddr entry))
           list)
          finally (return (nreverse list)))))
  
(defvar anything-c-resource-directory nil
  "[internal user] temporary variable: base directory 
that is used at anything-find-resource, anything-find-resource--transformer.")

(defun anything-find-resource ()
  "Enumerate resource files those are belonging to the project,
like eclipse's [Shift-Ctrl-R]."
  (interactive)
  (anything 
   '((
      (name . "Resouce files:")
      (init . (lambda () 
                (let* ((current-user-dir
                        (or (and buffer-file-name (file-name-directory (buffer-file-name)))
                            (expand-file-name default-directory)
                            (expand-file-name "~")))
                       (project-root-dir 
                        (anything-find-resource--find-project-root-dir current-user-dir))
                       (target-buffer (anything-candidate-buffer 'local)))
                  (setq anything-c-resource-directory project-root-dir)
                  (if project-root-dir
                      (anything-find-resource--collect-files-project-directory
                       anything-c-resource-directory target-buffer)
                    (setq anything-c-resource-directory (file-name-directory current-user-dir))
                    (anything-find-resource--collect-files-nonproject-directory
                     anything-c-resource-directory target-buffer))
                  )))
      (candidates-in-buffer)
      (action . (("Find file" . 
                  (lambda (i) 
                    (find-file (concat anything-c-resource-directory i))))))
      (candidate-transformer . 
                             (lambda (candidates)
                               (anything-find-resource--transformer candidates)))
      ))
   nil "resource files: " nil nil))

(provide 'anything-find-project-resources)

;;; anything-find-project-resources.el ends here
