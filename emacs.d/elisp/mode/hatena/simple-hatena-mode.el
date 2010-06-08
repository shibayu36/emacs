;;; -*- coding: utf-8; mode: emacs-lisp; indent-tabs-mode: nil -*-
;;; simple-hatena-mode.el --- Emacs interface to Hatena::Diary Writer

;; Copyright (C) 2007 Kentaro Kuribayashi
;; Author: Kentaro Kuribayashi <kentarok@gmail.com>
;; Keywords: blog, hatena, はてな

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; * simple-hatena-mode.elについて

;; このパッケージは、「はてなダイアリーライター」をEmacsから使えるよう
;; にし、はてなダイアリー/グループ日記を簡単に更新するためのメジャーモー
;; ド、simple-hatena-modeを提供します。simple-hatena-modeは、
;; html-helper-modeの派生モードとして定義されていますので、
;; html-helper-modeが提供する各種機能も利用できます。
;;
;; インストール、設定方法等については、以下のページをご覧ください。
;; http://coderepos.org/share/wiki/SimpleHatenaMode

;;; Code:

;;;; * ヴァージョン

(defconst simple-hatena-version "0.16"
  "simple-hatena-mode.elのヴァージョン。")

(defun simple-hatena-version ()
  "simple-hatena-mode.elのヴァージョンを表示する。"
  (interactive)
  (let ((version-string
         (format "simple-hatena-mode-v%s" simple-hatena-version)))
    (if (interactive-p)
        (message "%s" version-string)
      version-string)))

;;;; * ユーザによるカスタマイズが可能な設定

(defgroup simple-hatena nil
  "はてなダイアリーライターをEmacsから使うためのメジャーモード")

;; カスタマイズ変数
(defcustom simple-hatena-bin "hw.pl"
  "*はてなダイアリーライターのパスを指定する。"
  :type '(file :must-match t)
  :group 'simple-hatena)

(defcustom simple-hatena-root "~/.hatena"
  "*はてなダイアリーライターのデータを置くディレクトリのルートを指
定する。"
  :type 'directory
  :group 'simple-hatena)

(defcustom simple-hatena-default-id nil
  "*はてダラで使うデフォルトのはてなidを指定する。

この変数が設定されている場合、simple-hatenaあるいは
simple-hatena-group実行時に、設定されたidが使われるため、idを選択
する必要がない。

このidを変更するには、simple-hatena-change-default-idを実行する。"
  :type '(restricted-sexp :match-alternatives
                          (stringp 'nil))
  :group 'simple-hatena)

(defcustom simple-hatena-default-group nil
  "*デフォルトグループ名を指定する。"
  :type '(restricted-sexp :match-alternatives
                          (stringp 'nil))
  :group 'simple-hatena)

(defcustom simple-hatena-use-timestamp-permalink-flag t
  "*はてなダイアリーライターのパーマリンクに、タイムスタンプを使う
かどうかを指定するフラグ。"
  :type 'boolean
  :group 'simple-hatena)

(defcustom simple-hatena-time-offset nil
  "*日付を計算する際に用いるオフセット。
6 に設定すると午前6時まで前日の日付として扱われる"
  :type '(restricted-sexp :match-alternatives
                          (integerp 'nil))
  :group 'simple-hatena)

;; はてダラにわたすオプション
(defcustom simple-hatena-option-useragent (simple-hatena-version)
  "*はてなダイアリーライターのユーザエージェントオプションを指定す
る。

実行時に、-aオプションとして使われる。"
  :type 'string
  :group 'simple-hatena)

(defcustom simple-hatena-option-debug-flag nil
  "*はてなダイアリーライターを、デバッグモードで実行するか否かを指
定するフラグ。

はてなダイアリーライター実行時に、-dオプションとしてわたされ、また、
その場合、実行結果をバッファに表示する。

デバッグモードをオン/オフするには、
simple-hatena-toggle-debug-modeを実行する。"
  :type 'boolean
  :group 'simple-hatena)

(defcustom simple-hatena-option-timeout 30
  "*はてなダイアリーライターのタイムアウトを指定する。

実行時に、-Tオプションとして使われる。"
  :type 'integer
  :group 'simple-hatena)

(defcustom simple-hatena-option-cookie-flag t
  "*はてなダイアリーライターのログインに、cookieを利用するかどうか
を指定するフラグ。

実行時に、-cオプションとして使われる。"
  :type 'boolean
  :group 'simple-hatena)

(defcustom simple-hatena-process-buffer-name "*SimpleHatena*"
  "*はてダラを実行するプロセスのバッファ名。"
  :type 'string
  :group 'simple-hatena)

;; キーバインド
(setq simple-hatena-mode-map (make-keymap))

(define-key simple-hatena-mode-map (kbd "C-c C-v") 'simple-hatena-version)
(define-key simple-hatena-mode-map (kbd "C-c C-p") 'simple-hatena-submit)
(define-key simple-hatena-mode-map (kbd "C-c C-c") 'simple-hatena-trivial-submit)
(define-key simple-hatena-mode-map (kbd "C-c   *") 'simple-hatena-timestamp)
(define-key simple-hatena-mode-map (kbd "C-c C-i") 'simple-hatena-change-default-id)
(define-key simple-hatena-mode-map (kbd "C-c C-g") 'simple-hatena-change-default-group)
(define-key simple-hatena-mode-map (kbd "C-c C-n") 'simple-hatena-find-diary-for)
(define-key simple-hatena-mode-map (kbd "C-c C-b") 'simple-hatena-go-back)
(define-key simple-hatena-mode-map (kbd "C-c C-f") 'simple-hatena-go-forward)
(define-key simple-hatena-mode-map (kbd "C-c C-d") 'simple-hatena-toggle-debug-mode)
(define-key simple-hatena-mode-map (kbd "C-c C-e") 'simple-hatena-exit)
(define-key simple-hatena-mode-map (kbd       "*") 'simple-hatena-electric-asterisk)

;; フック
(defcustom simple-hatena-mode-hook nil
  "simple-hatena-mode開始時のフック。"
  :type 'hook
  :group 'simple-hatena)
(defcustom simple-hatena-before-submit-hook nil
  "日記を投稿する直前のフック"
  :type 'hook
  :group 'simple-hatena)
(defcustom simple-hatena-after-submit-hook nil
  "日記を投稿した直後のフック"
  :type 'hook
  :group 'simple-hatena)

;; フォントロック

(defvar simple-hatena-font-lock-keywords nil)
(defvar simple-hatena-slag-face 'simple-hatena-slag-face)
(defvar simple-hatena-subtitle-face 'simple-hatena-subtitle-face)
(defvar simple-hatena-inline-face 'simple-hatena-inline-face)
(defvar simple-hatena-markup-face 'simple-hatena-markup-face)
(defvar simple-hatena-link-face 'simple-hatena-link-face)

(defface simple-hatena-slag-face
  '((((class color) (background light)) (:foreground "IndianRed"))
    (((class color) (background dark)) (:foreground "wheat")))
  "小見出しの*タイムスタンプorスラッグ*部分のフェイス。")

(defface simple-hatena-subtitle-face
  '((((class color) (background light)) (:foreground "DarkOliveGreen"))
    (((class color) (background dark)) (:foreground "wheat")))
  "小見出しのフェイス。")

(defface simple-hatena-inline-face
  '((((class color) (background light)) (:foreground "MediumBlue" :bold t))
    (((class color) (background dark)) (:foreground "wheat" :bold t)))
  "id記法や[keyword:Emacs]等のface")

(defface simple-hatena-markup-face
  '((((class color) (background light)) (:foreground "DarkOrange" :bold t))
    (((class color) (background dark)) (:foreground "IndianRed3" :bold t)))
  "はてなのマークアップのフェイス。")

(defface simple-hatena-link-face
  '((((class color) (background light)) (:foreground "DeepPink"))
    (((class color) (background dark)) (:foreground "wheat")))
  "リンクのフェイス。")

;;;; * 実装

(eval-when-compile
  (require 'cl)
  (require 'derived)
  (require 'font-lock)
  (require 'html-helper-mode))

(defconst simple-hatena-filename-regex
   "/\\([^/]+\\)/\\(diary\\|group\\)/\\([^/]+\\)?/?\\([0-9][0-9][0-9][0-9]\\)-\\([01][0-9]\\)-\\([0-3][0-9]\\)\.txt"
  "日記ファイルの正規表現。マッチした場合、以下のインデックスによ
りファイル情報を取得できる。

  0. マッチした全体
  1. はてなid
  2. diary/group
  3. 2がgroupの場合は、グループ名。そうでない場合はnil
  4. 年(YYYY)
  5. 月(MM)
  6. 日(DD)")

;; はてなIDの正規表現
;; > http://d.hatena.ne.jp/keyword/%A4%CF%A4%C6%A4%CAID
;; > 大文字あるいは小文字のアルファベット・0-9の数字・「-」・「_」(いずれも
;; 半角)のいずれかを3-32文字並べたもの(ただし最初の文字はアルファベットで
;; あること)から成る。
(defconst simple-hatena-id-regex
  "^[A-z][\-_A-z0-9]+[A-z0-9]$"
  "")

;; はてなグループ名の正規表現
;; > http://g.hatena.ne.jp/group?mode=append
;; > （アルファベットで始まり、アルファベットか数字で終わる3文字以上、
;; > 24文字以内の半角英数字）
;; と書かれているが「-」も使える。
(defconst simple-hatena-group-regex
  "^[A-z][\-A-z0-9]+[A-z0-9]$"
  "")

;; simple-hatena-modeを、html-helper-modeの派生モードとして定義する。
(define-derived-mode simple-hatena-mode html-helper-mode "Simple Hatena"
  "はてなダイアリーライターを、Emacsから利用するためのインタフェイ
スを提供するモード。

設定方法や使い方については、以下を参照のこと。
http://coderepos.org/share/wiki/SimpleHatenaMode"

  ;; 現在開いているバッファの情報
  (make-local-variable 'simple-hatena-local-current-buffer-info)
  (make-local-variable 'simple-hatena-local-current-buffer-id)
  (make-local-variable 'simple-hatena-local-current-buffer-type)
  (make-local-variable 'simple-hatena-local-current-buffer-group)
  (make-local-variable 'simple-hatena-local-current-buffer-year)
  (make-local-variable 'simple-hatena-local-current-buffer-month)
  (make-local-variable 'simple-hatena-local-current-buffer-day)
  (make-local-variable 'simple-hatena-local-current-buffer-basehttpurl)

  (if (string-match simple-hatena-filename-regex (buffer-file-name))
      (progn
        (setq simple-hatena-local-current-buffer-info
              (match-string 0 (buffer-file-name)))
        (setq simple-hatena-local-current-buffer-id
              (match-string 1 (buffer-file-name)))
        (setq simple-hatena-local-current-buffer-type
              (match-string 2 (buffer-file-name)))
        (setq simple-hatena-local-current-buffer-group
              (match-string 3 (buffer-file-name)))
        (setq simple-hatena-local-current-buffer-year
              (match-string 4 (buffer-file-name)))
        (setq simple-hatena-local-current-buffer-month
              (match-string 5 (buffer-file-name)))
        (setq simple-hatena-local-current-buffer-day
              (match-string 6 (buffer-file-name)))
        (simple-hatena-update-modeline))
    (error "Current buffer isn't related to Hatena::Diary Writer data file"))

  ;; フォントロック
  (font-lock-add-keywords 'simple-hatena-mode
    (list
     (list  "^\\(\\*[*a-zA-Z0-9_-]*\\)\\(.*\\)$"
            '(1 simple-hatena-slag-face t)
            '(2 simple-hatena-subtitle-face t))
     ;; 必ず[]で囲まれていなければならないもの
     (list "\\[[*a-zA-Z0-9_-]+\\(:[^\n]+\\)+\\]"
           '(0 simple-hatena-inline-face t))
     ;; 必ずしも[]で囲まれていなくてもよいもの
     (list "\\[?\\(id\\|a\\|b\\|d\\|f\\|g\\|graph\\|i\\|idea\\|map\\|question\\|r\\|isbn\\|asin\\)\\(:[a-zA-Z0-9_+:-]+\\)+\\]?"
           '(0 simple-hatena-inline-face t))
     (list  "^\\(:\\)[^:\n]+\\(:\\)"
            '(1 simple-hatena-markup-face t)
            '(2 simple-hatena-markup-face t))
     (list  "^\\([-+]+\\)"
            '(1 simple-hatena-markup-face t))
     (list  "\\(((\\).*\\())\\)"
            '(1 simple-hatena-markup-face t)
            '(2 simple-hatena-markup-face t))
     (list  "^\\(>>\\|<<\\|><!--\\|--><\\|>|?[^|]*|\\||?|<\\|=====?\\)"
            '(1 simple-hatena-markup-face t))
     (list  "\\(s?https?://\[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#\]+\\)"
            '(1 simple-hatena-link-face t))))
  (font-lock-mode 1)

  (use-local-map simple-hatena-mode-map)
  (run-hooks 'simple-hatena-mode-hook))

;; はてダラデータにsimple-hatena-modeを適用する
;;
;; - ~/.hatena/hatena-id/diary/YYYY-MM-DD.txt
;; - ~/.hatena/hatena-id/group/group-name/YYYY-MM-DD.txt
;;
;; というファイルを開いたら、simple-hatena-modeにする
(add-to-list 'auto-mode-alist
             (cons simple-hatena-filename-regex 'simple-hatena-mode))

;;;; * コマンド

(defun simple-hatena-setup ()
  (interactive)
  "ディレクトリ配置をセットアップする。"
  (and
   ;; simple-hatena-bin
   (simple-hatena-setup-check-hatena-bin-exists-p)

   ;; hatena id(s)
   (simple-hatena-setup-id)

   ;; hatena group
   (if (y-or-n-p
        "Set up about `Hatena::Group' next? ")
       (simple-hatena-group-setup)
     (message "Enjoy!"))))

(defun simple-hatena-setup-check-hatena-bin-exists-p ()
  (let
      ((simple-hatena-bin-full-path
        (locate-library simple-hatena-bin t exec-path)))
    (if (and
         simple-hatena-bin-full-path
         (file-executable-p simple-hatena-bin-full-path))
        t
      (progn
        (if (y-or-n-p
             (format
              "`Hatena Diary Writer' not found in %s. Are you sure to continue setup? "
              simple-hatena-bin))
            t
          (progn
            (when (y-or-n-p
                   "Open the documentation of simple-hatnea-mode in your browser? ")
              (browse-url "http://coderepos.org/share/wiki/SimpleHatenaMode"))
            (message "You must download and install `Hatena Diary Writer' first")
            nil))))))

(defun simple-hatena-setup-id ()
  (let
      ((ids (list)))
    (when (file-directory-p simple-hatena-root)
      (dolist (id (simple-hatena-internal-list-directories simple-hatena-root))
        (add-to-list 'ids id)))

    (when simple-hatena-default-id
      (add-to-list 'ids simple-hatena-default-id))

    (while
        (or (not ids) ;;FIXME incomprehensible.
            (y-or-n-p
             (format
              "Existing id(s): `%s'\nSet up other id? "
              (mapconcat 'identity
                         ids "', `"))))
      (add-to-list
       'ids (simple-hatena-read-string-and-match-check
             "Please input your Hatena id: "
             simple-hatena-id-regex
             "`%s' is invalid as a Hatena id.")))

    (dolist (id ids)
      (simple-hatena-setup-id-create-directory-and-file id))
    ids))

(defun simple-hatena-setup-id-create-directory-and-file (id)
  (simple-hatena-setup-create-directory-and-file
   (expand-file-name
    (format "%s/%s/diary/config.txt"
            simple-hatena-root id))))

(defun simple-hatena-group-setup ()
  (interactive)
  "ディレクトリにはてなグループを追加する。"
  ;; hatena group(s)
  (simple-hatena-setup-group))

(defun simple-hatena-setup-group ()
  (let*
      ((groups (list))
       (id (condition-case err
               simple-hatena-local-current-buffer-id
             (error (simple-hatena-internal-completing-read-id))))
       (group-dir (expand-file-name (format "%s/%s/group"
                                            simple-hatena-root id))))

    (unless (file-directory-p group-dir)
      (make-directory group-dir 'parents))

    (dolist (group (simple-hatena-internal-list-directories group-dir))
      (add-to-list 'groups group))

    (while
        (or (not groups)
            (y-or-n-p
             (format
              "Existing group(s): `%s'\nSet up other group? "
              (mapconcat 'identity
                         groups "', `"))))
      (add-to-list
       'groups (simple-hatena-read-string-and-match-check
                (format
                 "Please input a group name for id:%s: " id)
                simple-hatena-group-regex
             "`%s' is invalid as a group name.")))

    (dolist (group groups)
      (if (string-match simple-hatena-group-regex group)
          (simple-hatena-setup-group-create-directory-and-file id group)
        (message "`%s' is invalid as a group name." group)))))

(defun simple-hatena-setup-group-create-directory-and-file (id group)
  (simple-hatena-setup-create-directory-and-file
   (expand-file-name
    (format "%s/%s/group/%s/config.txt"
            simple-hatena-root id group))))

(defun simple-hatena-setup-create-directory-and-file (filename)
  "Set up a directory and file.

NOTE: Create intermediate directories as required."
  (let
      ((dirname (file-name-directory filename)))
    (unless (file-exists-p filename)
      (unless (file-directory-p dirname)
        (make-directory dirname 'parents))
      (append-to-file 1 1 filename))))

(defun simple-hatena-read-string-and-match-check (prompt regex
                                                         &optional errmsg)
  "Read a string from the minibuffer, prompting with string prompt,
and Cheking input value.

If non-nil, third args, you can set error message.

NOTE: Please refer to `format' for the format of the error
message."
  (let
      ((input nil)
       (errmsg (or errmsg
                   "Your input is invalid...")))
    (while
        (and
         (setq input (read-string prompt))
         (not (string-match regex input)))
      (message errmsg input)
      (sleep-for 1))
    input))

(defun simple-hatena (id)
  "実行日現在の日付のファイルを開く。"
  (interactive
   (list
    (or simple-hatena-default-id
        (simple-hatena-internal-completing-read-id))))
  (simple-hatena-internal-safe-find-file
   (expand-file-name
    (format "%s/%s/diary/%s" simple-hatena-root id
            (simple-hatena-internal-make-diary-file-string 0)))))

(defun simple-hatena-group (id group)
  "実行日現在の日付の、指定されたグループに投稿するためのファイル
を開く。"
  (interactive
   (if simple-hatena-default-id
       (list
        simple-hatena-default-id
        (if simple-hatena-default-group
            simple-hatena-default-group
          (simple-hatena-internal-completing-read-group simple-hatena-default-id)))
     (let ((id (simple-hatena-internal-completing-read-id)))
       (list
        id
        (if simple-hatena-default-group
            simple-hatena-default-group
          (simple-hatena-internal-completing-read-group id))))))
  (simple-hatena-internal-safe-find-file
   (expand-file-name
    (format "%s/%s/group/%s/%s" simple-hatena-root id group
            (simple-hatena-internal-make-diary-file-string 0)))))

(defun simple-hatena-change-default-id ()
  "現在のデフォルトidを変更する。"
  (interactive)
  (setq simple-hatena-default-id
        (simple-hatena-internal-completing-read-id))
  (message "Changed current default id to %s" simple-hatena-default-id))

(defun simple-hatena-change-default-group ()
  "現在のデフォルトグループを変更する。"
  (interactive)
  (if simple-hatena-default-id
      (setq simple-hatena-default-group
            (simple-hatena-internal-completing-read-group simple-hatena-default-id))
    (list (simple-hatena-change-default-id)
          (setq simple-hatena-default-group
                (simple-hatena-internal-completing-read-group simple-hatena-default-id))))
  (message "Change current default group to %s" simple-hatena-default-group))

(defun simple-hatena-submit ()
  "はてなダイアリー/グループに投稿する。"
  (interactive)
  (simple-hatena-internal-do-submit))

(defun simple-hatena-trivial-submit ()
  "はてなダイアリー/グループに「ちょっとした更新」で投稿する。"
  (interactive)
  (simple-hatena-internal-do-submit "-t"))

(defun simple-hatena-timestamp ()
  "実行位置に、「*タイムスタンプ*」を挿入する。"
  (interactive)
  (insert (format-time-string "*%s*" (current-time))))

(defun simple-hatena-find-diary-for (date)
  "指定された日付の日記バッファを表示する。"
  (interactive "sDate(YYYY-MM-DD): ")
  (if (equal major-mode 'simple-hatena-mode)
      (if (string-match "^[0-9][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]$" date)
          (simple-hatena-internal-safe-find-file
           (concat (file-name-directory (buffer-file-name))
                   (concat date ".txt")))
        (if (string-match "^$" date)
            (simple-hatena-internal-safe-find-file
             (concat (file-name-directory (buffer-file-name))
                     (simple-hatena-internal-make-diary-file-string 0)))
          (error "Invalid date")))
    (error "Current major mode isn't simple-hatena-mode")))

(defun simple-hatena-go-forward (&optional i)
  "前の日付へ移動する。前置引数が渡された場合は、その数だけ後の日付に移動する。"
  (interactive "p")
  (if (not i)
      (simple-hatena-internal-go-for 1)
    (simple-hatena-internal-go-for i)))

(defun simple-hatena-go-back (&optional i)
  "次の日付へ移動する。前置引数が渡された場合は、その数だけ前の日付に移動する。"
  (interactive "p")
  (if (not i)
      (simple-hatena-internal-go-for -1)
    (simple-hatena-internal-go-for (- i))))

(defun simple-hatena-open-current-url ()
  "今開いているバッファに対応するHTTP URLをウェブブラウザで開く。"
  (interactive)
  (simple-hatena-create-base-httpurl)
  (browse-url simple-hatena-local-current-buffer-basehttpurl))

(defun simple-hatena-toggle-debug-mode ()
  "デバッグモードをオン/オフする。"
  (interactive)
  (setq simple-hatena-option-debug-flag
        (not simple-hatena-option-debug-flag))
  (message "%s %s"
           "Debug mode"
           (if simple-hatena-option-debug-flag
               "on" "off")))

(defun simple-hatena-exit ()
  "simple-hatena-modeの適用されているバッファを全て削除する。"
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and
           (buffer-file-name buffer)
           (string-match simple-hatena-filename-regex (buffer-file-name buffer)))
      (when (buffer-modified-p buffer)
        (progn
          (save-current-buffer
            (set-buffer buffer)
            (save-buffer))))
      (kill-buffer buffer)))
  (message "simple-hatena-mode has been exited"))

(defun simple-hatena-electric-asterisk (arg)
  "＊(アスタリスク)押下により、タイムスタンプ付き小見出しを挿入する。

ポイントが行頭にある場合のみ、タイムスタンプを挿入し、その他の場合
は、通常通りアスタリスクを挿入する。"
  (interactive "*p")
  (if (and simple-hatena-use-timestamp-permalink-flag
           (zerop (current-column)))
      (simple-hatena-timestamp)
    (self-insert-command arg)))

;;;; * 内部関数

(defun simple-hatena-internal-safe-find-file (filename)
  "新しいヴァージョンのhtml-helper-modeは、デフォルトでスケルトン
を作ってウザいので、阻止する。"
  (let ((html-helper-build-new-buffer nil))
    (find-file filename)))

(defun simple-hatena-internal-make-diary-file-string (i &optional date)
  "dateが指定されていない場合は、実行日現在の日付を起点にした日記ファイル名を生成する。

   0: 今日
   1: 明日
  -1: 昨日

指定されている場合は、その日付を起点にした日記ファイル名を生成する。"
  (apply (lambda (s min h d mon y &rest rest)
           (format-time-string "%Y-%m-%d.txt"
                               (encode-time s min h (+ d i) mon y)))
         (if date
             (append '(0 0 0) date)
           (apply (lambda (s min h d mon y &rest rest)
                    (list s min (- h (or simple-hatena-time-offset 0)) d mon y))
                  (decode-time)))))

(defun simple-hatena-internal-go-for (i)
  "引数の数だけ前後の日付のファイ名バッファへ移動する。"
  (simple-hatena-internal-safe-find-file
   (concat
    (file-name-directory (buffer-file-name))
    (simple-hatena-internal-make-diary-file-string
     i
       (list (string-to-number simple-hatena-local-current-buffer-day)
             (string-to-number simple-hatena-local-current-buffer-month)
             (string-to-number simple-hatena-local-current-buffer-year))))))

(defun simple-hatena-internal-list-directories (dir)
  "dir下にあるディレクトリをリストにして返す。"
  (let ((dir-list nil))
    (dolist (file (directory-files dir t "^[^\.]") dir-list)
      (if (file-directory-p file)
          (progn
            (string-match "\\([^/]+\\)/?$" file)
            (setq dir-list (cons (match-string 1 file) dir-list)))))))

(defun simple-hatena-internal-completing-read-id (&optional dir)
  "dir以下からはてなidを抽出し、id定義が複数の場合は入力補完プロンプトを
表示する。一つだけの場合は確定する。
dirが存在しない場合は`simple-hatena-setup'を呼ぶ。"
  (let*
      ((dir (or dir simple-hatena-root))
       (numbar-of-ids
        (and (file-directory-p dir)
             (simple-hatena-internal-list-directories dir))))
    (if numbar-of-ids
        (if (eq 1 (length numbar-of-ids))
            (car numbar-of-ids)
          (completing-read
           "Hatena id: "
           (simple-hatena-internal-list-directories dir) nil t))
      (and
       (simple-hatena-setup)
       (simple-hatena-internal-completing-read-id dir)))))

(defun simple-hatena-internal-completing-read-group (id)
  "dir以下からグループ名を抽出し、補完入力させる。"
  (completing-read
   "Group: " (simple-hatena-internal-list-directories
              (concat simple-hatena-root "/" id "/group")) nil t))

(defun simple-hatena-internal-do-submit (&optional flag)
  "はてなダイアリ/グループへ日記を投稿する。"
  (let ((max-mini-window-height 10)  ; hw.plが表示するメッセージを、
                                     ; echoエリアに表示させるため。
        (thisdir (file-name-directory (buffer-file-name))))
    (run-hooks 'simple-hatena-before-submit-hook)
    (when (buffer-modified-p)
      (save-buffer))
    (message "%s" "Now posting...")
    (let* ((buffer (get-buffer-create simple-hatena-process-buffer-name))
           (proc (get-buffer-process buffer)))
      (if (and
           proc
           (eq (process-status proc) 'run))
          (if (yes-or-no-p (format "A %s process is running; kill it?"
                                   (process-name proc)))
              (progn
                (interrupt-process proc)
                (sit-for 1)
                (delete-process proc))
            (error nil)))
      (with-current-buffer buffer
        (progn
          (erase-buffer)
          (buffer-disable-undo (current-buffer))
          (setq default-directory thisdir)))
      (make-comint-in-buffer
       "simple-hatena-submit" buffer shell-file-name nil
       shell-command-switch (simple-hatena-internal-build-command flag))
      (set-process-sentinel
       (get-buffer-process buffer)
       '(lambda (process signal)
          (if (string= signal "finished\n")
              (let ((max-mini-window-height 10))
                (display-message-or-buffer (process-buffer process))
                (run-hooks 'simple-hatena-after-submit-hook))))))))

(defun simple-hatena-internal-build-command (flag)
  "実行可能なコマンド文字列を作成する。"
  (let ((flag-list (list flag)))
    (if simple-hatena-option-debug-flag  (setq flag-list (cons "-d" flag-list)))
    (if simple-hatena-option-cookie-flag (setq flag-list (cons "-c" flag-list)))
    (simple-hatena-internal-join
     " "
     (cons simple-hatena-bin
           (append (simple-hatena-internal-build-option-list-from-alist) flag-list)))))

(defun simple-hatena-internal-build-option-list-from-alist ()
  "引数を取るオプションのリストを作成する。"
  (let ((opts nil))
    (dolist (pair
             `(("-u" . ,simple-hatena-local-current-buffer-id)
               ("-g" . ,simple-hatena-local-current-buffer-group)
               ("-a" . ,simple-hatena-option-useragent)
               ("-T" . ,(format "%s" simple-hatena-option-timeout)))
             opts)
      (if (cdr pair)
           (setq opts (append opts (list (car pair) (cdr pair))))))))

(defun simple-hatena-internal-join (sep list)
  "車輪の再発明なんだろうけど、見つからなかったのでjoin実装"
  (if (<= (length list) 1)
      (car list)
    (concat (car list) sep (simple-hatena-internal-join sep (cdr list)))))

(defun simple-hatena-update-modeline ()
  "モードラインの表示を更新する"
  (let ((id
         (concat
          (if simple-hatena-local-current-buffer-group
              (format "g:%s:" simple-hatena-local-current-buffer-group)
            "")
          (format "id:%s" simple-hatena-local-current-buffer-id))))
    (setq mode-name (format "Simple Hatena [%s]" id))
    (force-mode-line-update)))

(defun simple-hatena-create-base-httpurl ()
  (setq simple-hatena-local-current-buffer-basehttpurl
        (concat
         (if simple-hatena-local-current-buffer-group
             (format "http://%s.g.hatena.ne.jp/%s/"
                     simple-hatena-local-current-buffer-group
                     simple-hatena-local-current-buffer-id)
         (format "http://d.hatena.ne.jp/%s/"
                 simple-hatena-local-current-buffer-id))
         (format "%s%s%s"
                 simple-hatena-local-current-buffer-year
                 simple-hatena-local-current-buffer-month
                 simple-hatena-local-current-buffer-day))))

(provide 'simple-hatena-mode)

;;; simple-hatena-mode.el ends here
