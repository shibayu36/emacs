;;; carbon-font.el -- fontsets for Carbon Emacs -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2004-2008 by T. Hiromatsu <matsuan@users.sourceforge.jp>
;; Version 1_5_8
;; 2008-02-24

;;; Commentary:

;; This package defines fixed-width multilingual fontsets for Carbon Emacs
;; on Mac OS X. Comments, questions and feedback will be sent to an english
;; list <http://lists.sourceforge.jp/mailman/listinfo/macemacsjp-english>
;; of MacEmacs JP project <http://macemacsjp.sourceforge.jp/en/>.
;;----------------------------------------------------------------------
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; The GNU General Public License can be gotten from
;; the Free Software Foundation, Inc.,
;;     59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;     http://www.gnu.org/licenses/gpl.html
;;
;;----------------------------------------------------------------------
;;      本プログラムはフリー・ソフトウェアです。
;;      あなたは、Free Software Foundationが公表したGNU 一般公有使用許諾の
;;      「バージョン２」或いはそれ以降の各バージョンの中からいずれかを選択し、
;;      そのバージョンが定める条項に従って本プログラムを
;;      再頒布または変更することができます。
;;
;;      本プログラムは有用とは思いますが、頒布にあたっては、
;;      市場性及び特定目的適合性についての暗黙の保証を含めて、
;;      いかなる保証も行ないません。
;;      詳細についてはGNU 一般公有使用許諾書をお読みください。
;;
;;      GNU一般公有使用許諾は、　
;;      Free Software Foundation,
;;         59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
;;         http://www.gnu.org/licenses/gpl.html
;;      から入手可能です。
;;
;;----------------------------------------------------------------------
;; carbon-font.el 2007-07-22版;;
;;
;;  1. Introduction
;;  1.1. idea
;;      carbon-font provides font-width-compensation for fixed-width
;;      fontset for Emacs on Mac OSX. The reasons are:
;;          Monaco bold has different width from normal font.
;;          CJK font has different width from ascii font. (We want to use
;;          2 times width for CJK).
;;
;;      Defined fontset names are
;;          osaka = osaka + monaco
;;          hiramaru = ヒラギノ丸ゴ + monaco
;;          hirakaku_w3 = ヒラギノ角ゴ w3 + monaco
;;          hirakaku_w6 = ヒラギノ角ゴ w6 + monaco
;;          hirakaku_w8 = ヒラギノ角ゴ w8 + monaco
;;          hiramin_w3 = ヒラギノ明朝 w3 + courier 
;;          hiramin_w6 = ヒラギノ明朝 w6 + courier 
;;
;;      Defined sizes are
;;          point 7,8,9,10,12,14,16,18,20,24
;;
;;      then totally 70 fontsets were defined.
;;
;;  1.2. Emacs version
;;      carbon-font supports only CVS version of Emacs after June 1st, 2005.
;;
;;  2. Usage
;;  2.1. Installation
;;      Please put two files in the folder on load-path.
;;          carbon-font.el (this file)
;;          fixed-width-fontset.el
;;
;;  2.2. load package
;;      (if (eq window-system 'mac) (require 'carbon-font))
;;
;;  2.3. set fontset
;;      Use `fixed-width-set-fontset'.
;;          Set fontset and size to `default-frame-alist' and `frame-parameter' of
;;          current frame as `font'. if size is nil, default size of fontset will be used.
;;          To get available fontset, use `fontset-list'.
;;
;;      example:
;;          (fixed-width-set-fontset "hiramaru" 14)
;;
;;  2.4. disable compensation of font width
;;          (setq fixed-width-rescale nil)
;;
;;  3. create your own fontset
;;      If you want to create another fontset, please use  new function
;;          (carbon-font-create-fontset fontset size list)
;;              fontset : fontset name(striings)
;;              size : size or list of size that you want to create
;;              list : alist of encodings and font family name
;;    
;;      example : courier and ヒラギノ丸ゴシック(hiragino maru gothic)
;;
;;      (setq carbon-font-encode-family-list-courier
;;        '((ascii . "courier")
;;          (japanese-jisx0208 . "hiragino maru gothic pro")
;;          (katakana-jisx0201 . "hiragino maru gothic pro")
;;          (thai-tis620 . "ayuthaya")
;;          (chinese-gb2312 . "stheiti*")
;;          (chinese-big5-1 . "lihei pro*")
;;          (korean-ksc5601 . "applegothic*")))
;;
;;          (carbon-font-create-fontset "courier"
;;                                      carbon-font-defined-sizes
;;                                      carbon-font-encode-family-list-courier)
;;
;;      Then, you can get new fontsets "fontset-courier", that have sizes
;;      from 7 to 24 point.
;;
;;  4. Supported encodings on Carbon Emacs
;;      `mac-charset-info-alist shows
;;      (("mac-dingbats" 34 nil)
;;       ("adobe-fontspecific" 33 nil)
;;       ("mac-symbol" 33 nil)
;;       ("mac-centraleurroman" 29 mac-centraleurroman)
;;       ("gb2312.1980-0" 25 chinese-iso-8bit)
;;       ("mac-cyrillic" 7 mac-cyrillic)
;;       ("ksc5601.1989-0" 3 korean-iso-8bit)
;;       ("big5-0" 2 chinese-big5)
;;       ("jisx0201.1976-0" 1 japanese-shift-jis)
;;       ("jisx0208.1983-sjis" 1 japanese-shift-jis)
;;       ("mac-roman" 0 mac-roman))
;;
;;       And also "mac-roman" is described 
;;      ;; Create a fontset that uses mac-roman font.  With this fontset,
;;      ;; characters decoded from mac-roman encoding (ascii, latin-iso8859-1,
;;      ;; and mule-unicode-xxxx-yyyy) are displayed by a mac-roman font.
;;
;;----------------------------------------------------------------------
;;
;; 1. Introduction
;; 1.1. このファイルの中身
;;	このファイルは、carbon emacs on Mac OSX で、2バイト文字と、asciiを
;;	1:2の幅で(所謂等幅)で、表示するためのfontset定義の例を示しています。
;;
;;	定義しているのは、下記の7種の文字セットです。
;;          osaka = osaka + monaco
;;          hiramaru = ヒラギノ丸ゴ + monaco
;;          hirakaku_w3 = ヒラギノ角ゴ w3 + monaco
;;          hirakaku_w6 = ヒラギノ角ゴ w6 + monaco
;;          hirakaku_w8 = ヒラギノ角ゴ w8 + monaco
;;          hiramin_w3 = ヒラギノ明朝 w3 + courier 
;;          hiramin_w6 = ヒラギノ明朝 w6 + courier 
;;
;;	    point 7,8,9,10,12,14,16,18,20,24 のサイズ
;;
;;	を定義しています。つまり、このファイルでは、70種の、fontset を定義
;;	していることになります。
;;      defaultのサイズは、12です。
;; 
;; 1.2. 動作環境
;;	carbon emacs は、2005-06-01 以降のCVSから入手した物を御使いください。
;;	それ以前の物は、.emacs読み込みの時にエラーになる可能性があります。
;;
;;	種々な知恵を授けてくださった、mac-emacsen ML や 2ch mac de emacs会議
;;	室の方々に感謝します。
;;
;; 2. Usage(使い方)
;;  2.1. Install
;;      下記2つのファイルをロードパスの通ったところに置いて下さい。
;;          carbon-font.el (this file)
;;          fixed-width-fontset.el
;;
;;  2.2. 読み込み
;;      (if (eq window-system 'mac) (require 'carbon-font))
;;      としてください。
;;
;;  2.3. フォントセットを設定
;;      `fixed-width-set-fontset' を使います。
;;          Set fontset and size to `default-frame-alist' and `frame-parameter' of
;;          current frame as `font'. if size is nil, default size of fontset will be used.
;;          To get available fontset, use `fontset-list'.
;;
;;      例:
;;          (fixed-width-set-fontset "hiramaru" 14)
;;
;;  2.4. 等幅補正を切りたい場合
;;          (setq fixed-width-rescale nil)
;;      としてください。
;;
;;  2.5. fixed-width-fontset.0.9.0 との表示互換性
;;      (require 'carbon-font) の前に、
;;          (setq fixed-width-use-QuickDraw-for-ascii t)
;;      を設定して下さい。
;;
;;      これをやると、monaco も、QuickDraw を使うようになります。その代わり、
;;      bold が少し潰れてしまいます。行間が詰まり 0.9.0 と同様の表示になり
;;      ます。
;;      default は (setq fixed-width-use-QuickDraw-for-ascii nil) で、
;;      monaco はATSUI を使います。bold は等幅になって、すっきり見えます。
;;      又、行間が少し開きます。
;;
;;  3. 別のフォントセットを作りたい場合別の組合せの fontset を設定したい場
;;      合。(新機能)
;;
;;      (carbon-font-create-fontset fontset size list) を使ってください。
;;          fontset : fontset の名前(striings)
;;          size : 設定したいサイズ、又はサイズのリスト
;;          list : エンコーディングとフォントのファミリーネームの連想リスト
;;         
;;      例えば、courier に ヒラギノ丸ゴシックを組み合わせたい場合
;;
;;      (setq carbon-font-encode-family-list-courier
;;        '((ascii . "courier")
;;          (japanese-jisx0208 . "hiragino maru gothic pro")
;;          (katakana-jisx0201 . "hiragino maru gothic pro")
;;          (thai-tis620 . "ayuthaya")
;;          (chinese-gb2312 . "stheiti*")
;;          (chinese-big5-1 . "lihei pro*")
;;          (korean-ksc5601 . "applegothic*")))
;;
;;      等と定義しておいて、    
;;         
;;      (carbon-font-create-fontset "courier"
;;                                  carbon-font-defined-sizes
;;                                  carbon-font-encode-family-list-courier)
;;
;;      を評価すれば、7〜24 までのサイズの fontset が、fontset-courier という名前で
;;      定義されます。
;;
;;  4. 現在、carbon emacs が、サポートしているエンコーディング
;;      `mac-charset-info-alist shows
;;      (("mac-dingbats" 34 nil)
;;       ("adobe-fontspecific" 33 nil)
;;       ("mac-symbol" 33 nil)
;;       ("mac-centraleurroman" 29 mac-centraleurroman)
;;       ("gb2312.1980-0" 25 chinese-iso-8bit)
;;       ("mac-cyrillic" 7 mac-cyrillic)
;;       ("ksc5601.1989-0" 3 korean-iso-8bit)
;;       ("big5-0" 2 chinese-big5)
;;       ("jisx0201.1976-0" 1 japanese-shift-jis)
;;       ("jisx0208.1983-sjis" 1 japanese-shift-jis)
;;       ("mac-roman" 0 mac-roman))
;;
;;      "mac-roman" は、下記のように3つのエンコーディングを含んでいます。
;;      ;; Create a fontset that uses mac-roman font.  With this fontset,
;;      ;; characters decoded from mac-roman encoding (ascii, latin-iso8859-1,
;;      ;; and mule-unicode-xxxx-yyyy) are displayed by a mac-roman font.
;;
;;                                                  T.Hiromatsu
;;                                                  matsuan@users.sourceforge.jp

;;
;; fontset section
;;

(require 'fixed-width-fontset)

(defvar fixed-width-encode-reg-alist
  '((japanese-jisx0208 . "iso10646-*")
    (katakana-jisx0201 . "iso10646-*")
    (japanese-jisx0212 . "iso10646-*")
    (thai-tis620 . "iso10646-*")
    (chinese-gb2312 . "iso10646-*")
    (chinese-big5-1 . "iso10646-*")
    (korean-ksc5601 . "iso10646-*")
    (latin-iso8859-1 . "mac-roman")
    (latin-iso8859-2 . "mac-centraleurroman")
    (cyrillic-iso8859-5 . "mac-cyrillic")))

(defvar fixed-width-use-QuickDraw-for-ascii nil)

(defvar fixed-width-xlfd-template
  (if fixed-width-use-QuickDraw-for-ascii
      "-apple-%s-medium-r-normal--%d-*-*-*-*-*-mac-roman"
    "-apple-%s-medium-r-normal--%d-*-*-*-*-*-iso10646-1"))

(defvar fixed-width-fontset-template "-*-*-medium-r-normal--%d-*-*-*-*-*-fontset-%s")

(defalias 'fixed-width-create-fontset-func 'create-fontset-from-mac-roman-font)

(defalias 'carbon-font-create-fontset 'fixed-width-create-fontset)

;;
;; fontset definition section
;;

(defvar carbon-font-defined-sizes '(12 7 8 9 10 14 16 18 20 24))

;;
;; osaka = osaka + monaco
;;

(defvar carbon-font-encode-family-list-osaka
  '((ascii . "monaco")
    (japanese-jisx0208 . "osaka")
    (katakana-jisx0201 . "osaka")
    (japanese-jisx0212 . "osaka")
    (chinese-gb2312 . "stheiti*")
    (chinese-big5-1 . "lihei pro*")
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "osaka"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-osaka)

;;
;; use Quick Draw
;;

(setcdr (assoc 'japanese-jisx0208 fixed-width-encode-reg-alist) "jisx0208.*")
(setcdr (assoc 'katakana-jisx0201 fixed-width-encode-reg-alist) "jisx0201.*")

;;
;; hiramaru = ヒラギノ丸ゴ + monaco
;;

(defvar carbon-font-encode-family-list-hiramaru
  `((ascii . "monaco")
    (japanese-jisx0208 . "ヒラギノ丸ゴ pro w4")
    (katakana-jisx0201 . "ヒラギノ丸ゴ pro w4")
    (japanese-jisx0212 . "hiragino maru gothic pro")
    (thai-tis620 . "ayuthaya")
    (chinese-gb2312 . "stheiti*")
    (chinese-big5-1 . ,(if (x-list-fonts "*apple ligothic*")
                           "apple ligothic*" "lihei pro*"))
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "hiramaru"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hiramaru)

;;
;; hirakaku_w3 = ヒラギノ角ゴ w3 + monaco
;;

(defvar carbon-font-encode-family-list-hirakaku_w3
  `((ascii . "monaco")
    (japanese-jisx0208 . "ヒラギノ角ゴ pro w3")
    (katakana-jisx0201 . "ヒラギノ角ゴ pro w3")
    (japanese-jisx0212 . "hiragino kaku gothic pro")
    (thai-tis620 . "ayuthaya")
    (chinese-gb2312 . ,(if (x-list-fonts "*-hei-*") "hei*" "stheiti*"))
    (chinese-big5-1 . "lihei pro*")
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "hirakaku_w3"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hirakaku_w3)

;;
;; hirakaku_w6 = ヒラギノ角ゴ w6 + monaco
;;

(defvar carbon-font-encode-family-list-hirakaku_w6
  `((ascii . "monaco")
    (japanese-jisx0208 . "ヒラギノ角ゴ pro w6")
    (katakana-jisx0201 . "ヒラギノ角ゴ pro w6")
    (japanese-jisx0212 . "hiragino kaku gothic pro")
    (thai-tis620 . "ayuthaya")
    (chinese-gb2312 . ,(if (x-list-fonts "*-hei-*") "hei*" "stheiti*"))
    (chinese-big5-1 . "lihei pro*")
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "hirakaku_w6"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hirakaku_w6)

;;
;; hirakaku_w8 = ヒラギノ角ゴ w8 + monaco
;;

(defvar carbon-font-encode-family-list-hirakaku_w8
  `((ascii . "monaco")
    (japanese-jisx0208 . "ヒラギノ角ゴ std w8")
    (katakana-jisx0201 . "ヒラギノ角ゴ std w8")
    (japanese-jisx0212 . "hiragino kaku gothic pro")
    (thai-tis620 . "ayuthaya")
    (chinese-gb2312 . ,(if (x-list-fonts "*-hei-*") "hei*" "stheiti*"))
    (chinese-big5-1 . "lihei pro*")
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "hirakaku_w8"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hirakaku_w8)

;;
;; hiramin_w3 = ヒラギノ明朝 w3 + courier 
;;

(defvar carbon-font-encode-family-list-hiramin_w3
  `((ascii . "courier")
    (japanese-jisx0208 . "ヒラギノ明朝 pro w3")
    (katakana-jisx0201 . "ヒラギノ明朝 pro w3")
    (japanese-jisx0212 . "hiragino mincho pro")
    (chinese-gb2312 . ,(if (x-list-fonts "*stkaiti*") "stkaiti*" "stheiti*"))
    (chinese-big5-1 . ,(if (x-list-fonts "*lisong pro*") "lisong pro*" "lihei pro*"))
    (korean-ksc5601 . ,(if (x-list-fonts "*applemyungjo*")
                           "applemyungjo*" "applegothic*"))))

(carbon-font-create-fontset "hiramin_w3"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hiramin_w3)

;;
;; hiramin_w6 = ヒラギノ明朝 w6 + courier 
;;

(defvar carbon-font-encode-family-list-hiramin_w6
  `((ascii . "courier")
    (japanese-jisx0208 . "ヒラギノ明朝 pro w6")
    (katakana-jisx0201 . "ヒラギノ明朝 pro w6")
    (japanese-jisx0212 . "hiragino mincho pro")
    (chinese-gb2312 . ,(if (x-list-fonts "*stkaiti*") "stkaiti*" "stheiti*"))
    (chinese-big5-1 . ,(if (x-list-fonts "*lisong pro*") "lisong pro*" "lihei pro*"))
    (korean-ksc5601 . ,(if (x-list-fonts "*applemyungjo*")
                           "applemyungjo*" "applegothic*"))))

(carbon-font-create-fontset "hiramin_w6"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hiramin_w6)

;;
;; 変数 section
;;

;;  使用するフォントセットを変えた後、自動で、ボールドをリスケールさせる。
;;  ボールドをリスケールする為のファクターの定義

(defvar fixed-width-scale-alist-hiragino
  '(("7" . 1.15) ("8" . 1.35) ("9" . 1.35) ("10" . 1.2) ("12" . 1.2)
    ("14" . 1.2) ("16" . 1.25) ("18" . 1.25) ("20" . 1.2) ("24" . 1.2)))

(defvar fixed-width-scale-alist-two-byte-bold
  '(("8" . 1.2) ("9" . 1.25) ("10" . 1.1) ("12" . 1.15)
    ("14" . 1.1) ("16" . 1.2) ("18" . 1.2) ("20" . 1.15) ("24" . 1.15)))

(defvar fixed-width-scale-alist-osaka-normal
  '(("7" . 1.15) ("8" . 1.25) ("9" . 1.35) ("10" . 1.2) ("12" . 1.2)
    ("14" . 1.2) ("16" . 1.25) ("18" . 1.25) ("20" . 1.2) ("24" . 1.2)))

(defvar fixed-width-scale-alist-osaka-bold
  '(("7" . 1.15) ("8" . 1.25) ("9" . 1.35) ("10" . 1.25) ("12" . 1.2)
    ("14" . 1.2) ("16" . 1.25) ("18" . 1.25) ("20" . 1.2) ("24" . 1.2)))

(defvar fixed-width-scale-alist-hirakaku-bold
  '(("7" . 1.1) ("8" . 1.2) ("9" . 1.3) ("10" . 1.1) ("12" . 1.1)
    ("14" . 1.1) ("16" . 1.2) ("18" . 1.2) ("20" . 1.15) ("24" . 1.15)))

(defvar fixed-width-scale-alist-hirahan-bold
  '(("7" . 0.8) ("8" . 1.1) ("9" . 1.2) ("10" . 1.0) ("12" . 1.0)
    ("14" . 1.0) ("16" . 1.1) ("18" . 1.1) ("20" . 1.1) ("24" . 1.1)))

(defvar fixed-width-scale-alist-hiramin
  '(("7" . 1.15) ("8" . 1.35) ("9" . 1.2) ("10" . 1.2) ("12" . 1.2)
    ("14" . 1.2) ("16" . 1.25) ("18" . 1.25) ("20" . 1.2) ("24" . 1.2)))

(defvar fixed-width-scale-alist-monaco-bold
  '(("7" . 0.8) ("8" . 0.95) ("9" . 0.9) ("10" . 0.8) ("12" . 0.9)
    ("14" . 0.9) ("16" . 0.95) ("18" . 0.9) ("20" . 0.95) ("24" . 0.92)))

(defvar fixed-width-get-scale-alist
  `((".*monaco-bold-.*-mac-roman" . ,fixed-width-scale-alist-monaco-bold)
    (".*monaco cy-bold-.*-mac-cyrillic" . ,fixed-width-scale-alist-monaco-bold)
    (".*courier-bold-.*-mac-roman" . (( "9" . 0.9) ("10" . 0.9)))
    (".*osaka-medium.*" . ,fixed-width-scale-alist-osaka-normal)
    (".*osaka-bold.*" . ,fixed-width-scale-alist-osaka-bold)
    ("^-apple-hiragino.*" . ,fixed-width-scale-alist-hiragino)
    (,(encode-coding-string ".*ヒラギノ丸ゴ pro w4.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hiragino)
    (,(encode-coding-string ".*ヒラギノ角ゴ pro w3-medium.*" 'emacs-mule) .
     ,fixed-width-scale-alist-osaka-normal)
    (,(encode-coding-string ".*ヒラギノ角ゴ pro w3-bold.*jisx0208.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hirakaku-bold)
    (,(encode-coding-string ".*ヒラギノ角ゴ pro w3-bold.*jisx0201.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hirahan-bold)
    (,(encode-coding-string ".*ヒラギノ角ゴ pro w6.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hiragino)
    (,(encode-coding-string ".*ヒラギノ角ゴ std w8.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hiragino)
    (,(encode-coding-string ".*ヒラギノ明朝 pro w3.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hiramin)
    (,(encode-coding-string ".*ヒラギノ明朝 pro w6.*" 'emacs-mule) .
     ,fixed-width-scale-alist-hiramin)
    ("^-apple-stheiti-.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-lihei pro-.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-applegothic-.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-applemyungjo-.*" . ,fixed-width-scale-alist-hiramin)
    ("^-apple-lisong pro-.*" . ,fixed-width-scale-alist-hiramin)
    ("^-apple-stkaiti-.*" . ,fixed-width-scale-alist-hiramin)
    ("^-apple-hei-.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-apple ligothic-.*" . ,fixed-width-scale-alist-hiragino))
  "ReScale factor alist for each fonts and size.")

(provide 'carbon-font)

;;; carbon-font.el ends here
