<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: rfringe.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=rfringe.el" /><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: rfringe.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=rfringe.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for rfringe.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=rfringe.el" />
<script type="text/javascript">
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-2101513-1']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
</script>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/></head><body class="http://www.emacswiki.org/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/emacs/%e3%82%b5%e3%82%a4%e3%83%88%e3%83%9e%e3%83%83%e3%83%97"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/%e3%82%b5%e3%82%a4%e3%83%88%e3%83%9e%e3%83%83%e3%83%97">サイトマップ</a> <a class="local" href="http://www.emacswiki.org/emacs/%e6%9b%b4%e6%96%b0%e5%b1%a5%e6%ad%b4">更新履歴</a> <a class="local" href="http://www.emacswiki.org/emacs/%e3%83%8b%e3%83%a5%e3%83%bc%e3%82%b9">ニュース</a> <a class="local" href="http://www.emacswiki.org/emacs/%ef%bc%a5%ef%bd%8c%ef%bd%89%ef%bd%93%ef%bd%90%e3%82%bb%e3%82%af%e3%82%b7%e3%83%a7%e3%83%b3">Ｅｌｉｓｐセクション</a> <a class="local" href="http://www.emacswiki.org/emacs/%e5%88%a9%e7%94%a8%e6%89%8b%e5%bc%95">利用手引</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&amp;q=%22rfringe.el%22">rfringe.el</a></h1></div><div class="wrapper"><div class="content browse"><p class="download"><a href="http://www.emacswiki.org/emacs/download/rfringe.el">Download</a></p><pre class="code"><span class="linecomment">;;; rfringe.el --- display the relative location of the region, in the fringe.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Author     : Dino Chiesa &lt;dpchiesa@hotmail.com&gt;</span>
<span class="linecomment">;; Created    : April 2011</span>
<span class="linecomment">;; Version    : 1.0</span>
<span class="linecomment">;; Keywords   : fringe, bitmap</span>
<span class="linecomment">;; X-URL      : http://www.emacswiki.org/emacs/rfringe.el</span>
<span class="linecomment">;; Last-saved : &lt;2011-April-05 11:20:26&gt;</span>

<span class="linecomment">;; Copyright (C) 2011 Dino Chiesa</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This file is NOT part of GNU Emacs and is licensed differently.</span>
<span class="linecomment">;; rfringe.el is licensed under the Ms-PL.  See the full copy of that</span>
<span class="linecomment">;; license for more details. http://www.opensource.org/licenses/ms-pl</span>
<span class="linecomment">;;</span>

<span class="linecomment">;;; Commentary:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This is a module to allow the use of the fringe to indicate locations</span>
<span class="linecomment">;; relative to the bounds of the buffer.  rfringe = "relative fringe".</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; In emacs, displaying fringe indicators is done via text overlays. In</span>
<span class="linecomment">;; that way, bitmaps in the fringe are attached to the lines of text</span>
<span class="linecomment">;; shown in the buffer window.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This works nicely when the fringe is used to indicate information</span>
<span class="linecomment">;; that is relevant to the adjacent line; for example, text overflow, or</span>
<span class="linecomment">;; something similar. But, there isn't a simple way for an application</span>
<span class="linecomment">;; or module to use the fringe to display buffer-relative information -</span>
<span class="linecomment">;; for example, the location of compiler error messages.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; In fact, using the fringe to communicate this kind of information -</span>
<span class="linecomment">;; buffer-relative positions - is probably more intuitive and is</span>
<span class="linecomment">;; certainly more useful for the user. For example, consider the</span>
<span class="linecomment">;; scrollbar. The position and size of the scrollbar indicates to the</span>
<span class="linecomment">;; user the position and size of the current window relative to the</span>
<span class="linecomment">;; entire buffer. This is information that cound not be easily or</span>
<span class="linecomment">;; appropriately conveyed within the visual text. The fringe is</span>
<span class="linecomment">;; perfectly suited for this purpose.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Along those lines, a useful integration of fringe with flymake would</span>
<span class="linecomment">;; be to use fringe bitmaps visually indicate the position of all</span>
<span class="linecomment">;; flymake errors and warnings in the buffer, relative to the beginning</span>
<span class="linecomment">;; and end of the buffer. A quick glance at the fringe would give a</span>
<span class="linecomment">;; visual indication of the number of errors or warnings and their</span>
<span class="linecomment">;; approximate positions.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Likewise, a diff mode might want to display fringe indicators for the</span>
<span class="linecomment">;; number and approximate relative position of differences.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Doing this is not simple, because of the dependency of fringe bitmaps</span>
<span class="linecomment">;; on text overlays that I described above.  To use the fringe to</span>
<span class="linecomment">;; communicate information regarding buffer-relative positions requires</span>
<span class="linecomment">;; a transformation from "buffer position" to "window position".  And</span>
<span class="linecomment">;; this transformation must be re-computed each time a window scrolls or</span>
<span class="linecomment">;; changes size.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; This module addresses that need, and provides that transformation. It</span>
<span class="linecomment">;; allows you to set an indicator that is buffer-relative in the fringe;</span>
<span class="linecomment">;; the indicator automatically redisplays if the window changes size, or</span>
<span class="linecomment">;; scrolls.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Examples:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 1. In the simplest case, you can use rfringe to provide a visual</span>
<span class="linecomment">;;    indicator of the top of the region in the buffer, like so:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;       (rfringe-show-region)</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;    To turn off the indicator, do this:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;       (rfringe-hide-region)</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; 2. You can also use rfringe to display a set of indicators,</span>
<span class="linecomment">;;    corresponding to a set of locations in the buffer. These might be</span>
<span class="linecomment">;;    the locations of compiler errors, or section beginnings, or</span>
<span class="linecomment">;;    whatever you like.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;       (setq posns '(79 1000 2000 3000 4000 5000 6000 9000 10000))</span>
<span class="linecomment">;;       (mapc 'rfringe-create-relative-indicator posns)</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;    As you scroll through the buffer, the indicators in the fringe remain fixed.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;    To remove the indicators, do this:</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;       (rfringe-remove-managed-indicators)</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;    By default, rfringe defines advice to extend flymake to display</span>
<span class="linecomment">;;    indicators this way.  This is not the only intended use of rfringe.el,</span>
<span class="linecomment">;;    but it is a good example.</span>
<span class="linecomment">;;</span>


(require 'fringe)

(defvar rfringe-region-indicator-ovly nil
  "<span class="quote">the overlay used internally for the region; see `rfringe-show-region-indicator'.

Applications should not set this value directly. It is intended
for use internally by rfringe.el .
</span>" )

(defvar rfringe-managed-indicators nil
  "<span class="quote">a list holding the position and actual overlay for all
\"managed\" indicators. They are managed in the sense that they
automatically update their positions when the window changes
configuration or scrolls, and they can be deleted as a set via
`rfringe-remove-managed-indicators'.

Each element in this list is a cons cell, (POS . OVLY) where POS
is the character position and OVLY is the actual overlay.

Applications should not set this value directly. It is intended
for use internally by rfringe.el .
</span>")


(make-variable-buffer-local 'rfringe-managed-indicators)
(make-variable-buffer-local 'rfringe-region-indicator-ovly)

<span class="linecomment">;; rfringe displays only one kind of bitmap - a thin dash. Create it here.</span>
(define-fringe-bitmap 'rfringe-thin-dash [255 0])

(defun rfringe--compute-position (lines start-pos)
  "<span class="quote">computes a position that is LINES ahead of START-POS</span>"
  (save-excursion
    (goto-char start-pos)
    (while (&gt; lines 0)
      (forward-line 1)
      (decf lines))
    <span class="linecomment">;;(message "l(%d) sp(%d) =x(%d)" lines start-pos (point))</span>
    (point)))


(defun rfringe-hide-region ()
  "<span class="quote">Hide any bitmap currently displayed in the fringe indicating the region.</span>"
  (interactive)
  (if rfringe-region-indicator-ovly
      (progn
        (delete-overlay rfringe-region-indicator-ovly)
        (setq rfringe-region-indicator-ovly nil))))



(defun rfringe-update-region-indicator (&optional buf)
  "<span class="quote">update any fringe indicator for the region, in the buffer BUF.</span>"
  (if (not buf)
      (setq buf (current-buffer)))
  (with-current-buffer buf
    (if rfringe-region-indicator-ovly
        (rfringe-show-region-indicator buf))))



(defun rfringe-insert-bitmap (bitmap pos &optional side face)
  "<span class="quote">Insert a fringe bitmap at POS.

BITMAP is the name of a bitmap defined with `define-fringe-bitmap'.
SIDE defaults to 'left-fringe and can also be
'right-fringe.  FACE is used to determine the bitmap's color.

The function returns an overlay object.
It should be removed when no longer needed via `delete-overlay'.
</span>"

  (let* ((display-string `(,(or side 'left-fringe) ,bitmap .
                           ,(when face (cons face nil))))
          (before-string (propertize "<span class="quote">!</span>" 'display display-string))
          (ov (make-overlay pos pos)))
    (overlay-put ov 'before-string before-string)
    (overlay-put ov 'fringe-helper t)
    ov))




(defun rfringe-create-relative-indicator (pos &optional dont-manage)
  "<span class="quote">Display an indicator in the fringe in the current buffer, for
the position POS relative to the buffer size, via a simple bitmap
dash.

If optional DONT-MANAGE is nil, or not present, the overlay is
stored and remembered.  In this case, if the window changes size,
or scrolls, the bitmap will be automatically moved. It can also
be deleted with `rfringe-remove-managed-indicators'. Passing
DONT-MANAGE as t does not do this.

For example, for a buffer of length 10000, if you pas a POS of
5000, then this funciton will display a dash in the fringe,
halfway down, regardless of whether char position 5000 is
visible in the window.

</span>"
  (let* ((top-of-window (window-start))
         (line-delta (scroll-bar-scale (cons pos (point-max)) (window-body-height)))
         (pos-of-indicator
          (rfringe--compute-position line-delta top-of-window))
         ov)
    <span class="linecomment">;;(message "tow(%d) ld(%d)" top-of-window line-delta)</span>
    <span class="linecomment">;;(message "compute x=&gt;x1: %d =&gt; %d" pos pos-of-indicator)</span>
    (setq ov (rfringe-insert-bitmap
              'rfringe-thin-dash
              pos-of-indicator
              'right-fringe
              'font-lock-warning-face))
    (if (not dont-manage)
        <span class="linecomment">;; save the location, and the actual overlay object</span>
        (push (cons pos ov) rfringe-managed-indicators))
    ov))




(defun rfringe-show-region-indicator (buf)
  "<span class="quote">Display an indicator in the fringe of the position of the region
in the buffer BUF, via a bitmap dash.

For example, if the region is at the top of the buffer, then a
dash will appear at the top of the fringe, regardless of whether
any part of the region is in fact visible in the window.</span>"

  (with-current-buffer buf
    (rfringe-hide-region)
    (if (mark) <span class="linecomment">;; the mark is set</span>
        (setq rfringe-region-indicator-ovly
              (rfringe-create-relative-indicator (min (point) (mark)) t)))))



(defun rfringe-remove-managed-indicators ()
  "<span class="quote">Removes all rfringe-managed indicators for the current buffer.</span>"
  (if rfringe-managed-indicators
      (progn
        (mapc (lambda (pair)
                (delete-overlay (cdr pair)))
              rfringe-managed-indicators)
        (setq rfringe-managed-indicators nil))))



(defun rfringe-show-region ()
  "<span class="quote">Display an indicator in the fringe, for the top of the region.</span>"
  (interactive)
  (rfringe-show-region-indicator (current-buffer)))



<span class="linecomment">;; hooks</span>

(defun rfringe--update-region-on-window-scroll (wnd new-start)
  "<span class="quote">a sort-of-hook that gets called as each window is scrolled.
The window is given by WND and the new start position is given
by NEW-START.

See `window-scroll-functions' for more info.
</span>"
  (if wnd
      (rfringe-update-region-indicator (window-buffer wnd))))



(defun rfringe--reset-region-indicator-on-window-config-change ()
  "<span class="quote">a sort-of-hook that gets called as a window's
\"configuration\" changes. Configuration includes size, width (I
guess), and so on. If the user splits or unsplits the window,
then the configuration changes, and this hook gets called.

This one resets the region indicator, if it is visible.

See `window-configuration-change-hook' for more info.
</span>"
  (if rfringe-region-indicator-ovly
      (rfringe-show-region)))




(defun rfringe--reset-visible-indicators ()
  "<span class="quote">a sort-of-hook that gets called as a window's
\"configuration\" changes. Configuration includes size, width (I
guess), and so on. Also, if the user splits or unsplits the
window, then the configuration changes, and this hook gets
called.

This fn moves all managed indicators.

See`window-configuration-change-hook' for more info.
</span>"
  (if rfringe-managed-indicators
      (progn
        <span class="linecomment">;;(message "rfringe resetting...")</span>
        (let* ((top-of-window (window-start))
               (bdy-height (window-body-height))
               (mx (point-max))
               (move-one
                (lambda (pair)
                  (let* ((pos (car pair))
                         (ov (cdr pair))
                         (line-delta (scroll-bar-scale (cons pos mx) bdy-height))
                         (ipos (rfringe--compute-position line-delta top-of-window)))
                    <span class="linecomment">;; (message "move %s to %s"</span>
                    <span class="linecomment">;;          (prin1-to-string ov)</span>
                    <span class="linecomment">;;          (prin1-to-string ipos))</span>
                    (move-overlay ov ipos ipos)))))
          (mapc move-one rfringe-managed-indicators)))))



(defun rfringe--update-managed-indicators-on-window-scroll (wnd new-start)
  "<span class="quote">a sort-of-hook that gets called as each window is scrolled.
The window is given by WND and the new start position is given
by NEW-START.

See `window-scroll-functions' for more info.
</span>"
  (if wnd
      (with-current-buffer (window-buffer wnd)
        (rfringe--reset-visible-indicators))))



<span class="linecomment">;; hooks for managing the 'special' region indicator</span>
(add-hook 'window-scroll-functions 'rfringe--update-region-on-window-scroll)
(add-hook 'window-configuration-change-hook
          'rfringe--reset-region-indicator-on-window-config-change)
(add-hook 'activate-mark-hook 'rfringe-update-region-indicator)


<span class="linecomment">;; hooks for managing all managed indicators</span>
(add-hook 'window-scroll-functions 'rfringe--update-managed-indicators-on-window-scroll)
(add-hook 'window-configuration-change-hook 'rfringe--reset-visible-indicators)


(defun rfringe--char-pos-for-line (line-no)
  (save-excursion
    (goto-line line-no)
    (point)))


<span class="linecomment">;; extend flymake to show fringe indicators</span>
(defadvice flymake-post-syntax-check (after
                                      rfringe-indicate-flymake-status
                                      activate compile)
  (rfringe-remove-managed-indicators)
  (let ((err-count (flymake-get-err-count flymake-err-info "<span class="quote">e</span>"))
        (warn-count (flymake-get-err-count flymake-err-info "<span class="quote">w</span>")))

    (if (or (/= 0 err-count) (/= 0 warn-count))
       (mapc (lambda (item)
               (message "<span class="quote">rfringe: marking error at line %d</span>" (car item))
               (rfringe-create-relative-indicator (rfringe--char-pos-for-line (car item))))
             flymake-err-info))))


(provide 'rfringe)</pre></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/%e3%82%b5%e3%82%a4%e3%83%88%e3%83%9e%e3%83%83%e3%83%97">サイトマップ</a> <a class="local" href="http://www.emacswiki.org/emacs/%e6%9b%b4%e6%96%b0%e5%b1%a5%e6%ad%b4">更新履歴</a> <a class="local" href="http://www.emacswiki.org/emacs/%e3%83%8b%e3%83%a5%e3%83%bc%e3%82%b9">ニュース</a> <a class="local" href="http://www.emacswiki.org/emacs/%ef%bc%a5%ef%bd%8c%ef%bd%89%ef%bd%93%ef%bd%90%e3%82%bb%e3%82%af%e3%82%b7%e3%83%a7%e3%83%b3">Ｅｌｉｓｐセクション</a> <a class="local" href="http://www.emacswiki.org/emacs/%e5%88%a9%e7%94%a8%e6%89%8b%e5%bc%95">利用手引</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=rfringe.el;missing=de_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=rfringe.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=rfringe.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=rfringe.el">Administration</a></span><span class="time"><br /> Last edited 2011-04-05 15:20 UTC by <a class="author" title="from mobile-166-137-140-245.mycingular.net" href="http://www.emacswiki.org/emacs/DinoChiesa">DinoChiesa</a></span><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>
