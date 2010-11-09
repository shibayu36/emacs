;;-*-coding: emacs-mule;-*-
(define-abbrev-table 'apropos-mode-abbrev-table '(
    ))

(define-abbrev-table 'awk-mode-abbrev-table '(
    ))

(define-abbrev-table 'c++-mode-abbrev-table '(
    ))

(define-abbrev-table 'c-mode-abbrev-table '(
    ))

(define-abbrev-table 'change-log-mode-abbrev-table '(
    ))

(define-abbrev-table 'comint-mode-abbrev-table '(
    ))

(define-abbrev-table 'conf-colon-mode-abbrev-table '(
    ))

(define-abbrev-table 'conf-javaprop-mode-abbrev-table '(
    ))

(define-abbrev-table 'conf-ppd-mode-abbrev-table '(
    ))

(define-abbrev-table 'conf-space-mode-abbrev-table '(
    ))

(define-abbrev-table 'conf-unix-mode-abbrev-table '(
    ))

(define-abbrev-table 'conf-windows-mode-abbrev-table '(
    ))

(define-abbrev-table 'conf-xdefaults-mode-abbrev-table '(
    ))

(define-abbrev-table 'cperl-mode-abbrev-table '(
    ("=head1" "=head1" cperl-electric-pod 0)
    ("=head2" "=head2" cperl-electric-pod 0)
    ("=over" "=over" cperl-electric-pod 0)
    ("=pod" "=pod" cperl-electric-pod 0)
    ("continue" "continue" cperl-electric-else 0)
    ("do" "do" cperl-electric-keyword 0)
    ("else" "else" cperl-electric-else 0)
    ("elsif" "elsif" cperl-electric-keyword 0)
    ("for" "for" cperl-electric-keyword 0)
    ("foreach" "foreach" cperl-electric-keyword 0)
    ("foreachmy" "foreachmy" cperl-electric-keyword 0)
    ("formy" "formy" cperl-electric-keyword 0)
    ("head1" "head1" cperl-electric-pod 0)
    ("head2" "head2" cperl-electric-pod 0)
    ("if" "if" cperl-electric-keyword 0)
    ("over" "over" cperl-electric-pod 0)
    ("pod" "pod" cperl-electric-pod 0)
    ("unless" "unless" cperl-electric-keyword 0)
    ("until" "until" cperl-electric-keyword 0)
    ("while" "while" cperl-electric-keyword 0)
    ))

(define-abbrev-table 'diff-mode-abbrev-table '(
    ))

(define-abbrev-table 'fundamental-mode-abbrev-table '(
    ))

(define-abbrev-table 'global-abbrev-table '(
    ("name" "expand-name" nil 2)
    ))

(define-abbrev-table 'grep-mode-abbrev-table '(
    ))

(define-abbrev-table 'html-helper-mode-abbrev-table '(
    ("%" "" (lambda nil (interactive) (snippet-insert "<% $. -%>")) 0)
    ("%%" "" (lambda nil (interactive) (snippet-insert "<%= $. %>")) 0)
    ("%for" "" (lambda nil (interactive) (snippet-insert "<% for $${elem} in @$${list} %>
$>$.
<% end %>$>")) 0)
    ("%h" "" (lambda nil (interactive) (snippet-insert "<%=h $${@item} %>")) 0)
    ("%if" "" (lambda nil (interactive) (snippet-insert "<% if $${cond} -%>
$.
<% end -%>")) 0)
    ("%ifel" "" (lambda nil (interactive) (snippet-insert "<% if $${cond} -%>
$.
<% else -%>
<% end -%>")) 0)
    ("%unless" "" (lambda nil (interactive) (snippet-insert "<% unless $${cond} -%>
$.
<% end -%>")) 0)
    ("body" "" (lambda nil (interactive) (snippet-insert "<body id=\"$${id}\" $${onload}>
$>$.
</body>")) 0)
    ("dchttr" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
$>\"http://www.w3.org/TR/html4/loose.dtd\">
")) 0)
    ("dcxml1" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
$>\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
")) 0)
    ("dcxmlf" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">
")) 0)
    ("dcxmls" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
")) 0)
    ("dcxmlt" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
")) 0)
    ("div" "" (lambda nil (interactive) (snippet-insert "<div>
$>$${paste}
</div>")) 0)
    ("dtht" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
$>\"http://www.w3.org/TR/html4/strict.dtd\">
")) 0)
    ("ff" "" (lambda nil (interactive) (snippet-insert "<%= form_for :$${item}, :action => \"$${update}\" %>
$.
<% end %>")) 0)
    ("form" "" (lambda nil (interactive) (snippet-insert "<form action=\"$${action}\" method=\"$${post}\" accept-charset=\"utf-8\">
$>$.

$><p><input type=\"submit\" value=\"Continue &rarr;\"/></p>
</form>")) 0)
    ("ft" "" (lambda nil (interactive) (snippet-insert "<%= form_tag :action => \"$${update}\" %>
$.
<%= end_form_tag %>")) 0)
    ("h" "" (lambda nil (interactive) (snippet-insert "<h1 id=\"$${alpha}\">$${paste}</h1>")) 0)
    ("head" "" (lambda nil (interactive) (snippet-insert "<head>
$><meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\"/>
$><title>$${title}</title>
$>$.
</head>")) 0)
    ("input" "" (lambda nil (interactive) (snippet-insert "<input type=\"$${button}\" name=\"$${some_name}\" value=\"$${3}\"$${id}>")) 0)
    ("lia" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :action => \"$${index}\" %>")) 0)
    ("liai" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :action => \"$${edit}\", :id => $${item} %>")) 0)
    ("lic" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :controller => \"$${items}\" %>")) 0)
    ("lica" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${index}\" %>")) 0)
    ("licai" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${item} %>")) 0)
    ("link" "" (lambda nil (interactive) (snippet-insert "<link rel=\"$${stylesheet}\" href=\"$${master}\" type=\"text/css\" media=\"$${screen}\" title=\"$${title}\" charset=\"$${utf}\"/>")) 0)
    ("mailto" "" (lambda nil (interactive) (snippet-insert "<a href=\"mailto:$${example}?subject=$${feedback}\">$${email}</a>")) 0)
    ("meta" "" (lambda nil (interactive) (snippet-insert "<meta name=\"$${name}\" content=\"$${content}\"/>")) 0)
    ("movie" "" (lambda nil (interactive) (snippet-insert "<object width=\"$${2}\" height=\"$${3}\" classid=\"clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B\" codebase=\"http://www.apple.com/qtactivex/qtplugin.cab\">
$><param name=\"src\" value=\"$${1}\"/>
$><param name=\"controller\" value=\"$${4}\"/>
$><param name=\"autoplay\" value=\"$${5}\"/>
$><embed src=\"$${movie}\"
$>$>width=\"$${320}\" height=\"$${240}\"
$>$>controller=\"$${true}\" autoplay=\"$${true}\"
$>$>scale=\"tofit\" cache=\"true\"
$>$>pluginspage=\"http://www.apple.com/quicktime/download/\"
$>/>
</object>")) 0)
    ("script" "" (lambda nil (interactive) (snippet-insert "<script type=\"text/javascript\" charset=\"utf-8\">
$>$.
</script>")) 0)
    ("scriptsrc" "" (lambda nil (interactive) (snippet-insert "<script src=\"$${1}\" type=\"text/javascript\" charset=\"$${utf}\"></script>")) 0)
    ("style" "" (lambda nil (interactive) (snippet-insert "<style type=\"text/css\" media=\"screen\">
$>$.
</style>")) 0)
    ("table" "" (lambda nil (interactive) (snippet-insert "<table border=\"$${0}\" $${cellpadding}>
$><tr><th>$${Header}</th></tr>
$><tr><td>$${Data}</td></tr>
</table>")) 0)
    ("textarea" "" (lambda nil (interactive) (snippet-insert "<textarea name=\"$${Name}\" rows=\"$${8}\" cols=\"$${40}\">$.</textarea>")) 0)
    ("title" "" (lambda nil (interactive) (snippet-insert "<title>$${title}</title>")) 0)
    ))

(define-abbrev-table 'html-mode-abbrev-table '(
    ("%" "" (lambda nil (interactive) (snippet-insert "<% $. -%>")) 0)
    ("%%" "" (lambda nil (interactive) (snippet-insert "<%= $. %>")) 0)
    ("%for" "" (lambda nil (interactive) (snippet-insert "<% for $${elem} in @$${list} %>
$>$.
<% end %>$>")) 0)
    ("%h" "" (lambda nil (interactive) (snippet-insert "<%=h $${@item} %>")) 0)
    ("%if" "" (lambda nil (interactive) (snippet-insert "<% if $${cond} -%>
$.
<% end -%>")) 0)
    ("%ifel" "" (lambda nil (interactive) (snippet-insert "<% if $${cond} -%>
$.
<% else -%>
<% end -%>")) 0)
    ("%unless" "" (lambda nil (interactive) (snippet-insert "<% unless $${cond} -%>
$.
<% end -%>")) 0)
    ("body" "" (lambda nil (interactive) (snippet-insert "<body id=\"$${id}\" $${onload}>
$>$.
</body>")) 0)
    ("dchttr" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
$>\"http://www.w3.org/TR/html4/loose.dtd\">
")) 0)
    ("dcxml1" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
$>\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
")) 0)
    ("dcxmlf" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">
")) 0)
    ("dcxmls" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
")) 0)
    ("dcxmlt" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
")) 0)
    ("div" "" (lambda nil (interactive) (snippet-insert "<div>
$>$${paste}
</div>")) 0)
    ("dtht" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
$>\"http://www.w3.org/TR/html4/strict.dtd\">
")) 0)
    ("ff" "" (lambda nil (interactive) (snippet-insert "<%= form_for :$${item}, :action => \"$${update}\" %>
$.
<% end %>")) 0)
    ("form" "" (lambda nil (interactive) (snippet-insert "<form action=\"$${action}\" method=\"$${post}\" accept-charset=\"utf-8\">
$>$.

$><p><input type=\"submit\" value=\"Continue &rarr;\"/></p>
</form>")) 0)
    ("ft" "" (lambda nil (interactive) (snippet-insert "<%= form_tag :action => \"$${update}\" %>
$.
<%= end_form_tag %>")) 0)
    ("h" "" (lambda nil (interactive) (snippet-insert "<h1 id=\"$${alpha}\">$${paste}</h1>")) 0)
    ("head" "" (lambda nil (interactive) (snippet-insert "<head>
$><meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\"/>
$><title>$${title}</title>
$>$.
</head>")) 0)
    ("input" "" (lambda nil (interactive) (snippet-insert "<input type=\"$${button}\" name=\"$${some_name}\" value=\"$${3}\"$${id}>")) 0)
    ("lia" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :action => \"$${index}\" %>")) 0)
    ("liai" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :action => \"$${edit}\", :id => $${item} %>")) 0)
    ("lic" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :controller => \"$${items}\" %>")) 0)
    ("lica" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${index}\" %>")) 0)
    ("licai" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${item} %>")) 0)
    ("link" "" (lambda nil (interactive) (snippet-insert "<link rel=\"$${stylesheet}\" href=\"$${master}\" type=\"text/css\" media=\"$${screen}\" title=\"$${title}\" charset=\"$${utf}\"/>")) 0)
    ("mailto" "" (lambda nil (interactive) (snippet-insert "<a href=\"mailto:$${example}?subject=$${feedback}\">$${email}</a>")) 0)
    ("meta" "" (lambda nil (interactive) (snippet-insert "<meta name=\"$${name}\" content=\"$${content}\"/>")) 0)
    ("movie" "" (lambda nil (interactive) (snippet-insert "<object width=\"$${2}\" height=\"$${3}\" classid=\"clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B\" codebase=\"http://www.apple.com/qtactivex/qtplugin.cab\">
$><param name=\"src\" value=\"$${1}\"/>
$><param name=\"controller\" value=\"$${4}\"/>
$><param name=\"autoplay\" value=\"$${5}\"/>
$><embed src=\"$${movie}\"
$>$>width=\"$${320}\" height=\"$${240}\"
$>$>controller=\"$${true}\" autoplay=\"$${true}\"
$>$>scale=\"tofit\" cache=\"true\"
$>$>pluginspage=\"http://www.apple.com/quicktime/download/\"
$>/>
</object>")) 0)
    ("script" "" (lambda nil (interactive) (snippet-insert "<script type=\"text/javascript\" charset=\"utf-8\">
$>$.
</script>")) 0)
    ("scriptsrc" "" (lambda nil (interactive) (snippet-insert "<script src=\"$${1}\" type=\"text/javascript\" charset=\"$${utf}\"></script>")) 0)
    ("style" "" (lambda nil (interactive) (snippet-insert "<style type=\"text/css\" media=\"screen\">
$>$.
</style>")) 0)
    ("table" "" (lambda nil (interactive) (snippet-insert "<table border=\"$${0}\" $${cellpadding}>
$><tr><th>$${Header}</th></tr>
$><tr><td>$${Data}</td></tr>
</table>")) 0)
    ("textarea" "" (lambda nil (interactive) (snippet-insert "<textarea name=\"$${Name}\" rows=\"$${8}\" cols=\"$${40}\">$.</textarea>")) 0)
    ("title" "" (lambda nil (interactive) (snippet-insert "<title>$${title}</title>")) 0)
    ))

(define-abbrev-table 'idl-mode-abbrev-table '(
    ))

(define-abbrev-table 'java-mode-abbrev-table '(
    ))

(define-abbrev-table 'lisp-mode-abbrev-table '(
    ))

(define-abbrev-table 'log-edit-mode-abbrev-table '(
    ))

(define-abbrev-table 'magit-log-edit-mode-abbrev-table '(
    ))

(define-abbrev-table 'nxml-mode-abbrev-table '(
    ("%" "" (lambda nil (interactive) (snippet-insert "<% $. -%>")) 0)
    ("%%" "" (lambda nil (interactive) (snippet-insert "<%= $. %>")) 0)
    ("%for" "" (lambda nil (interactive) (snippet-insert "<% for $${elem} in @$${list} %>
$>$.
<% end %>$>")) 0)
    ("%h" "" (lambda nil (interactive) (snippet-insert "<%=h $${@item} %>")) 0)
    ("%if" "" (lambda nil (interactive) (snippet-insert "<% if $${cond} -%>
$.
<% end -%>")) 0)
    ("%ifel" "" (lambda nil (interactive) (snippet-insert "<% if $${cond} -%>
$.
<% else -%>
<% end -%>")) 0)
    ("%unless" "" (lambda nil (interactive) (snippet-insert "<% unless $${cond} -%>
$.
<% end -%>")) 0)
    ("body" "" (lambda nil (interactive) (snippet-insert "<body id=\"$${id}\" $${onload}>
$>$.
</body>")) 0)
    ("dchttr" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
$>\"http://www.w3.org/TR/html4/loose.dtd\">
")) 0)
    ("dcxml1" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
$>\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
")) 0)
    ("dcxmlf" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">
")) 0)
    ("dcxmls" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
")) 0)
    ("dcxmlt" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
")) 0)
    ("div" "" (lambda nil (interactive) (snippet-insert "<div>
$>$${paste}
</div>")) 0)
    ("dtht" "" (lambda nil (interactive) (snippet-insert "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
$>\"http://www.w3.org/TR/html4/strict.dtd\">
")) 0)
    ("ff" "" (lambda nil (interactive) (snippet-insert "<%= form_for :$${item}, :action => \"$${update}\" %>
$.
<% end %>")) 0)
    ("form" "" (lambda nil (interactive) (snippet-insert "<form action=\"$${action}\" method=\"$${post}\" accept-charset=\"utf-8\">
$>$.

$><p><input type=\"submit\" value=\"Continue &rarr;\"/></p>
</form>")) 0)
    ("ft" "" (lambda nil (interactive) (snippet-insert "<%= form_tag :action => \"$${update}\" %>
$.
<%= end_form_tag %>")) 0)
    ("h" "" (lambda nil (interactive) (snippet-insert "<h1 id=\"$${alpha}\">$${paste}</h1>")) 0)
    ("head" "" (lambda nil (interactive) (snippet-insert "<head>
$><meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\"/>
$><title>$${title}</title>
$>$.
</head>")) 0)
    ("input" "" (lambda nil (interactive) (snippet-insert "<input type=\"$${button}\" name=\"$${some_name}\" value=\"$${3}\"$${id}>")) 0)
    ("lia" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :action => \"$${index}\" %>")) 0)
    ("liai" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :action => \"$${edit}\", :id => $${item} %>")) 0)
    ("lic" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :controller => \"$${items}\" %>")) 0)
    ("lica" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${index}\" %>")) 0)
    ("licai" "" (lambda nil (interactive) (snippet-insert "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${item} %>")) 0)
    ("link" "" (lambda nil (interactive) (snippet-insert "<link rel=\"$${stylesheet}\" href=\"$${master}\" type=\"text/css\" media=\"$${screen}\" title=\"$${title}\" charset=\"$${utf}\"/>")) 0)
    ("mailto" "" (lambda nil (interactive) (snippet-insert "<a href=\"mailto:$${example}?subject=$${feedback}\">$${email}</a>")) 0)
    ("meta" "" (lambda nil (interactive) (snippet-insert "<meta name=\"$${name}\" content=\"$${content}\"/>")) 0)
    ("movie" "" (lambda nil (interactive) (snippet-insert "<object width=\"$${2}\" height=\"$${3}\" classid=\"clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B\" codebase=\"http://www.apple.com/qtactivex/qtplugin.cab\">
$><param name=\"src\" value=\"$${1}\"/>
$><param name=\"controller\" value=\"$${4}\"/>
$><param name=\"autoplay\" value=\"$${5}\"/>
$><embed src=\"$${movie}\"
$>$>width=\"$${320}\" height=\"$${240}\"
$>$>controller=\"$${true}\" autoplay=\"$${true}\"
$>$>scale=\"tofit\" cache=\"true\"
$>$>pluginspage=\"http://www.apple.com/quicktime/download/\"
$>/>
</object>")) 0)
    ("script" "" (lambda nil (interactive) (snippet-insert "<script type=\"text/javascript\" charset=\"utf-8\">
$>$.
</script>")) 0)
    ("scriptsrc" "" (lambda nil (interactive) (snippet-insert "<script src=\"$${1}\" type=\"text/javascript\" charset=\"$${utf}\"></script>")) 0)
    ("style" "" (lambda nil (interactive) (snippet-insert "<style type=\"text/css\" media=\"screen\">
$>$.
</style>")) 0)
    ("table" "" (lambda nil (interactive) (snippet-insert "<table border=\"$${0}\" $${cellpadding}>
$><tr><th>$${Header}</th></tr>
$><tr><td>$${Data}</td></tr>
</table>")) 0)
    ("textarea" "" (lambda nil (interactive) (snippet-insert "<textarea name=\"$${Name}\" rows=\"$${8}\" cols=\"$${40}\">$.</textarea>")) 0)
    ("title" "" (lambda nil (interactive) (snippet-insert "<title>$${title}</title>")) 0)
    ))

(define-abbrev-table 'objc-mode-abbrev-table '(
    ))

(define-abbrev-table 'php-mode-abbrev-table '(
    ("ex" "extends" nil 0)
    ))

(define-abbrev-table 'pike-mode-abbrev-table '(
    ))

(define-abbrev-table 'rails-controller-minor-mode-abbrev-table '(
    ("af" "" (lambda nil (interactive) (snippet-insert "after_filter :$${filter}")) 0)
    ("arf" "" (lambda nil (interactive) (snippet-insert "around_filter :$${filter}")) 0)
    ("bf" "" (lambda nil (interactive) (snippet-insert "before_filter :$${filter}")) 0)
    ("ra" "" (lambda nil (interactive) (snippet-insert "render :action => '$${index}'")) 0)
    ("ral" "" (lambda nil (interactive) (snippet-insert "render :action => '$${index}', :layout => '{default}'")) 0)
    ("rcea" "" (lambda nil (interactive) (snippet-insert "render_component :action => '$${index}'")) 0)
    ("rcec" "" (lambda nil (interactive) (snippet-insert "render_component :controller => '$${items}'")) 0)
    ("rceca" "" (lambda nil (interactive) (snippet-insert "render_component :controller => '$${items}', :action => '$${index}'")) 0)
    ("rcreate" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-create}")) 0)
    ("rdestroy" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-destroy}")) 0)
    ("rea" "" (lambda nil (interactive) (snippet-insert "redirect_to :action => '$${index}'")) 0)
    ("reai" "" (lambda nil (interactive) (snippet-insert "redirect_to :action => '$${show}', :id => $${item}")) 0)
    ("rec" "" (lambda nil (interactive) (snippet-insert "redirect_to :controller => '$${items}'")) 0)
    ("reca" "" (lambda nil (interactive) (snippet-insert "redirect_to :controller => '$${items}', :action => '$${list}'")) 0)
    ("recai" "" (lambda nil (interactive) (snippet-insert "redirect_to :controller => '$${items}', :action => '$${show}', :id => $${item}")) 0)
    ("redit" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-edit}")) 0)
    ("rest" "" (lambda nil (interactive) (snippet-insert "respond_to do |format|
$>format.html$>$.
end$>")) 0)
    ("rf" "" (lambda nil (interactive) (snippet-insert "render :file => '$${filepath}'")) 0)
    ("rfu" "" (lambda nil (interactive) (snippet-insert "render :file => '$${filepath}', :use_full_path => $${false}")) 0)
    ("ri" "" (lambda nil (interactive) (snippet-insert "render :inline => '$${hello}'")) 0)
    ("ril" "" (lambda nil (interactive) (snippet-insert "render :inline => '$${hello}', :locals => { $${name} => '$${value}'$${4} }")) 0)
    ("rindex" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-index}")) 0)
    ("rit" "" (lambda nil (interactive) (snippet-insert "render :inline => '$${hello}', :type => $${rxml}")) 0)
    ("rl" "" (lambda nil (interactive) (snippet-insert "render :layout => '$${layoutname}'")) 0)
    ("rn" "" (lambda nil (interactive) (snippet-insert "render :nothing => $${true}")) 0)
    ("rnew" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-new}")) 0)
    ("rns" "" (lambda nil (interactive) (snippet-insert "render :nothing => $${true}, :status => $${401}")) 0)
    ("rp" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}'")) 0)
    ("rpc" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :collection => $${items}")) 0)
    ("rpl" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :locals => { :$${name} => '$${value}'$${4} }")) 0)
    ("rpo" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :object => $${object}")) 0)
    ("rps" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :status => $${500}")) 0)
    ("rshow" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-show}")) 0)
    ("rt" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}'")) 0)
    ("rtl" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}', :layout => '$${layoutname}'")) 0)
    ("rtlt" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}', :layout => $${true}")) 0)
    ("rts" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}', :status => $${401}")) 0)
    ("ru" "" (lambda nil (interactive) (snippet-insert "render :update do |page|
$>$.
end$>")) 0)
    ("rupdate" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-update}")) 0)
    ))

(define-abbrev-table 'rails-functional-test-minor-mode-abbrev-table '(
    ("art" "" (lambda nil (interactive) (snippet-insert "assert_redirected_to :action => '$${index}'")) 0)
    ("as" "" (lambda nil (interactive) (snippet-insert "assert $${test}")) 0)
    ("asa" "" (lambda nil (interactive) (snippet-insert "assert assigns(:$${,rails-snippets-feature:model-name})")) 0)
    ("ase" "" (lambda nil (interactive) (snippet-insert "assert_equal $${expected}, $${actual}")) 0)
    ("asid" "" (lambda nil (interactive) (snippet-insert "assert_in_delta $${expected_float}, $${actual_float}, $${20}")) 0)
    ("asio" "" (lambda nil (interactive) (snippet-insert "assert_instance_of $${ExpectedClass}, $${actual_instance}")) 0)
    ("asko" "" (lambda nil (interactive) (snippet-insert "assert_kind_of $${ExpectedKind}, $${actual_instance}")) 0)
    ("asm" "" (lambda nil (interactive) (snippet-insert "assert_match(/$${expected_pattern}/, $${actual_string})")) 0)
    ("asn" "" (lambda nil (interactive) (snippet-insert "assert_nil $${instance}")) 0)
    ("asne" "" (lambda nil (interactive) (snippet-insert "assert_not_equal $${unexpected}, $${actual}")) 0)
    ("asnm" "" (lambda nil (interactive) (snippet-insert "assert_no_match(/$${unexpected_pattern}/, $${actual_string})")) 0)
    ("asnn" "" (lambda nil (interactive) (snippet-insert "assert_not_nil $${instance}")) 0)
    ("asnr" "" (lambda nil (interactive) (snippet-insert "assert_nothing_raised $${Exception}  { $. }")) 0)
    ("asns" "" (lambda nil (interactive) (snippet-insert "assert_not_same $${unexpected}, $${actual}")) 0)
    ("asnt" "" (lambda nil (interactive) (snippet-insert "assert_nothing_thrown { $. }")) 0)
    ("aso" "" (lambda nil (interactive) (snippet-insert "assert_operator $${left}, :$${operator}, $${right}")) 0)
    ("asr" "" (lambda nil (interactive) (snippet-insert "assert_raise $${Exception} { $. }")) 0)
    ("asre" "" (lambda nil (interactive) (snippet-insert "assert_response :$${success}")) 0)
    ("asrt" "" (lambda nil (interactive) (snippet-insert "assert_respond_to $${object}, :$${method}")) 0)
    ("ass" "" (lambda nil (interactive) (snippet-insert "assert_same $${expected}, $${actual}")) 0)
    ("assd" "" (lambda nil (interactive) (snippet-insert "assert_send [$${object}, :$${message}, $${args}]")) 0)
    ("ast" "" (lambda nil (interactive) (snippet-insert "assert_throws :$${expected} { $. }")) 0)
    ("astm" "" (lambda nil (interactive) (snippet-insert "assert_template '$${index}'")) 0)
    ("fix" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:fixture}(:$${one})$.")) 0)
    ("rcreate" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-create}")) 0)
    ("rdestroy" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-destroy}")) 0)
    ("redit" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-edit}")) 0)
    ("rindex" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-index}")) 0)
    ("rnew" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-new}")) 0)
    ("rshow" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-show}")) 0)
    ("rupdate" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-update}")) 0)
    ))

(define-abbrev-table 'rails-helper-minor-mode-abbrev-table '(
    ("ra" "" (lambda nil (interactive) (snippet-insert "render :action => '$${index}'")) 0)
    ("ral" "" (lambda nil (interactive) (snippet-insert "render :action => '$${index}', :layout => '{default}'")) 0)
    ("rcea" "" (lambda nil (interactive) (snippet-insert "render_component :action => '$${index}'")) 0)
    ("rcec" "" (lambda nil (interactive) (snippet-insert "render_component :controller => '$${items}'")) 0)
    ("rceca" "" (lambda nil (interactive) (snippet-insert "render_component :controller => '$${items}', :action => '$${index}'")) 0)
    ("rcreate" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-create}")) 0)
    ("rdestroy" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-destroy}")) 0)
    ("rea" "" (lambda nil (interactive) (snippet-insert "redirect_to :action => '$${index}'")) 0)
    ("reai" "" (lambda nil (interactive) (snippet-insert "redirect_to :action => '$${show}', :id => $${item}")) 0)
    ("rec" "" (lambda nil (interactive) (snippet-insert "redirect_to :controller => '$${items}'")) 0)
    ("reca" "" (lambda nil (interactive) (snippet-insert "redirect_to :controller => '$${items}', :action => '$${list}'")) 0)
    ("recai" "" (lambda nil (interactive) (snippet-insert "redirect_to :controller => '$${items}', :action => '$${show}', :id => $${item}")) 0)
    ("redit" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-edit}")) 0)
    ("rf" "" (lambda nil (interactive) (snippet-insert "render :file => '$${filepath}'")) 0)
    ("rfu" "" (lambda nil (interactive) (snippet-insert "render :file => '$${filepath}', :use_full_path => $${false}")) 0)
    ("ri" "" (lambda nil (interactive) (snippet-insert "render :inline => '$${hello}'")) 0)
    ("ril" "" (lambda nil (interactive) (snippet-insert "render :inline => '$${hello}', :locals => { $${name} => '$${value}'$${4} }")) 0)
    ("rindex" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-index}")) 0)
    ("rit" "" (lambda nil (interactive) (snippet-insert "render :inline => '$${hello}', :type => $${rxml}")) 0)
    ("rl" "" (lambda nil (interactive) (snippet-insert "render :layout => '$${layoutname}'")) 0)
    ("rn" "" (lambda nil (interactive) (snippet-insert "render :nothing => $${true}")) 0)
    ("rnew" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-new}")) 0)
    ("rns" "" (lambda nil (interactive) (snippet-insert "render :nothing => $${true}, :status => $${401}")) 0)
    ("rp" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}'")) 0)
    ("rpc" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :collection => $${items}")) 0)
    ("rpl" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :locals => { :$${name} => '$${value}'$${4} }")) 0)
    ("rpo" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :object => $${object}")) 0)
    ("rps" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :status => $${500}")) 0)
    ("rshow" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-show}")) 0)
    ("rt" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}'")) 0)
    ("rtl" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}', :layout => '$${layoutname}'")) 0)
    ("rtlt" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}', :layout => $${true}")) 0)
    ("rts" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}', :status => $${401}")) 0)
    ("rupdate" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-update}")) 0)
    ))

(define-abbrev-table 'rails-migration-minor-mode-abbrev-table '(
    ("acl" "" (lambda nil (interactive) (snippet-insert "add_column :$${,rails-snippets-feature:migration-table-name}, :$${column}, :$${string}")) 0)
    ("ai" "" (lambda nil (interactive) (snippet-insert "add_index :$${,rails-snippets-feature:migration-table-name}, $${column}")) 0)
    ("aiu" "" (lambda nil (interactive) (snippet-insert "add_index :$${,rails-snippets-feature:migration-table-name}, $${column}, :unique => true")) 0)
    ("ct" "" (lambda nil (interactive) (snippet-insert "create_table :$${,rails-snippets-feature:migration-table-name} do |t|
$>tcls$.
end$>")) 0)
    ("dt" "" (lambda nil (interactive) (snippet-insert "drop_table :$${,rails-snippets-feature:migration-table-name}$.")) 0)
    ("recl" "" (lambda nil (interactive) (snippet-insert "rename_column :$${column}, :$${new_column}")) 0)
    ("ret" "" (lambda nil (interactive) (snippet-insert "rename_table :$${,rails-snippets-feature:migration-table-name}, :$${new_name}$.")) 0)
    ("rmcl" "" (lambda nil (interactive) (snippet-insert "remove_column :$${,rails-snippets-feature:migration-table-name}, :$${column}")) 0)
    ("tcl" "" (lambda nil (interactive) (snippet-insert "t.column :$${title}, :$${string}$.")) 0)
    ("tcln" "" (lambda nil (interactive) (snippet-insert "t.column :$${title}, :$${string}, :null => false$.")) 0)
    ("tcls" "" (lambda nil (interactive) (snippet-insert "t.column :$${title}, :$${string}
$>tcls$.")) 0)
    ))

(define-abbrev-table 'rails-model-minor-mode-abbrev-table '(
    ("bt" "" (lambda nil (interactive) (snippet-insert "belongs_to :$${model}")) 0)
    ("habtm" "" (lambda nil (interactive) (snippet-insert "has_and_belongs_to_many :$${object}")) 0)
    ("hm" "" (lambda nil (interactive) (snippet-insert "has_many :$${objects}")) 0)
    ("hmt" "" (lambda nil (interactive) (snippet-insert "has_many :$${objects}, :through => :$${,rails-snippets-feature:prev-has-many-table-name}")) 0)
    ("ho" "" (lambda nil (interactive) (snippet-insert "has_one :$${object}")) 0)
    ("va" "" (lambda nil (interactive) (snippet-insert "validates_associated :$${attribute}")) 0)
    ("vc" "" (lambda nil (interactive) (snippet-insert "validates_confirmation_of :$${attribute}")) 0)
    ("ve" "" (lambda nil (interactive) (snippet-insert "validates_exclusion_of :$${attribute}")) 0)
    ("vl" "" (lambda nil (interactive) (snippet-insert "validates_length_of :$${attribute}, :within => $${20}")) 0)
    ("vp" "" (lambda nil (interactive) (snippet-insert "validates_presence_of :$${attribute}")) 0)
    ("vpif" "" (lambda nil (interactive) (snippet-insert "validates_presence_of :$${attribute}, :if => proc { |obj| $${condition} }")) 0)
    ("vu" "" (lambda nil (interactive) (snippet-insert "validates_uniqueness_of :$${attribute}")) 0)
    ))

(define-abbrev-table 'rails-script:output-mode-abbrev-table '(
    ))

(define-abbrev-table 'rails-test:compilation-mode-abbrev-table '(
    ))

(define-abbrev-table 'rails-unit-test-minor-mode-abbrev-table '(
    ("art" "" (lambda nil (interactive) (snippet-insert "assert_redirected_to :action => '$${index}'")) 0)
    ("as" "" (lambda nil (interactive) (snippet-insert "assert $${test}")) 0)
    ("asa" "" (lambda nil (interactive) (snippet-insert "assert assigns(:$${,rails-snippets-feature:model-name})")) 0)
    ("ase" "" (lambda nil (interactive) (snippet-insert "assert_equal $${expected}, $${actual}")) 0)
    ("asid" "" (lambda nil (interactive) (snippet-insert "assert_in_delta $${expected_float}, $${actual_float}, $${20}")) 0)
    ("asio" "" (lambda nil (interactive) (snippet-insert "assert_instance_of $${ExpectedClass}, $${actual_instance}")) 0)
    ("asko" "" (lambda nil (interactive) (snippet-insert "assert_kind_of $${ExpectedKind}, $${actual_instance}")) 0)
    ("asm" "" (lambda nil (interactive) (snippet-insert "assert_match(/$${expected_pattern}/, $${actual_string})")) 0)
    ("asn" "" (lambda nil (interactive) (snippet-insert "assert_nil $${instance}")) 0)
    ("asne" "" (lambda nil (interactive) (snippet-insert "assert_not_equal $${unexpected}, $${actual}")) 0)
    ("asnm" "" (lambda nil (interactive) (snippet-insert "assert_no_match(/$${unexpected_pattern}/, $${actual_string})")) 0)
    ("asnn" "" (lambda nil (interactive) (snippet-insert "assert_not_nil $${instance}")) 0)
    ("asnr" "" (lambda nil (interactive) (snippet-insert "assert_nothing_raised $${Exception}  { $. }")) 0)
    ("asns" "" (lambda nil (interactive) (snippet-insert "assert_not_same $${unexpected}, $${actual}")) 0)
    ("asnt" "" (lambda nil (interactive) (snippet-insert "assert_nothing_thrown { $. }")) 0)
    ("aso" "" (lambda nil (interactive) (snippet-insert "assert_operator $${left}, :$${operator}, $${right}")) 0)
    ("asr" "" (lambda nil (interactive) (snippet-insert "assert_raise $${Exception} { $. }")) 0)
    ("asre" "" (lambda nil (interactive) (snippet-insert "assert_response :$${success}")) 0)
    ("asrt" "" (lambda nil (interactive) (snippet-insert "assert_respond_to $${object}, :$${method}")) 0)
    ("ass" "" (lambda nil (interactive) (snippet-insert "assert_same $${expected}, $${actual}")) 0)
    ("assd" "" (lambda nil (interactive) (snippet-insert "assert_send [$${object}, :$${message}, $${args}]")) 0)
    ("ast" "" (lambda nil (interactive) (snippet-insert "assert_throws :$${expected} { $. }")) 0)
    ("astm" "" (lambda nil (interactive) (snippet-insert "assert_template '$${index}'")) 0)
    ("fix" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:fixture}(:$${one})$.")) 0)
    ))

(define-abbrev-table 'rails-view-minor-mode-abbrev-table '(
    ("ra" "" (lambda nil (interactive) (snippet-insert "render :action => '$${index}'")) 0)
    ("ral" "" (lambda nil (interactive) (snippet-insert "render :action => '$${index}', :layout => '{default}'")) 0)
    ("rcea" "" (lambda nil (interactive) (snippet-insert "render_component :action => '$${index}'")) 0)
    ("rcec" "" (lambda nil (interactive) (snippet-insert "render_component :controller => '$${items}'")) 0)
    ("rceca" "" (lambda nil (interactive) (snippet-insert "render_component :controller => '$${items}', :action => '$${index}'")) 0)
    ("rcreate" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-create}")) 0)
    ("rdestroy" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-destroy}")) 0)
    ("rea" "" (lambda nil (interactive) (snippet-insert "redirect_to :action => '$${index}'")) 0)
    ("reai" "" (lambda nil (interactive) (snippet-insert "redirect_to :action => '$${show}', :id => $${item}")) 0)
    ("rec" "" (lambda nil (interactive) (snippet-insert "redirect_to :controller => '$${items}'")) 0)
    ("reca" "" (lambda nil (interactive) (snippet-insert "redirect_to :controller => '$${items}', :action => '$${list}'")) 0)
    ("recai" "" (lambda nil (interactive) (snippet-insert "redirect_to :controller => '$${items}', :action => '$${show}', :id => $${item}")) 0)
    ("redit" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-edit}")) 0)
    ("rf" "" (lambda nil (interactive) (snippet-insert "render :file => '$${filepath}'")) 0)
    ("rfu" "" (lambda nil (interactive) (snippet-insert "render :file => '$${filepath}', :use_full_path => $${false}")) 0)
    ("ri" "" (lambda nil (interactive) (snippet-insert "render :inline => '$${hello}'")) 0)
    ("ril" "" (lambda nil (interactive) (snippet-insert "render :inline => '$${hello}', :locals => { $${name} => '$${value}'$${4} }")) 0)
    ("rindex" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-index}")) 0)
    ("rit" "" (lambda nil (interactive) (snippet-insert "render :inline => '$${hello}', :type => $${rxml}")) 0)
    ("rl" "" (lambda nil (interactive) (snippet-insert "render :layout => '$${layoutname}'")) 0)
    ("rn" "" (lambda nil (interactive) (snippet-insert "render :nothing => $${true}")) 0)
    ("rnew" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-new}")) 0)
    ("rns" "" (lambda nil (interactive) (snippet-insert "render :nothing => $${true}, :status => $${401}")) 0)
    ("rp" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}'")) 0)
    ("rpc" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :collection => $${items}")) 0)
    ("rpl" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :locals => { :$${name} => '$${value}'$${4} }")) 0)
    ("rpo" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :object => $${object}")) 0)
    ("rps" "" (lambda nil (interactive) (snippet-insert "render :partial => '$${item}', :status => $${500}")) 0)
    ("rshow" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-show}")) 0)
    ("rt" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}'")) 0)
    ("rtl" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}', :layout => '$${layoutname}'")) 0)
    ("rtlt" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}', :layout => $${true}")) 0)
    ("rts" "" (lambda nil (interactive) (snippet-insert "render :text => '$${render}', :status => $${401}")) 0)
    ("rupdate" "" (lambda nil (interactive) (snippet-insert "$${,rails-snippets-feature:rest-update}")) 0)
    ))

(define-abbrev-table 'ruby-mode-abbrev-table '(
    (":" "" (lambda nil (interactive) (snippet-insert ":$${key} => '$${value}'")) 0)
    ("all" "" (lambda nil (interactive) (snippet-insert "all? { |$${e}| $. }")) 0)
    ("am" "" (lambda nil (interactive) (snippet-insert "alias_method :$${new_name}, :$${old_name}")) 0)
    ("amc" "" (lambda nil (interactive) (snippet-insert "alias_method_chain :$${first_method}, :$${second_method}")) 0)
    ("any" "" (lambda nil (interactive) (snippet-insert "any? { |$${e}| $. }")) 0)
    ("array" "" (lambda nil (interactive) (snippet-insert "Array.new($${10}) { |$${i}|$. }")) 0)
    ("b" "" (lambda nil (interactive) (snippet-insert "=begin rdoc
$>$.
=end")) 0)
    ("begin" "" (lambda nil (interactive) (snippet-insert "begin
$>$${paste}
rescue $${Exception} => $${e}
$>$.
end$>
")) 0)
    ("bm" "" (lambda nil (interactive) (snippet-insert "TESTS = $${10_000}
Benchmark.bmbm($${10}) do |results|
  $.
end$>")) 0)
    ("cl" "" (lambda nil (interactive) (snippet-insert "classify { |$${e}| $. }")) 0)
    ("cladl" "" (lambda nil (interactive) (snippet-insert "class $${ClassName} < DelegateClass($${ParentClass})
$>def initialize$${1}
$>$>super($${del_obj})
$>$>
$>$>$.
end$>
$>
$>
end$>")) 0)
    ("clapr" "" (lambda nil (interactive) (snippet-insert "class $${ClassName} < $${ParentClass}
$>def initialize$${1}
$>$>$.
end$>
$>
$>
end$>")) 0)
    ("clasf" "" (lambda nil (interactive) (snippet-insert "class << $${self}
$>$.
end$>")) 0)
    ("class" "" (lambda nil (interactive) (snippet-insert "class $${ClassName}
$>$.
end$>")) 0)
    ("classi" "" (lambda nil (interactive) (snippet-insert "class $${ClassName}
$>def initialize$${1}
$>$>$.
end$>
$>
$>
end$>")) 0)
    ("clast" "" (lambda nil (interactive) (snippet-insert "class $${ClassName} < Struct.new(:$${attr_names})
$>def initialize(*args)
$>$>super
$>$>
$>$>$.
end$>
$>
$>
end$>")) 0)
    ("col" "" (lambda nil (interactive) (snippet-insert "collect { |$${e}| $. }")) 0)
    ("collect" "" (lambda nil (interactive) (snippet-insert "collect { |$${element}| $${element}.$. }")) 0)
    ("comp" "" (lambda nil (interactive) (snippet-insert "include Comparable

def <=>(other)
$>$.
end$>")) 0)
    ("dee" "" (lambda nil (interactive) (snippet-insert "Marshal.load(Marshal.dump($${obj_to_copy}))")) 0)
    ("defd" "" (lambda nil (interactive) (snippet-insert "def_delegator :$${del_obj}, :$${del_meth}, :$${new_name}")) 0)
    ("defds" "" (lambda nil (interactive) (snippet-insert "def_delegators :$${del_obj}, :$${del_methods}")) 0)
    ("defs" "" (lambda nil (interactive) (snippet-insert "def self.$${class_method_name}
$>$.
end$>")) 0)
    ("deft" "" (lambda nil (interactive) (snippet-insert "def test_$${case_name}
$>$.
end$>")) 0)
    ("deli" "" (lambda nil (interactive) (snippet-insert "delete_if { |$${e}| $. }")) 0)
    ("det" "" (lambda nil (interactive) (snippet-insert "detect { |$${e}| $. }")) 0)
    ("dir" "" (lambda nil (interactive) (snippet-insert "Dir.glob($${glob}) { |$${file}| $. }")) 0)
    ("do" "" (lambda nil (interactive) (snippet-insert "do
$>$.
end$>")) 0)
    ("doo" "" (lambda nil (interactive) (snippet-insert "do |$${object}|
$>$.
end$>")) 0)
    ("dow" "" (lambda nil (interactive) (snippet-insert "downto($${0}) { |$${n}|$. }")) 0)
    ("ea" "" (lambda nil (interactive) (snippet-insert "each { |$${e}| $. }")) 0)
    ("eab" "" (lambda nil (interactive) (snippet-insert "each_byte { |$${byte}| $. }")) 0)
    ("eac" "" (lambda nil (interactive) (snippet-insert "each_cons($${2}) { |$${group}| $. }")) 0)
    ("each" "" (lambda nil (interactive) (snippet-insert "each { |$${element}| $${element}.$. }")) 0)
    ("eai" "" (lambda nil (interactive) (snippet-insert "each_index { |$${i}| $. }")) 0)
    ("eak" "" (lambda nil (interactive) (snippet-insert "each_key { |$${key}| $. }")) 0)
    ("eal" "" (lambda nil (interactive) (snippet-insert "each_line$${1} { |$${line}| $. }")) 0)
    ("eap" "" (lambda nil (interactive) (snippet-insert "each_pair { |$${name}, $${val}| $. }")) 0)
    ("eas" "" (lambda nil (interactive) (snippet-insert "each_slice($${2}) { |$${group}| $. }")) 0)
    ("eav" "" (lambda nil (interactive) (snippet-insert "each_value { |$${val}| $. }")) 0)
    ("eawi" "" (lambda nil (interactive) (snippet-insert "each_with_index { |$${e}, $${i}| $. }")) 0)
    ("elsif" "" (lambda nil (interactive) (snippet-insert "elsif $${condition}
$>$.")) 0)
    ("enum" "" (lambda nil (interactive) (snippet-insert "include Enumerable

def each(&block)
$>$.
end$>")) 0)
    ("fet" "" (lambda nil (interactive) (snippet-insert "fetch($${name}) { |$${key}|$. }")) 0)
    ("fil" "" (lambda nil (interactive) (snippet-insert "fill($${range}) { |$${i}|$. }")) 0)
    ("file" "" (lambda nil (interactive) (snippet-insert "File.foreach($${file}) { |$${line}| $. }")) 0)
    ("fin" "" (lambda nil (interactive) (snippet-insert "find { |$${e}| $. }")) 0)
    ("fina" "" (lambda nil (interactive) (snippet-insert "find_all { |$${e}| $. }")) 0)
    ("fl" "" (lambda nil (interactive) (snippet-insert "flunk('$${message}')")) 0)
    ("flao" "" (lambda nil (interactive) (snippet-insert "inject(Array.new) { |$${arr}, $${a}| $${arr}.push(*$${a}) }")) 0)
    ("flash" "" (lambda nil (interactive) (snippet-insert "flash[:$${notice}] = '$${Successfully}'$.")) 0)
    ("forin" "" (lambda nil (interactive) (snippet-insert "for $${element} in $${collection}
$>$${element}.$.
end$>")) 0)
    ("forw" "" (lambda nil (interactive) (snippet-insert "extend Forwardable")) 0)
    ("gre" "" (lambda nil (interactive) (snippet-insert "grep($${pattern}) { |$${match}| $. }")) 0)
    ("gsu" "" (lambda nil (interactive) (snippet-insert "gsub(/$${pattern}/) { |$${match}|$. }")) 0)
    ("ha" "" (lambda nil (interactive) (snippet-insert "{ $>:$. }")) 0)
    ("hash" "" (lambda nil (interactive) (snippet-insert "Hash.new { |$${hash}, $${key}| $${hash}[$${key}] = $. }")) 0)
    ("inj" "" (lambda nil (interactive) (snippet-insert "inject($${init}) { |$${mem}, $${var}| $. }")) 0)
    ("inject" "" (lambda nil (interactive) (snippet-insert "inject($${object}) { |$${injection}, $${element}| $${4} }$.")) 0)
    ("lam" "" (lambda nil (interactive) (snippet-insert "lambda { |$${args}|$. }")) 0)
    ("logd" "" (lambda nil (interactive) (snippet-insert "logger.debug '$${message}'$.")) 0)
    ("loge" "" (lambda nil (interactive) (snippet-insert "logger.error '$${message}'$.")) 0)
    ("logf" "" (lambda nil (interactive) (snippet-insert "logger.fatal '$${message}'$.")) 0)
    ("logi" "" (lambda nil (interactive) (snippet-insert "logger.info '$${message}'$.")) 0)
    ("logw" "" (lambda nil (interactive) (snippet-insert "logger.warn '$${message}'$.")) 0)
    ("map" "" (lambda nil (interactive) (snippet-insert "map { |$${e}| $. }")) 0)
    ("mapwi" "" (lambda nil (interactive) (snippet-insert "enum_with_index.map { |$${e}, $${i}| $. }")) 0)
    ("max" "" (lambda nil (interactive) (snippet-insert "max { |a, b| $. }")) 0)
    ("md" "" (lambda nil (interactive) (snippet-insert "File.open($${dump}, \"w\") { |$${file}| Marshal.dump($${obj}, $${file}) }")) 0)
    ("min" "" (lambda nil (interactive) (snippet-insert "min { |a, b| $. }")) 0)
    ("ml" "" (lambda nil (interactive) (snippet-insert "File.open($${dump}) { |$${file}| Marshal.load($${file}) }")) 0)
    ("mm" "" (lambda nil (interactive) (snippet-insert "def method_missing(meth, *args, &block)
$>$.
end$>")) 0)
    ("modf" "" (lambda nil (interactive) (snippet-insert "module $${ModuleName}
$>module ClassMethods
$>$>$.
end$>
$>
$>extend ClassMethods
$>
$>def self.included(receiver)
$>$>receiver.extend(ClassMethods)
end$>
$>
$>
end$>")) 0)
    ("mods" "" (lambda nil (interactive) (snippet-insert "module $${ModuleName}
$>$.
end$>")) 0)
    ("modu" "" (lambda nil (interactive) (snippet-insert "module $${ModuleName}
$>module_function
$>
$>$.
end$>")) 0)
    ("nr" "" (lambda nil (interactive) (snippet-insert "@$${item}.new_record?")) 0)
    ("ope" "" (lambda nil (interactive) (snippet-insert "open($${pipe}) { |$${io}| $. }")) 0)
    ("par" "" (lambda nil (interactive) (snippet-insert "params[:$${id}]")) 0)
    ("patfh" "" (lambda nil (interactive) (snippet-insert "File.join(File.dirname(__FILE__), *%w[$${here}])")) 0)
    ("pn" "" (lambda nil (interactive) (snippet-insert "PStore.new($${file_name})")) 0)
    ("r" "" (lambda nil (interactive) (snippet-insert "attr_reader :$${attr_names}")) 0)
    ("ran" "" (lambda nil (interactive) (snippet-insert "sort_by { rand }")) 0)
    ("rb" "" (lambda nil (interactive) (snippet-insert "#!/usr/bin/env ruby -w

")) 0)
    ("rdl" "" (lambda nil (interactive) (snippet-insert "RAILS_DEFAULT_LOGGER.debug '$${message}'$.")) 0)
    ("rej" "" (lambda nil (interactive) (snippet-insert "reject { |$${e}| $. }")) 0)
    ("reject" "" (lambda nil (interactive) (snippet-insert "reject { |$${element}| $${element}.$. }")) 0)
    ("rep" "" (lambda nil (interactive) (snippet-insert "results.report(\"$${name}:\") { TESTS.times { $. } }")) 0)
    ("req" "" (lambda nil (interactive) (snippet-insert "require \"$.\"")) 0)
    ("reve" "" (lambda nil (interactive) (snippet-insert "reverse_each { |$${e}| $. }")) 0)
    ("rw" "" (lambda nil (interactive) (snippet-insert "attr_accessor :$${attr_names}")) 0)
    ("sca" "" (lambda nil (interactive) (snippet-insert "scan(/$${pattern}/) { |$${match}| $. }")) 0)
    ("sel" "" (lambda nil (interactive) (snippet-insert "select { |$${e}| $. }")) 0)
    ("select" "" (lambda nil (interactive) (snippet-insert "select { |$${element}| $${element}.$${2} }$.")) 0)
    ("session" "" (lambda nil (interactive) (snippet-insert "session[:$${User}]")) 0)
    ("sin" "" (lambda nil (interactive) (snippet-insert "class << self; self end")) 0)
    ("sor" "" (lambda nil (interactive) (snippet-insert "sort { |a, b| $. }")) 0)
    ("sorb" "" (lambda nil (interactive) (snippet-insert "sort_by { |$${e}| $. }")) 0)
    ("ste" "" (lambda nil (interactive) (snippet-insert "step($${2}) { |$${n}|$. }")) 0)
    ("sub" "" (lambda nil (interactive) (snippet-insert "sub(/$${pattern}/) { |$${match}|$. }")) 0)
    ("tc" "" (lambda nil (interactive) (snippet-insert "require \"test/unit\"

require \"$${library_file_name}\"

class Test$${amp} < Test::Unit::TestCase
$>def test_$${case_name}
$>$>$.
end$>
end$>")) 0)
    ("tim" "" (lambda nil (interactive) (snippet-insert "times { |$${n}|$. }")) 0)
    ("tra" "" (lambda nil (interactive) (snippet-insert "transaction$${1} { $. }")) 0)
    ("ts" "" (lambda nil (interactive) (snippet-insert "require \"test/unit\"

require \"tc_$${test_case_file}\"
require \"tc_$${test_case_file}\"
")) 0)
    ("until" "" (lambda nil (interactive) (snippet-insert "until $${condition}
$>$.
end$>")) 0)
    ("upt" "" (lambda nil (interactive) (snippet-insert "upto($${0}) { |$${n}|$. }")) 0)
    ("verred" "" (lambda nil (interactive) (snippet-insert "verify :only => [:$${1}], :session => :user, :params => :id, :redirect_to => {:action => '$${index}'}
")) 0)
    ("verren" "" (lambda nil (interactive) (snippet-insert "verify :only => [:$${1}], :method => :post, :render => {:status => 500, :text => \"use HTTP-POST\"}
")) 0)
    ("w" "" (lambda nil (interactive) (snippet-insert "attr_writer :$${attr_names}")) 0)
    ("when" "" (lambda nil (interactive) (snippet-insert "when $${condition}
$>$.")) 0)
    ("while" "" (lambda nil (interactive) (snippet-insert "while $${condition}
$>$.
end$>")) 0)
    ("y" "" (lambda nil (interactive) (snippet-insert " :yields: $${arguments}")) 0)
    ("yd" "" (lambda nil (interactive) (snippet-insert "File.open($${yaml}, \"w\") { |$${file}| YAML.dump($${obj}, $${file}) }")) 0)
    ("yl" "" (lambda nil (interactive) (snippet-insert "File.open($${yaml}) { |$${file}| YAML.load($${file}) }")) 0)
    ("zip" "" (lambda nil (interactive) (snippet-insert "zip($${enums}) { |$${row}| $. }")) 0)
    ))

(define-abbrev-table 'select-tags-table-mode-abbrev-table '(
    ))

(define-abbrev-table 'sgml-mode-abbrev-table '(
    ))

(define-abbrev-table 'sh-mode-abbrev-table '(
    ))

(define-abbrev-table 'shell-mode-abbrev-table '(
    ))

(define-abbrev-table 'simple-hatena-mode-abbrev-table '(
    ))

(define-abbrev-table 'snippet-mode-abbrev-table '(
    ))

(define-abbrev-table 'sql-mode-abbrev-table '(
    ))

(define-abbrev-table 'text-mode-abbrev-table '(
    ))

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '(
    ))

