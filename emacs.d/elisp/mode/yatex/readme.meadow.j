 Meadow(Mule for Windows) への 野鳥のインストール
*************************************************************

野鳥のインストールは、以下の3ステップで完了します。

(1) 初期設定ファイル(~/.emacs)の設定
(2) Emacs Lispファイル(*.el)のコピー
(3) 取扱説明書(help/info)の設定

順に説明します。


(1) 初期設定ファイル(~/.emacs)の設定

Emacsは、ユーザそれぞれの初期設定をホームディレクトリ(~/)の中にある
.emacsというファイル(あるいは.emacs.elというファイル)から読み込みます。

ここでは、拡張子が.texであるファイルを読み込むと、自動的に野鳥が読み込
まれるように設定します。

まず ~/.emacsに下の2項目を加えます。

	(setq auto-mode-alist
	      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
	(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

次に野鳥の emacs-lisp ファイル群を置くディレクトリを load-path に加えます。
たとえば、`~/src/emacs/yatex'に置くのであれば、

	(setq load-path (cons "~/src/emacs/yatex" load-path))

などとします。もし、pLaTeX2e, dviout などのコマンドをインストールした場
所にPATHを通していないときはこれらを明示的に指定します。以下の例は
c:\ptex\bin にLaTeX関係のコマンドをインストールしてある場合の指定です。

	(setq tex-command "c:/ptex/bin/platex")
	(setq dvi2-command "c:/ptex/bin/dviout")

これで、初期設定ファイルの設定は終了です。



(2) Emacs Lispファイル(*.el)のコピー

Emacs Lispで書かれた野鳥本体をインストールするには、(i) makeを使う方法
と、(ii) 手動でコピーする方法とがあります。


(i) makeを使う方法

Windows環境に Unix ライクなコマンドを提供する「Cygwin」を導入したりし
て、makeが使える環境にある方は、次のような方法でインストールできます。
そうでない方は(ii)に進んでください。

まず、Emacsなどのエディタでこのファイルと同じディレクトリにある
makefileというファイルを開き、自分の環境にあわせて編集します。

たとえば、

PREFIX	= /usr/localを、PREFIX = /usrにしたり、

## mule2
EMACS	= mule
EMACSDIR= ${PREFIX}/lib/${EMACS}
## emacs20
#EMACS	= emacs
#EMACSDIR= ${PREFIX}/share/${EMACS}
## XEmacs
#EMACS	= xemacs
#EMACSDIR= ${PREFIX}/lib/${EMACS}

を自分の使っているEmacsの種類にあわせたりします。行頭に#をつけるとコメ
ント扱いとなり、その行は無効化されます。その行を有効にするためには、行
頭の#を外します。

また、次の行の行頭に#をつけてください。

GEO	= -geometry 80x20+0+0


以下は書き換えた部分の例です

PREFIX=/usr/local/meadow
EMACS=meadow
EMACSDIR=${PREFIX}/site-lisp
#GEO	= -geometry 80x20+0+0

以上の作業が終わったら、makefileを保存し、bashなどのシェルで、

	% export DISPLAY=":0.0"
	% make install 

とすれば、インストールされます。

(3)の取扱説明書のインストールも基本的には終わっていますが、Emacsから読
めるようにするために、info ディレクトリの dir というファイルにこのディ
レクトリの dir ファイルの中身を追加してください。



(ii) 手動でコピーする方法

Windows環境などでmakeがなかったり、上記(i)のmake installが失敗する場合
は、手動で必要ファイルをインストール(コピー)することができます。

たとえば、Meadow 用の外部Emacs-Lispパッケージをインストールするディレク
トリが /usr/local/meadow/site-lisp/ だったとします。その場合、
このアーカイブ(yatex<VERSION>.tar.gz)を展開したディレクトリ
(yatex<VERSION>) をそこに移動します。すると、
/usr/local/meadow/site-lisp/yatex<VERSION>/ というディレクト
リになりますので、これをバージョン番号無しの
/usr/local/meadow/site-lisp/yatex/
にします。

これで野鳥は使えるようになっているはずですが、さらに実行速度をわずかばか
り稼ぐため、Emacs Lispファイルをコンパイル(バイトコンパイル)することもで
きます。ただし、バイトコンパイルする手順が決まっているので手動でやるのは
Emacsについての知識が必要です。さらにバイトコンパイルしても実感できるほ
ど速くはならないのであまりお勧めしません。それでもバイトコンパイルしたい
場合は、Cygwinなど、makeの使える環境を用意して、それで自動的に処理する方
が良いと思います。

(3) 取扱説明書(help/info)の設定

次に、各種の説明書をインストールします。

LaTeXマクロの利用解説書である help/YATEXHLP.jp ファイルを
/usr/local/meadow/site-lisp/
にコピーします。

野鳥自身のマニュアルである docs/yatexj, docs/yatexe と
yahtml のマニュアルである docs/yahtmlj, docs/yahtmle を
/usr/local/meadow/1.10/info/
にコピーし、info ディレクトリの dir というファイルに
このディレクトリの dir ファイルの中身を追加します。

以上でインストールは完了です。
