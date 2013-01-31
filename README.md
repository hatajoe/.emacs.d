emacs
=====

# for use
$ cd<br />
$ git clone https://github.com/Hatajoe/emacs.git .emacs.d<br />
$ mv .emacs.d/.emacs.el .<br />

show key-bind is 'hj'

# elisp install
$ cd <br />
$ mkdir .emacs.d/elisp .emacs.d/elpa .emacs.d/public_repos<br />
$ cd .emacs.d/elisp<br />
$ wget http://www.emacswiki.org/cgi-bin/wiki/download/install-elisp.el<br />
$ wget http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el?format=txt<br />
$ mv init-loader.el\@format\=txt init-loader.el<br />
$ cd ~/.emacs.d/conf<br />
$ grep -rn "require" .<br />
\# show all required elisp list<br />
\# install → http://www.google.co.jp/<br />

