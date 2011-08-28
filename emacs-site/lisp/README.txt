-*- mode: org; mode: mode: smart-chars; -*-

Various Emacs settings and macros reside here for your perusal.
These sources essentially consist of everything I would place within my ~/.emacs
were I to only use one file.

Much of this elisp source is entirely my own creation, but some small code
bits have been shamelessly copied from the examples of others (without violating
licensing terms). Individuals from whom I have borrowed examples extensively in
the past, and merit recognition, include:

| Trent Buck | http://twb.ath.cx/~twb/.emacs/ | `twb' |

It should be noted that all of these sources use the ‘provide’ and ‘require’
functions, rather than load or load-library, to automatically load other
sources. This is to prevent multiple loads of the same file, and also
considerably improves Emacs initialization speed.

The Emacs versions that I am currently using with this source:
    GNU Emacs 23.1 cvs
    GNU Emacs 23.2 release

Unofficial packages and/or installed out-of-tree via bleeding-edge CVS code.
These packages are used and possibly referenced by the provided lisp source:

| auctex        | http://www.gnu.org/software/auctex/                                      |
| cfengine      | http://www.iu.hio.no/cfengine/                                           |
| clisp         | http://clisp.sourceforge.net/                                            |
| css-mode      | http://www.garshol.priv.no/download/software/css-mode/                   |
| dictionary    | http://www.myrkr.in-berlin.de/dictionary/index.html                      |
| easypg        | http://www.easypg.org/                                                   |
| ebuild-mode   | http://bugs.gentoo.org/                                                  |
| emacs-w3m     | http://emacs-w3m.namazu.org                                              |
| emms          | http://www.gnu.org/software/emms/                                        |
| gforth        | http://www.gnu.org/software/gforth                                       |
| gnus          | http://gnus.org/                                                         |
| gnuserv       | http://meltin.net/hacks/emacs/                                           |
| ledger        | http://www.newartisans.com/ledger.html                                   |
| lua-mode      | http://lua-users.org/wiki/LuaEditorSupport                               |
| matlab        | http://www.mathworks.com/products/matlab/                                |
| maxima        | http://maxima.sourceforge.net/                                           |
| muse          | http://www.mwolson.org/projects/MuseMode.html                            |
| nxml-mode     | http://www.emacswiki.org/cgi-bin/wiki/NxmlMode                           |
| octave        | http://www.octave.org/                                                   |
| org           | http://staff.science.uva.nl/~dominik/Tools/org/                          |
| php-mode      | http://php-mode.sourceforge.net                                          |
| post.el       | http://www.drao-ofr.hia-iha.nrc-cnrc.gc.ca/~rreid/software/mutt/         |
| preview-latex | http://preview-latex.sourceforge.net/                                    |
| prolog.el     | http://turing.ubishops.ca/home/bruda/emacs-prolog/                       |
| psvn          | http://subversion.tigris.org/                                            |
| reftex        | http://www.gnu.org/software/auctex/reftex.html                           |
| ruby-mode     | http://www.ruby-lang.org/                                                |
| txutils.el    | (search for it on Google)                                                |
| verilog-mode  | http://www.verilog.com/emacs_install.html                                |
| vlog-mode     | http://vlog-mode.sourceforge.net/                                        |
| xmlunicode    | http://nwalsh.com/emacs/xmlchars/                                        |


-effecting
