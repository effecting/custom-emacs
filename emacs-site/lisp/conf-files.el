;; 
;; 

;; Authored by effecting2 in 2006-2007.
;; This is free software distributed under the GPL Version 2 or newer.
;; It has no warranty. See the GNU General Public License for more
;; information.

;; This code is not part of GNU Emacs.

;; mode autoloads
(mapc (lambda (p)
        (let ( (a (nth 0 p)) (b (nth 1 p)) (c (nth 2 p)) (d (nth 3 p)) )
          ;; Only do anything if library is present. I do not want
          ;; to override a standard mode with a non-standard mode
          ;; if the non-standard mode is absent.
          (when (locate-library a)
            ;; library to load for a particular mode
            (if (not (null a))
                (autoload b a nil t) )
            ;; file extensions to indicate default mode
            (if (not (null c))
                (add-to-list 'auto-mode-alist (cons c  b)))
            ;; interpreter identifiers
            (while d
              (add-to-list 'interpreter-mode-alist (cons (car d) b))
              (setq d (cdr d)) )
            )
          ))
      ;;  LIBRARY        MODE           FILE-EXTENSION-REGEX           INTERPRETER-MODES
      '(
        ( "cperl-mode"      cperl-mode       "\\.\\([pP][Llm]\\|al\\)\\'"   ("perl" "perl5" "miniperl"))
        ( "clisp-mode"      clisp-mode       "\\.c?lisp\\'"                 ("clisp"))
        ( "set-tex"         latex-mode       nil                            nil)
        ( "set-tex"         LaTeX-mode       "\\.tex\\'"                    nil)
        ( "bibtex"          bibtex-mode      "\\.bib\\'"                    nil)
        ( "gentoo-syntax"   ebuild-mode      "\\.\\(ebuild\\|eclass\\)\\'"  nil)
        ( "gentoo-syntax"   eselect-mode     "\\.eselect\\'"                nil)
        ( "yatex"           yatex-mode       nil                            nil)
        ( "python"          python-mode      "\\.py\\'"                     ("python"))
        ( "lua-mode"        lua-mode         "\\.lua\\'"                    ("lua"))
        ( "prolog"          prolog-mode      "\\.pro\\'"                    ("gprolog" "swi-prolog"))
        ( "php-mode"        php-mode         "\\.\\(php\\|phtml\\)\\'"      ("php"))
        ( "gforth"          forth-mode       "\\.fs\\'"                     ("gforth"))
        ( "css-mode"        css-mode         "\\.css\\'"                    nil)
        ( "matlab"          matlab-mode      "\\.m\\'"                      nil)
        ( "muttrc-mode"     muttrc-mode      "\\.muttrc"                    nil)
        ( "cfengine"        cfengine-mode    "^.\\{10\\}cf\\..\\{3,\\}\\'"  nil)
        ( "maxima"          maxima-mode      "\\.max\\'"                    nil)
        ( "emaxima"         emaxima-mode     nil                            nil)
        ( "vhdl-mode"       vhdl-mode        "\\.vhdl?\\'"                  nil)
        ( "vlog-mode"       vlog-mode        "\\.v[hl]?\\'"                 nil)
        ( "verilog-mode"    verilog-mode     nil                            nil)
        ( "org"             org-mode         "\\.org\\'"                    nil)
        ( "post"            post-mode        "mutt-\\w*-[0-9]*-[0-9]*-[0-9.]*\\'" nil)
        ( "ruby-mode"       ruby-mode        "\\.ruby\\'"                   ("ruby"))
        ( "nxml-mode"       nxml-mode        "\\.\\(xslt\\|[xX][mMsS][lL]\\|xmap\\|xhtml\\)\\'" nil)
        ( "muse-autoloads"  muse-mode        "\\.muse\\'" nil)
        ( "nb-mode"         nb-mode          "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9T]\\{5\\}_[0-9]\\{2\\}_[0-9]\\{2\\}\\.txt\\'" nil)
        ( "ledger-indent"   ledger-mode      nil                            nil)
        ( "wv-mode"         wv-mode          "\\.doc[0-9]+[A-Za-z0-9_]\\{3\\}\\.txt\\'" nil)
        ( "scilab"          scilab-mode      "\\.sc[ei]\\'"                 nil)
        ( "csharp-mode"     csharp-mode      "\\.cs\\'"                     nil)
        ))

; gentoo-syntax
; (add-to-list 'interpreter-mode-alist '("runscript" . sh-mode))

(provide 'conf-files)
