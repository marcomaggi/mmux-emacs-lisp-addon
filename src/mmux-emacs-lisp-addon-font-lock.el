;;; mmux-emacs-lisp-addon-font-lock.el --- custom font locking for Emacs Lisp mode

;;Copyright (C) 2020 Marco Maggi <mrc.mgg@gmail.com>

;;Author: Marco Maggi <mrc.mgg@gmail.com>
;;Maintainer: Marco Maggi <mrc.mgg@gmail.com>
;;Created: Mar  1, 2020
;;Keywords: languages, lisp, faces, matching
;;Version: 1.0

;;This file is part of MMUX Emacs Lisp Addon.

;;This program is free software: you can redistribute it and/or modify it under the terms of the GNU
;;General Public  License as  published by  the Free Software  Foundation, either  version 3  of the
;;License, or (at your option) any later version.

;;This program is distributed in the hope that  it will be useful, but WITHOUT ANY WARRANTY; without
;;even the  implied warranty of MERCHANTABILITY  or FITNESS FOR  A PARTICULAR PURPOSE.  See  the GNU
;;General Public License for more details.

;;You should  have received a copy  of the GNU General  Public License along with  this program.  If
;;not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;This file should be loaded with:
;;
;;  (require 'mmux-emacs-lisp-addon-font-lock)
;;
;;upon loading: the font-locking for Emacs Lisp mode is configured at the top-level.

;;; Change Log:

;;; Code:

(eval-when-compile
  (require 'mmux-emacs-lisp-addon-lists-of-symbols))


;;; Documentation:

;;Introduction
;;------------
;;
;;To understand what is going  on read the Emacs guide (node "Font Lock")  and the Elisp guide (node
;;"Font Lock Mode").  Here we give a brief description, cutting out the alternatives and focusing on
;;the choices made for this customisation.
;;
;;There are a plethora of font locking related  variables; here we have to understand the two buffer
;;local  variables `font-lock-defaults'  and `font-lock-keywords',  and how  they interact  with the
;;`font-lock-add-keywords' function.
;;
;;Inspection
;;----------
;;
;;At  any  instant  we  can  inspect  the  list  of faces  defined  in  a  buffer  by  issuing  `M-x
;;list-faces-display': it will open a buffer with a table of faces and examples.
;;
;;To  try  the  colour  configuration  of  a  face:  open  a  buffer  with  the  mode  on,  do  `M-x
;;set-face-foreground' then type the  name of the face and the name of  the colour.  The change will
;;be applied instantly to all the text marked with the selected face.
;;
;;When debugging keep an eye on the "*Messages*" buffer: if a face is not recognised there should be
;;a message there.
;;
;;To test if a string matches a regexp: in the *scratch* buffer apply `C-j' to:
;;
;;   (let ((rex "'[[:alnum:]-\\?]+")
;;         (text "'woppa-wippa?"))
;;      (list (string-match rex text)
;;            (match-end 0)))
;;
;;the result should be a list of indexes specifying the matching portion.
;;
;;For reasons currently unknown  to me: to update the font locking configuration  we have to restart
;;Emacs  and load  the updated  files anew.   This is  not required  with other  customisations, for
;;example custom indentation, but with font locking that is the way it is.
;;
;;Keywords
;;--------
;;
;;To get what we want, our target is  to put The Right Value(tm) in `font-lock-keywords'.  We SHOULD
;;NOT do it directly with:
;;
;;   (setq font-lock-keywords ...)  ;; NO!!!
;;
;;rather  we   should  store   a  statically   computed,  full   font  locking   specification  into
;;`font-lock-defaults'; then,  optionally, we  should dynamically  compute other  specifications and
;;activate them using `font-lock-add-keywords'.
;;
;;The value of  `font-lock-keywords' has two parts.  The  first is computed by Emacs  as selected in
;;"scheme.el", and  we do not  touch it.  The second  part is initialised  from `font-lock-defaults'
;;(whose original  value is  copied from  the function  `scheme-mode-variables' in  "scheme.el") and
;;optionally updated by `font-lock-add-keywords'.
;;
;;There are  3 mutually exclusive levels  of font locking:  light, medium, heavy.  By  default heavy
;;mode is  selected; we can  change this, in  a mode specific  manner, by appropriately  setting the
;;variable `font-lock-maximum-decoration'.  We do  not go into details here, suffice  it to say that
;;the high level is selected for Scheme mode by doing:
;;
;;   (setq font-lock-maximum-decoration '((scheme-mode . 3)))
;;
;;for details read the Emacs guide.
;;
;;The  value  of `font-lock-defaults'  is  a  list;  for the  full  setting  with comments  see  the
;;`mmec-setup-font-locking' function below.  Its first element can be a list of symbols, each symbol
;;being the name of a variable holding the  font locking specification for a level.  The default for
;;Scheme mode is:
;;
;;   (scheme-font-lock-keywords
;;    scheme-font-lock-keywords-1
;;    scheme-font-lock-keywords-2)
;;
;;we change it to:
;;
;;   (scheme-font-lock-keywords
;;    scheme-font-lock-keywords-1
;;    mmec-font-lock-keywords)
;;
;;and    embed   `scheme-font-lock-keywords-2'    inside    the   custom
;;specification `mmec-font-lock-keywords'.
;;
;;Notes on regular expressions
;;----------------------------
;;
;;We need regular expressions to match portions of Scheme code.  We must remember that:
;;
;;* Basically  a regular  expression should  be a  string like  "\\(...\\)", in  which the  dots are
;;  replaced by the pattern, and the quoted parentheses select a grouping pattern.
;;
;;* When a regular expression is composed by more,  alternative, patterns we join them with a quoted
;;  bar: "...\\|...".
;;
;;* The regexp  for an optional  white space  is: "\\s-*", where  "\\s-" represents any  white space
;;  character.
;;
;;* The regexp for a mandatory white space is: "\\s-+".
;;
;;* The regexp for a mandatory white space followed by an optional open parenthesis is: "\\s-+(?".
;;
;;* The  "\\<" and  "\\>" match  the empty  strings  at the  beginning and  end of  a "word".   What
;;  constitutes a "word" is defined by the Emacs syntax table active in Scheme mode.
;;
;;  We DO NOT want to change the definition of "word", because it is used, for example, with the key
;;  commands [M-f] and [M-b].
;;
;;* To  test a  set  of regexps  definitions:  write them  in  a buffer,  evaluate  the buffer  with
;;  `eval-buffer', then, in the "*scratch*" buffer, try something like:
;;
;;     (string-match mmec-identifier-rex "->ciao")
;;     (string-match mmec-identifier-rex "ciao")
;;     (string-match mmec-identifier-rex "ciao-hello")
;;     (string-match mmec-identifier-rex "ciao,hello")
;;
;;  the return value of `string-match' is the index of  the first char that matched, or nil if there
;;  is no match.
;;
;;To build  optimised regular expressions for  lists of words,  we use `regexp-opt'; notice  that it
;;automatically quotes  special characters in the  input strings and it  automatically adds grouping
;;parentheses at the beginning and end if the  second argument is non-`nil', for example `t'.  Usage
;;example:
;;
;;  (eval-when-compile
;;    (regexp-opt '("define" "define*") 'symbols))
;;
;;To build a regexp  for a list of words the  second argument should be `words'; to  match a list of
;;symbols, like the programming language keywords, the second argument should be `symbols'.
;;
;;To match at the beginning  of a Scheme form, we prepend an open  parenthesis and an optional white
;;space and append a mandatory white space:
;;
;;  (concat "(\\s-*"
;;          (eval-when-compile
;;            (regexp-opt '("define" "define*") 'symbols))
;;          "\\s-+")
;;
;;Notes on faces
;;--------------
;;
;;To define and use a  face we do 2 steps: define the customisation  item, define a variable holding
;;the name of the face.  For unfathomable reasons, we need both.
;;
;;The namespace of faces is separated from the others, so  it is not mandatory to name a face with a
;;symbol ending with `-face'.  Here we do it nevertheless.
;;
;;We define a set of  faces with `defface' forms and a set of  variables with `defconst' forms.  The
;;variables can reference both a built in Emacs face or a custom face.
;;


;;;; custom faces

;; (defface mmux-emacs-lisp-addon-built-in-constants-face
;;   `((t (:foreground "aquamarine3")))
;;   "Emacs Lisp mode custom face used for built-in constants."
;;   :group 'lisp
;;   :group 'custom-faces)

;; (defconst mmux-emacs-lisp-addon-built-in-constants-face
;;   'mmux-emacs-lisp-addon-built-in-constants-face
;;   "Emacs Lisp mode custom face used for built-in constants.")

;;; --------------------------------------------------------------------

(defface mmux-emacs-lisp-addon-custom-constants-face
  `((t (:foreground "aquamarine3")))
  "Emacs Lisp mode custom face used for custom constants."
  :group 'lisp
  :group 'custom-faces)

(defconst mmux-emacs-lisp-addon-custom-constants-face
  'mmux-emacs-lisp-addon-custom-constants-face
  "Emacs Lisp mode custom face used for custom constants.")

;;; --------------------------------------------------------------------

(defface mmux-emacs-lisp-addon-functions-face
  `((t (:foreground "spring green")))
  "Emacs Lisp mode custom face used for MMUX Emacs functions."
  :group 'lisp
  :group 'font-lock-faces)

(defconst mmux-emacs-lisp-addon-functions-face
  'mmux-emacs-lisp-addon-functions-face
  "Emacs Lisp mode custom face used for MMUX Emacs functions.")

;;; --------------------------------------------------------------------

(defface mmux-emacs-lisp-addon-error-symbols-face
  `((t (:foreground "orange")))
  "Emacs Lisp mode custom face used for MMUX Emacs error symbols."
  :group 'lisp
  :group 'font-lock-faces)

(defconst mmux-emacs-lisp-addon-error-symbols-face
  'mmux-emacs-lisp-addon-error-symbols-face
  "Emacs Lisp mode custom face used for MMUX Emacs error symbols.")


;;;; regular expressions

(defconst mmux-emacs-lisp-addon-built-in-constants-rex
  (eval-when-compile
    (regexp-opt mmux-emacs-lisp-addon-built-in-constants-list 'symbols))
  "Regexp to match built-in Emacs Lisp language constants.")

(defconst mmux-emacs-lisp-addon-custom-constants-rex
  (eval-when-compile
    (regexp-opt mmux-emacs-lisp-addon-custom-constants-list 'symbols))
  "Regexp to match custom Emacs Lisp language constants defined by MMUX Emacs packages.")

(defconst mmux-emacs-lisp-addon-functions-list-rex
  (eval-when-compile
    (regexp-opt mmux-emacs-lisp-addon-functions-list 'symbols))
  "Regexp to match known Emacs Lisp functions defined by the MMUX Emacs packages.")

(defconst mmux-emacs-lisp-addon-object-types-list-rex
  (eval-when-compile
    (regexp-opt mmux-emacs-lisp-addon-object-types-list 'symbols))
  "Regexp to match known Emacs Lisp object types defined by the MMUX Emacs packages.")

(defconst mmux-emacs-lisp-addon-error-symbols-list-rex
  (eval-when-compile
    (regexp-opt mmux-emacs-lisp-addon-error-symbols-list 'symbols))
  "Regexp to match known Emacs Lisp error symbols defined by the MMUX Emacs packages.")


;;;; main hook

;;We perform this  call to `font-lock-add-keywords' at  the top-level, so the  configuration is done
;;only once at file-loading time.
;;
(font-lock-add-keywords
    ;;This  argument  MODE is  set  to  `emacs-lisp-mode' because  this  call  is performed  at  the
    ;;top-level.  See the documentation of `font-lock-add-keywords' for details.
    'emacs-lisp-mode

  ;;Here  we need  to  remember that  "(regexp-opt  ... 'symbols)"  encloses  the generated  regular
  ;;expression between  '\_<\(' and  '\)\_>' so  the SUBEXP  number must  be 1  to match  the actual
  ;;symbol.
  ;;
  `(
    (,mmux-emacs-lisp-addon-built-in-constants-rex	1 font-lock-constant-face keep)
    (,mmux-emacs-lisp-addon-custom-constants-rex	1 mmux-emacs-lisp-addon-custom-constants-face keep)
    (,mmux-emacs-lisp-addon-functions-list-rex		1 mmux-emacs-lisp-addon-functions-face keep)
    (,mmux-emacs-lisp-addon-object-types-list-rex	1 font-lock-type-face)
    (,mmux-emacs-lisp-addon-error-symbols-list-rex	1 mmux-emacs-lisp-addon-error-symbols-face keep)

    ;; (mmec-defmethod mmec-bytevector-ref ?args . ?body)
    ;; (mmec-defun mmec-bytevector-compare ?args . ?body)
    (,(eval-when-compile
	(concat "(\\s-*"
		(regexp-opt '("mmec-defmethod"
			      "mmec-defun")
			    'symbols)
		"\\s-*[^()]*" ;match any number of qualifiers
		"\\s-+("      ;any whitespace and open paren
		"\\s-*"	      ;optional white space separator
		"\\(\\sw\\)"))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))
    )

  ;;This  true value  as HOW  argument causes  this specification  to be  appended to  the value  of
  ;;`font-lock-keywords'.
  ;;
  ;;We need it to  allow correct fontification of known function names, which  must happen after the
  ;;fontification built into `emacs-lisp-mode'.
  t)


;;;; done

(provide 'mmux-emacs-lisp-addon-font-lock)

;;; mmux-emacs-lisp-addon-font-lock.el ends here
