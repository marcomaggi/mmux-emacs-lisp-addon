;;; mmux-emacs-lisp-mode-addon.el --- editing additions for Emacs Lisp mode

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb 29, 2020
;; Time-stamp: <2020-03-01 10:48:34 marco>
;; Keywords: languages

;; This file is part of MMUX Emacs Lisp Addon.
;;
;; This program is  free software: you can redistribute  it and/or modify it under the  terms of the
;; GNU General Public License as published by the  Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied  warranty of MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.
;;
;; You should have  received a copy of the  GNU General Public License along with  this program.  If
;; not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:

;;

;;; Change Log:

;;

;;; Code:

(require 'mmux-emacs-lisp-addon-font-lock)


;;;; imenu customisation

(defconst mmux-emacs-lisp-addon-imenu-generic-expression
  `(
;;; functions, generic functions, methods, variables

    ;; (mmux-emacs-lisp-addon-defmethod ?fun (?args) . ?body)
    ;; (mmux-defun ?fun (?args) . ?body)
    (nil
     ,(eval-when-compile
	(concat "^\\s-*(\\s-*"
		(regexp-opt '("mmec-defmethod"
			      "mmec-defun")
			    'symbols)
		"\\(" mmux-emacs-lisp-addon-identifiers-rex "\\)"))
     2)
    )
  "Customise imenu for Emacs Lisp editing using MMUX Emacs packages.

For details on how to use it see `imenu-generic-expression'.")

(defun mmux-emacs-lisp-addon-setup-imenu ()
  "Setup imenu for MMEC mode."
  (interactive)
  (setq imenu-generic-expression (append imenu-generic-expression
					 mmux-emacs-lisp-addon-imenu-generic-expression)))


;;;; custom indentation

(defconst mmux-emacs-lisp-addon-mmux-emacs-core
  '(
    ;;These are defined here even though their indentation specification is also defined in the code
    ;;with:
    ;;
    ;;   (declare (indent defun))
    ;;
    ;;such DECLARE is executed only if the  macro definitions are evaluated, which normally does not
    ;;happen if we do not load the MMUX Emacs Core package.
    (mmec-defmethod			. 2)
    (mmec-defun				. 2)
    )
  "List of indentation rules for MMUX Emacs forms.")

(defun mmux-emacs-lisp-addon-setup-indentation ()
  "Setup custom indentation for Emacs Lisp mode using MMUX Emacs packages."
  (interactive)
  (dolist (list-of-indent-specs (list mmux-emacs-lisp-addon-mmux-emacs-core
				      ))
    (dolist (indent-spec list-of-indent-specs)
      (put (car indent-spec) 'lisp-indent-function (cdr indent-spec)))))


;;;; done

;;;###autoload
(defun mmux-emacs-lisp-addon ()
  "Setup editing addons for Emacs Lisp related to the MMUX Emacs packages."
  (mmux-emacs-lisp-addon-setup-imenu)
  (mmux-emacs-lisp-addon-setup-indentation))

(provide 'mmux-emacs-lisp-addon)
;;; mmec.el ends here
