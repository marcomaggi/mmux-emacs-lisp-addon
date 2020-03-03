;; mmux-emacs-lisp-addon-lists-of-symbols.el --- definition of lists of symbols for the Emacs Lisp language

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb 29, 2020
;; Time-stamp: <2020-03-03 05:10:49 marco>
;; Keywords: convenience, data, languages

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


;;; Change Log:

;;

;;; Code:


;;;; list of identifiers: constants

(defconst mmux-emacs-lisp-addon-built-in-constants-list
  '("t" "nil")
  "List of constants for the Emacs Lisp language.")

(defconst mmux-emacs-lisp-addon-custom-constants-list
  '("--func--")
  "List of custom constants for the Emacs Lisp language.

These constants are defined by the MMUX Emacs packages.")


;;;; list of identifiers: functions

(defconst mmux-emacs-lisp-addon-functions-list
  '(
    ;; numbers
    "mmec-add"
    "mmec-sub"
    "mmec-mul"
    "mmec-div"
    "mmec-mod"
    "mmec-expt"
    "mmec-exp"
    "mmec-sqrt"
    "mmec-cbrt"
    "mmec-root"
    "mmec-hypot"
    "mmec-log"
    "mmec-log2"
    "mmec-log10"
    "mmec-logb"
    "mmec-sin"
    "mmec-cos"
    "mmec-tan"
    "mmec-asin"
    "mmec-acos"
    "mmec-atan"
    "mmec-atan2"
    "mmec-sinh"
    "mmec-cosh"
    "mmec-tanh"
    "mmec-asinh"
    "mmec-acosh"
    "mmec-atanh"

    ;; bytevectors
    "mmec-bytevector-p"
    "mmec-integer-bytevector-p"
    "mmec-signed-integer-bytevector-p"
    "mmec-unsigned-integer-bytevector-p"
    "mmec-floating-point-bytevector-p"
    ;;
    "mmec-char-bytevector-p"
    "mmec-schar-bytevector-p"
    "mmec-uchar-bytevector-p"
    "mmec-wchar-bytevector-p"
    "mmec-sshrt-bytevector-p"
    "mmec-ushrt-bytevector-p"
    "mmec-sint-bytevector-p"
    "mmec-uint-bytevector-p"
    "mmec-slong-bytevector-p"
    "mmec-ulong-bytevector-p"
    "mmec-sllong-bytevector-p"
    "mmec-ullong-bytevector-p"
    "mmec-ssize-bytevector-p"
    "mmec-usize-bytevector-p"
    "mmec-sintmax-bytevector-p"
    "mmec-uintmax-bytevector-p"
    "mmec-ptrdiff-bytevector-p"
    "mmec-sint8-bytevector-p"
    "mmec-uint8-bytevector-p"
    "mmec-sint16-bytevector-p"
    "mmec-uint16-bytevector-p"
    "mmec-sint32-bytevector-p"
    "mmec-uint32-bytevector-p"
    "mmec-sint64-bytevector-p"
    "mmec-uint64-bytevector-p"
    "mmec-float-bytevector-p"
    "mmec-double-bytevector-p"
    "mmec-ldouble-bytevector-p"
    ;;
    "mmec-bytevector-number-of-slots"
    "mmec-bytevector-slot-size"
    "mmec-bytevector-signed-p"
    "mmec-bytevector-obj"
    ;;
    "mmec-bytevector-ref"
    "mmec-bytevector-set"
    "mmec-bytevector-from-list"
    "mmec-bytevector-to-list"
    "mmec-bytevector-from-vector"
    "mmec-bytevector-to-vector"
    "mmec-bytevector-compare"
    "mmec-bytevector-equal"
    "mmec-bytevector-less"
    "mmec-bytevector-greater"
    "mmec-bytevector-leq"
    "mmec-bytevector-geq"
    "mmec-subbytevector"
    )
  "List of function names for the Emacs Lisp language.

These constants are defined by the MMUX Emacs packages.")


;;;; list of identifiers: object types

(defconst mmux-emacs-lisp-addon-object-types-list
  '(
    ;; numbers
    "mmec-number"
    "mmec-integer"
    "mmec-signed-integer"
    "mmec-unsigned-integer"
    "mmec-floating-point"
    "mmec-char"
    "mmec-schar"
    "mmec-uchar"
    "mmec-wchar"
    "mmec-sshrt"
    "mmec-ushrt"
    "mmec-sint"
    "mmec-uint"
    "mmec-slong"
    "mmec-ulong"
    "mmec-sllong"
    "mmec-ullong"
    "mmec-ssize"
    "mmec-usize"
    "mmec-sintmax"
    "mmec-uintmax"
    "mmec-ptrdiff"
    "mmec-sint8"
    "mmec-uint8"
    "mmec-sint16"
    "mmec-uint16"
    "mmec-sint32"
    "mmec-uint32"
    "mmec-sint64"
    "mmec-uint64"
    "mmec-float"
    "mmec-double"
    "mmec-ldouble"

    ;; bytevectors
    "mmec-bytevector"
    "mmec-integer-bytevector"
    "mmec-signed-integer-bytevector"
    "mmec-unsigned-integer-bytevector"
    "mmec-floating-point-bytevector"
    ;;
    "mmec-char-bytevector"
    "mmec-schar-bytevector"
    "mmec-uchar-bytevector"
    "mmec-wchar-bytevector"
    "mmec-sshrt-bytevector"
    "mmec-ushrt-bytevector"
    "mmec-sint-bytevector"
    "mmec-uint-bytevector"
    "mmec-slong-bytevector"
    "mmec-ulong-bytevector"
    "mmec-sllong-bytevector"
    "mmec-ullong-bytevector"
    "mmec-ssize-bytevector"
    "mmec-usize-bytevector"
    "mmec-sintmax-bytevector"
    "mmec-uintmax-bytevector"
    "mmec-ptrdiff-bytevector"
    "mmec-sint8-bytevector"
    "mmec-uint8-bytevector"
    "mmec-sint16-bytevector"
    "mmec-uint16-bytevector"
    "mmec-sint32-bytevector"
    "mmec-uint32-bytevector"
    "mmec-sint64-bytevector"
    "mmec-uint64-bytevector"
    "mmec-float-bytevector"
    "mmec-double-bytevector"
    "mmec-ldouble-bytevector"
    )
  "List of object type names for the Emacs Lisp language.

These types are defined by the MMUX Emacs packages.")


;;;; list of identifiers: error symbols

(defconst mmux-emacs-lisp-addon-error-symbols-list
  '(
    "mmec-error"
    ;;
    "mmec-error-unsupported-feature"
    "mmec-error-unimplemented-c-language-function"
    ;;
    "mmec-error-constructor"
    "mmec-error-no-memory"
    "mmec-error-instantiating-abstract-type"
    "mmec-error-unsupported-init-type"
    ;;
    "mmec-error-bytevector-constructor"
    "mmec-error-bytevector-constructor-invalid-number-of-slots"
    "mmec-error-bytevector-constructor-invalid-slot-size"
    "mmec-error-bytevector-constructor-size-too-big"
    ;;
    "mmec-error-value-out-of-range"
    "mmec-error-index-out-of-range"
    ;;
    "mmec-error-bytevector-is-empty"
    "mmec-error-bytevector-index-out-of-range"
    "mmec-error-bytevector-span-start-out-of-range"
    "mmec-error-bytevector-span-past-out-of-range"
    "mmec-error-bytevector-invalid-span-limits"
    ;;
    "mmec-error-signed/unsigned-integer-comparison"
    "mmec-error-overflow"
    "mmec-error-underflow"
    )
  "List of error symbols for the Emacs Lisp language.

These errors are defined by the MMUX Emacs packages.")


;;;; done

(provide 'mmux-emacs-lisp-addon-lists-of-symbols)
;;; mmux-emacs-lisp-addon-lists-of-symbols.el ends here
