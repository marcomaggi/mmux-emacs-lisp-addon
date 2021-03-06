;; mmux-emacs-lisp-addon-lists-of-symbols.el --- definition of lists of symbols for the Emacs Lisp language

;; Copyright (C) 2020 Marco Maggi

;; Author: Marco Maggi <mrc.mgg@gmail.com>
;; Created: Feb 29, 2020
;; Time-stamp: <2020-03-13 06:49:49 marco>
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

(defconst mmux-emacs-lisp-addon-custom-constants-list
  '("--func--")
  "List of custom constants for the Emacs Lisp language.

These constants are defined by the MMUX Emacs packages.")


;;;; list of identifiers: functions

(defconst mmux-emacs-lisp-addon-functions-list
  '(
    ;; number constructors
    "make-mmec-char"
    "make-mmec-schar"
    "make-mmec-uchar"
    "make-mmec-wchar"
    "make-mmec-sshrt"
    "make-mmec-ushrt"
    "make-mmec-sint"
    "make-mmec-uint"
    "make-mmec-slong"
    "make-mmec-ulong"
    "make-mmec-sllong"
    "make-mmec-ullong"
    "make-mmec-ssize"
    "make-mmec-usize"
    "make-mmec-sintmax"
    "make-mmec-uintmax"
    "make-mmec-ptrdiff"
    "make-mmec-sint8"
    "make-mmec-uint8"
    "make-mmec-sint16"
    "make-mmec-uint16"
    "make-mmec-sint32"
    "make-mmec-uint32"
    "make-mmec-sint64"
    "make-mmec-uint64"
    "make-mmec-float"
    "make-mmec-double"
    "make-mmec-ldouble"
    "copy-mmec-char"
    "copy-mmec-schar"
    "copy-mmec-uchar"
    "copy-mmec-wchar"
    "copy-mmec-sshrt"
    "copy-mmec-ushrt"
    "copy-mmec-sint"
    "copy-mmec-uint"
    "copy-mmec-slong"
    "copy-mmec-ulong"
    "copy-mmec-sllong"
    "copy-mmec-ullong"
    "copy-mmec-ssize"
    "copy-mmec-usize"
    "copy-mmec-sintmax"
    "copy-mmec-uintmax"
    "copy-mmec-ptrdiff"
    "copy-mmec-sint8"
    "copy-mmec-uint8"
    "copy-mmec-sint16"
    "copy-mmec-uint16"
    "copy-mmec-sint32"
    "copy-mmec-uint32"
    "copy-mmec-sint64"
    "copy-mmec-uint64"
    "copy-mmec-float"
    "copy-mmec-double"
    "copy-mmec-ldouble"

    ;; operations on numbers
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

    ;; number predicates
    "mmec-fits-number-type-p"
    "mmec-number-type-is-signed-p"
    "mmec-number-type-p"
    "mmec-symbol-number-type-is-signed-p"

    ;; number inspection
    "mmec-limit-min"
    "mmec-limit-max"
    "mmec-sizeof-number-type"

    ;; bytevector constructors
    "make-mmec-char-bytevector"
    "make-mmec-schar-bytevector"
    "make-mmec-uchar-bytevector"
    "make-mmec-wchar-bytevector"
    "make-mmec-sshrt-bytevector"
    "make-mmec-ushrt-bytevector"
    "make-mmec-sint-bytevector"
    "make-mmec-uint-bytevector"
    "make-mmec-slong-bytevector"
    "make-mmec-ulong-bytevector"
    "make-mmec-sllong-bytevector"
    "make-mmec-ullong-bytevector"
    "make-mmec-ssize-bytevector"
    "make-mmec-usize-bytevector"
    "make-mmec-sintmax-bytevector"
    "make-mmec-uintmax-bytevector"
    "make-mmec-ptrdiff-bytevector"
    "make-mmec-sint8-bytevector"
    "make-mmec-uint8-bytevector"
    "make-mmec-sint16-bytevector"
    "make-mmec-uint16-bytevector"
    "make-mmec-sint32-bytevector"
    "make-mmec-uint32-bytevector"
    "make-mmec-sint64-bytevector"
    "make-mmec-uint64-bytevector"
    "make-mmec-float-bytevector"
    "make-mmec-double-bytevector"
    "make-mmec-ldouble-bytevector"
    "copy-mmec-bytevector"
    "copy-mmec-char-bytevector"
    "copy-mmec-schar-bytevector"
    "copy-mmec-uchar-bytevector"
    "copy-mmec-wchar-bytevector"
    "copy-mmec-sshrt-bytevector"
    "copy-mmec-ushrt-bytevector"
    "copy-mmec-sint-bytevector"
    "copy-mmec-uint-bytevector"
    "copy-mmec-slong-bytevector"
    "copy-mmec-ulong-bytevector"
    "copy-mmec-sllong-bytevector"
    "copy-mmec-ullong-bytevector"
    "copy-mmec-ssize-bytevector"
    "copy-mmec-usize-bytevector"
    "copy-mmec-sintmax-bytevector"
    "copy-mmec-uintmax-bytevector"
    "copy-mmec-ptrdiff-bytevector"
    "copy-mmec-sint8-bytevector"
    "copy-mmec-uint8-bytevector"
    "copy-mmec-sint16-bytevector"
    "copy-mmec-uint16-bytevector"
    "copy-mmec-sint32-bytevector"
    "copy-mmec-uint32-bytevector"
    "copy-mmec-sint64-bytevector"
    "copy-mmec-uint64-bytevector"
    "copy-mmec-float-bytevector"
    "copy-mmec-double-bytevector"
    "copy-mmec-ldouble-bytevector"

    ;; operations on bytevectors
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
    "mmec-test-error"
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
    ;;
    "mmec-error-invalid-argument"
    "mmec-error-unknown-number-object-type-or-stem"
    )
  "List of error symbols for the Emacs Lisp language.

These errors are defined by the MMUX Emacs packages.")


;;;; done

(provide 'mmux-emacs-lisp-addon-lists-of-symbols)
;;; mmux-emacs-lisp-addon-lists-of-symbols.el ends here
