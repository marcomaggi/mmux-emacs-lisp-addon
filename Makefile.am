## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I meta/autotools
AUTOMAKE_OPTIONS	= foreign
EXTRA_DIST		= INSTALL
dist_doc_DATA		= README COPYING
CLEANFILES		=
MAINTAINERCLEANFILES	=

## --------------------------------------------------------------------
## documentation

AM_MAKEINFOFLAGS		= --no-split
info_TEXINFOS			= doc/mmux-emacs-lisp-addon.texi
doc_chicken_mode_TEXINFOS	= doc/fdl-1.3.texi doc/gpl-3.0.texi doc/macros.texi

## --------------------------------------------------------------------

MY_EMACS_DIRS		= \
	--directory $(srcdir)/src			\
	--directory $(builddir)/src			\
	--directory /usr/share/emacs/site-lisp		\
	--directory /usr/local/share/emacs/site-lisp

# This  variable is  reserved to  the  package author.   Users of  the
# package can set ELCFLAGS.
AM_ELCFLAGS	= --no-init-file $(MY_EMACS_DIRS)

sharelispdir		= $(datadir)/emacs/site-lisp
dist_sharelisp_LISP	= \
	src/mmux-emacs-lisp-addon.el			\
	src/mmux-emacs-lisp-addon-font-lock.el		\
	src/mmux-emacs-lisp-addon-lists-of-symbols.el

src/mmux-emacs-lisp-addon-font-lock.elc: src/mmux-emacs-lisp-addon-lists-of-symbols.el

src/mmux-emacs-lisp-addon.elc: src/mmux-emacs-lisp-addon-font-lock.elc

### end of file
