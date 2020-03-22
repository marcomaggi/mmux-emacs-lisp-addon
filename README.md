# MMUX Emacs Lisp Addon

## Introduction

This  package is  a distribution  of  Emacs Lisp  files implementing  an
editing mode addon for Emacs Lisp under GNU Emacs.  The addon implements
special support for the facilities of the MMUX Emacs packages.

The library targets  POSIX systems.  This package is meant  to work with
GNU Emacs version 26+.

The package uses the GNU Autotools.

The  last time  the author  bothered to  update this  paragraph, he  had
tested GNU Emacs 26.1.

## License

Copyright (c) 2020 Marco Maggi<br/>
`mrc.mgg@gmail.com`<br/>
All rights reserved.

This program is free software: you  can redistribute it and/or modify it
under the  terms of the GNU  General Public License as  published by the
Free Software Foundation,  either version 3 of the License,  or (at your
option) any later version.

This program  is distributed  in the  hope that it  will be  useful, but
WITHOUT   ANY   WARRANTY;  without   even   the   implied  warranty   of
MERCHANTABILITY  or  FITNESS FOR  A  PARTICULAR  PURPOSE.  See  the  GNU
General Public License for more details.

You should have received a copy  of the GNU General Public License along
with this program.  If not, see <http://www.gnu.org/licenses/>.

## Install

To install from a proper release tarball, do this:

```
$ cd mmux-emacs-lisp-addon-0.1.0
$ mkdir build
$ cd build
$ ../configure
$ make
$ make check
$ make install
```

to inspect the available configuration options:

```
$ ../configure --help
```

The Makefile is designed to allow parallel builds, so we can do:

```
$ make -j4 all && make -j4 check
```

which,  on  a  4-core  CPU,   should  speed  up  building  and  checking
significantly.

The Makefile supports the DESTDIR  environment variable to install files
in a temporary location, example: to see what will happen:

```
$ make -n install DESTDIR=/tmp/mmux-emacs-lisp-addon
```

to really do it:

```
$ make install DESTDIR=/tmp/mmux-emacs-lisp-addon
```

After the  installation it is  possible to verify the  installed library
against the test suite with:

```
$ make installcheck
```

From a repository checkout or snapshot  (the ones from the Github site):
we  must install  the GNU  Autotools  (GNU Automake,  GNU Autoconf,  GNU
Libtool), then  we must first run  the script `autogen.sh` from  the top
source directory, to generate the needed files:

```
$ cd mmux-emacs-lisp-addon
$ sh autogen.sh

```

After this  the procedure  is the same  as the one  for building  from a
proper release tarball, but we have to enable maintainer mode:

```
$ ../configure --enable-maintainer-mode [options]
$ make
$ make check
$ make install
```

The Emacs code goes under `$lispdir`, for example:

```
/usr/local/share/emacs/site-lisp
```

so to load the module we should do something like:

```
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'mmux-emacs-lisp-addon)
```

## Usage

Read the documentation generated from  the Texinfo sources.  The package
installs the documentation  in Info format; we can  generate and install
documentation in HTML format by running:

```
$ make html
$ make install-html
```

## Credits

The  stuff was  written by  Marco Maggi.   If this  package exists  it's
because of the great GNU software tools that he uses all the time.

## Bugs, vulnerabilities and contributions

Bug  and vulnerability  reports are  appreciated, all  the vulnerability
reports  are  public; register  them  using  the  Issue Tracker  at  the
project's GitHub  site.  For  contributions and  patches please  use the
Pull Requests feature at the project's GitHub site.

## Resources

The latest release of this package can be downloaded from:

[https://bitbucket.org/marcomaggi/mmux-emacs-lisp-addon/downloads](https://bitbucket.org/marcomaggi/mmux-emacs-lisp-addon/downloads)

development takes place at:

[http://github.com/marcomaggi/mmux-emacs-lisp-addon/](http://github.com/marcomaggi/mmux-emacs-lisp-addon/)

and as backup at:

[https://bitbucket.org/marcomaggi/mmux-emacs-lisp-addon/](https://bitbucket.org/marcomaggi/mmux-emacs-lisp-addon/)

the documentation is available online:

[http://marcomaggi.github.io/docs/mmux-emacs-lisp-addon.html](http://marcomaggi.github.io/docs/mmux-emacs-lisp-addon.html)

the GNU Project software can be found here:

[http://www.gnu.org/](http://www.gnu.org/)

