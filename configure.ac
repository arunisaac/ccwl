dnl ccwl --- Concise Common Workflow Language
dnl Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
dnl
dnl This file is part of ccwl.
dnl
dnl ccwl is free software: you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl ccwl is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with ccwl.  If not, see <https://www.gnu.org/licenses/>.

AC_INIT([ccwl], [0.1.0])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign check-news no-dist-gzip dist-lzip])
AM_SILENT_RULES([yes])

GUILE_PKG([3.0 2.2])
GUILE_PROGS

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([scripts/ccwl], [chmod +x scripts/ccwl])
AC_OUTPUT
