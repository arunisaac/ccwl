#! /bin/sh -e

# ccwl --- Concise Common Workflow Language
# Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
#
# This file is part of ccwl.
#
# ccwl is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ccwl is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ccwl.  If not, see <https://www.gnu.org/licenses/>.

set -o pipefail

cwl_file=$1
shift

output_file=$(basename $cwl_file .cwl).out
{
    echo "\$ ccwl compile $(basename $cwl_file .cwl).scm > $(basename $cwl_file)"
    echo -n "\$ cwltool $(basename $cwl_file) "
    for arg in $@
    do
        echo -n "${arg#doc/} "
    done
    echo

    # On Guix, workflows involving gcc need to preserve the
    # LIBRARY_PATH environment variable.
    cwltool --preserve-environment LIBRARY_PATH --outdir doc/cwl-output $cwl_file "$@" 2>&1 \
        | sed '1,2d' \
        | sed 's|\[1;30mINFO\[0m ||g' \
        | sed "s|$(pwd)/doc/cwl-output|/home/manimekalai|g"
} > doc/$output_file
