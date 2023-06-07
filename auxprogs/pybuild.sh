#! /bin/sh

# "Build" a given Python file `foo`: format it, type-check it, lint it, and
# generate the final file from the `foo.in` file.
#
# The following Python tools are used by this script.
#
# - Formatters:
#   - `black`, for general formatting. This avoids the need for style checkers
#     like `flake8`. Note that `black` allows a max line length of 88, which is
#     a mild but common PEP-8 violation.
#   - `isort`, for import sorting.
#
# - Type-checkers:
#   - `mypy`. This is the most commonly used Python type checker.
#   - `pyright`. This is another good type checker.
#   - Sometimes they give different result. Both should be kept happy.
#
# - Linters:
#   - `ruff`. Sometimes useful, and very fast to run.
#   - `pylint`. Sometimes annoying, sometimes useful. The `pylintrc`
#     modifies/disables the more annoying lints.
#   - Sometimes they give different result. Both should be kept happy.
#
# The following tools are relevant, but not run by this script.
#
# - Profilers:
#   - `cProfile` + `snakeviz`: Typically run with
#     `python3 -m cProfile -o cg.prof cg_annotate $INPUT && snakeviz cg.prof`.
#   - `scalene`. Typically run with `scalene ./cg_annotate $INPUT`.
#
# - Packager:
#   - `cp` is used for distribution. This is possible because this program is a
#     single file and only uses the Python Standard Library. This avoids the
#     needs for any of the million different Python package management tools.
#
# All of the above tools can be installed with `pip3 install $NAME`, except
# `cProfile` which is built into Python.

set -e

# Currently targetting Python 3.9 (released in October 2020) and up. The tools
# use two different syntaxes for specifying the version number.
ver=3.9
pyver=py39

auxprogs=$1
infile=$2
outfile=$3
if [ -z "$outfile" ] ; then
    exit 1
fi

echo "== black =="
black $infile
echo

echo "== isort =="
isort $infile
echo

echo "== mypy =="
mypy --strict $infile --python-version $ver
echo

# Strict mode for pyright is enabled by a `pyright: strict` comment inside each
# Python file.
#
# Note: `pyright` refuses to check any file without a `.py` extension, hence
# the copying to a temp file with a `.py` extension.
echo "== pyright =="
tmpfile=`mktemp --tmpdir $infile.XXX.py`
echo "program output" >> $tmpfile
cp $infile $tmpfile
pyright --pythonversion $ver $tmpfile
rm $tmpfile
echo

echo "== ruff =="
ruff check --target-version $pyver $infile
echo

echo "== pylint =="
pylint --rcfile=$auxprogs/pylintrc --py-version $ver $infile

echo "== config.status =="
make $outfile
echo

