#!/usr/bin/env python3

import subprocess
import sys
import shlex
import os
import sexpdata
from enum import Enum

from pathlib import Path
from subprocess import CompletedProcess

class color:
    def __init__(self, code):
        self.code = '\033[' + str(code) + 'm'

    def __call__(self, x=""):
        return self.code + x

BLACK = color(30)
RED = color(31)
GREEN = color(32)
YELLOW = color(33)
BLUE = color(34)
MAGENTA = color(35)
CYAN = color(36)
WHITE = color(37)
UNDERLINE = color(4)
RESET = color(0)

def progress_call(f, x, end="", verbose=False):
  try:
    if verbose:
        print("*", x, end="")
    result = f(x).run()
    if result.returncode == 0:
        if verbose:
            print(" ✓")
        else:
            print("·", end=end)
        return None
    else:
        if verbose:
            print(RED(" ✗") + RESET())
        else:
            print(RED("✗") + RESET(), end=end)
        return result

  finally:
      sys.stdout.flush()

def single_test(path, f):
    return [] if progress_call(f, path, end="\n") else [f]


def each_test(dir, f, verbose=False):
    errs = []
    for x in sorted(dir.iterdir()):
        if x.is_dir():
            continue
        result = progress_call(f, x, verbose=verbose)
        if result is not None:
            errs.append((x,result))
    print()

    if len(errs) > 0:
        print()

    for x,msg in errs:
        print("~" * 30, "FAILURE:", str(x), "~" * 30)
        print()
        print(msg.stdout.decode("utf-8"))

    return errs
class Cmd:
    def __init__(self, *args):
        self.args = args

    def run(self, stdin=None):
        kwargs = {}
        if stdin is not None:
            kwargs["input"] = stdin
        return subprocess.run(
            self.args,
            stderr=subprocess.STDOUT,
            stdout=subprocess.PIPE,
            #capture_output=True,
            **kwargs
        )

class Mode(Enum):
    OK = 0
    FAIL = 1
    INVALID = 2

def faial(mode, faial_exe="./faial"):
    def run(x):
        args = [faial_exe, str(x)]
        if mode == Mode.OK:
            pass
        elif mode == Mode.FAIL:
            args.append("--expect-race")
        elif mode == Mode.INVALID:
            args.append("--expect-invalid")
        else:
            raise ValueError(mode)
        return Cmd(*args) # | ensure_ok()
    return run

def test(label, path, cmd, verbose=False):
    end = "\n" if verbose else ""
    print(label + ": ", end=end)
    try:
      if len(each_test(path, cmd, verbose=verbose)) > 0:
          sys.exit(1)
    except FileNotFoundError as e:
          print()
          print(e, file=sys.stderr)
          sys.exit(1)

def run_all_tests(verbose=False):
    test("OK tests", Path("examples"), faial(Mode.OK), verbose=verbose)
    test("FAIL tests", Path("examples/fail"), faial(Mode.FAIL), verbose=verbose)
    test("INVALID tests", Path("examples/invalid"), faial(Mode.INVALID), verbose=verbose)

def run_one_test(file, solver):
    modes = {"examples": Mode.OK, "fail": Mode.FAIL, "invalid": Mode.INVALID}
    mode = modes[file.parent.name]
    cmd = faial(mode)(file)
    result = cmd.run()
    print(result.stdout.decode("utf-8"))


def which(program):
    return any(
        os.access(os.path.join(p, program), os.X_OK)
          for p in os.environ["PATH"].split(os.pathsep)
    )

def main():
    import argparse
    parser = argparse.ArgumentParser(description='Runs system tests.')
    parser.add_argument('--timeout', default=3000, type=int, help="Sets the timeout of the solver. Default: %(default)s")
    parser.add_argument('-f', dest="file", metavar='FILE', help='Tries to solve a single file.')
    parser.add_argument('--verbose', action="store_true", help="Increase verbosity level.")
    args = parser.parse_args()
    if args.file is None:
        run_all_tests(verbose=args.verbose)
    else:
        run_one_test(file=Path(args.file))



if __name__ == '__main__':
    main()
