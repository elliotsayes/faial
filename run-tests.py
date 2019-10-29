#!/usr/bin/env python3

import subprocess
import sys
import shlex
import os
import sexpdata

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

def progress_call(f, x, end=""):
  try:
    result = f(x).run()
    if result.returncode == 0:
        print("·", end=end)
        return None
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
        result = progress_call(f, x)
        if result is not None:
            errs.append((x,result))
    print("")
    if len(errs) > 0:
        print("Errors:")
    for x,msg in errs:
        if verbose:
            print()
            print(str(x)+":")
            print(msg.stdout.decode("utf-8"))
        else:
            print("  ", x)

    return errs

class IgnoreRet:
    def __init__(self, cmd):
        self.cmd = cmd

    def run(self, stdin=None):
        result = self.cmd.run(stdin=stdin)
        result.returncode = 0
        return result

    def __or__(self, other):
        return self.cmd | other

class Py:
    def __init__(self, func):
        self.func = func

    def run(self, stdin=None):
        return self.func(stdin=stdin)

    def __or__(self, other):
        return Pipe(self, other)

class Pipe:
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __or__(self, other):
        return Pipe(self, other)

    def run(self, stdin=None):
        result1 = self.left.run(stdin=stdin)
        if result1.returncode != 0:
            return result1
        result2 = self.right.run(stdin=result1.stdout)
        return CompletedProcess(
            args = result2.args,
            stderr = result1.stderr + result2.stderr,
            stdout = result2.stdout,
            returncode = result2.returncode
        )

class Cmd:
    def __init__(self, *args):
        self.args = args

    def run(self, stdin=None):
        kwargs = {}
        if stdin is not None:
            kwargs["input"] = stdin
        return subprocess.run(self.args, capture_output=True, **kwargs)

    def __or__(self, other):
        return Pipe(self, other)


def faial(x):
    return Cmd("./main", str(x))

def z3(timeout=3000):
    return IgnoreRet(Cmd("z3",  "-in", "-t:%d" % timeout))

def cvc4(timeout=3000):
    return IgnoreRet(Cmd("cvc4", "--lang=smtlib", "--incremental-parallel", "--tlimit=%d" % timeout))


def parse_smtlib(stdin):
    data = "(" + stdin.decode("utf-8") + ")"
    elems = []
    for idx, row in enumerate(sexpdata.loads(data)):
        if idx % 2 == 0:
            elems.append(row.value())
    return elems

def ensure_ok():
    def handle(stdin):
        elems = parse_smtlib(stdin)
        return CompletedProcess(
          stdout = stdin,
          stderr = b'',
          returncode=0 if all(map(lambda x: x == "unsat", elems)) else 255,
          args = [],
        )
    return Py(handle)

def ensure_fail():
    def handle(stdin):
        elems = parse_smtlib(stdin)
        return CompletedProcess(
          stdout = stdin,
          stderr = b'',
          returncode=0 if any(map(lambda x: x == "sat" or "unknown", elems)) else 255,
          args = [],
        )
    return Py(handle)

def test(label, path, cmd, verbose=False):
    print(label + ": ", end="")
    if len(each_test(path, cmd, verbose=verbose)) > 0:
        sys.exit(1)

def run_all_tests(solver, verbose=False):
    test("Parsing OK tests", Path("examples"), faial, verbose=verbose)
    test("Parsing FAIL tests", Path("examples/fail"), faial, verbose=verbose)
    test("Solving OK tests", Path("examples"), lambda x: faial(x) | solver | ensure_ok(), verbose=verbose)
    test("Solving FAIL tests", Path("examples/fail"), lambda x: faial(x) | solver | ensure_fail(), verbose=verbose)

def run_one_test(file, solver):
    cmd = faial(file) | solver
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
    parser.add_argument('--cvc4', action="store_false", dest="z3", help="Use CVC4 instead of Z3 (default).")
    parser.add_argument('--verbose', action="store_true", help="Increase verbosity level.")
    #parser.add_argument('--z3', action='store_true', help='By default uses cvc4, this lets you use z3')
    args = parser.parse_args()
    if args.z3:
        solver = z3(timeout=args.timeout)
    elif which("cvc4"):
        solver = cvc4(timeout=args.timeout)
    else:
        solver = z3(timeout=args.timeout)
    if args.file is None:
        run_all_tests(solver=solver, verbose=args.verbose)
    else:
        run_one_test(solver=solver, file=Path(args.file))



if __name__ == '__main__':
    main()
