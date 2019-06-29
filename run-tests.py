#!/usr/bin/env python3

import subprocess
import sys
import shlex
import os

from pathlib import Path
from subprocess import check_call, check_output

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
      f(x)
      print("·", end=end)
      return True
  except subprocess.CalledProcessError:
      print(RED("✗") + RESET(), end=end)
      return False
  finally:
      sys.stdout.flush()

def single_test(path, f):
    return [] if progress_call(f, path, end="\n") else [f]


def each_test(dir, f):
    errs = []
    for x in sorted(dir.iterdir()):
        if x.is_dir():
            continue
        if not progress_call(f, x):
            errs.append(x)
    print("")
    if len(errs) > 0:
        print("Errors:")
    for x in errs:
        print("  ", x)
    return errs

class Cmd:
    def __init__(self, cmd):
        self.cmd = cmd

    def __or__(self, o):
        return Cmd(self.cmd + " | " + o.cmd)

    def __str__(self):
        return self.cmd

def faial(x):
    return Cmd("./main " + shlex.quote(str(x)))

def z3(timeout=3000):
    return Cmd("z3 -in -t:%d" % timeout)

def cvc4(timeout=3000):
    return Cmd("cvc4 --lang=smtlib --incremental-parallel --tlimit=%d" % timeout)

def ensure_ok():
    return Cmd("grep -c -e ^sat -e ^unknown") | Cmd("grep 0")

def ensure_fail():
    return Cmd("grep -l ^sat")


def run(op):
    return lambda x: check_call(
      str(op(x)),
      stdout=subprocess.DEVNULL,
      stderr=subprocess.DEVNULL,
      shell=True
    )

def test(label, path, cmd):
    print(label + ": ", end="")
    if len(each_test(path, run(cmd))) > 0:
        sys.exit(1)

def run_all_tests(solver):
    test("Parsing OK tests", Path("examples"), faial)
    test("Parsing FAIL tests", Path("examples/fail"), faial)
    test("Solving OK tests", Path("examples"), lambda x: faial(x) | solver | ensure_ok())
    test("Solving FAIL tests", Path("examples"), lambda x: faial(x) | solver | ensure_fail())

def run_one_test(file, solver):
    subprocess.call(
      str(faial(file) | solver),
      shell=True
    )

def which(program):
    return any(
        os.access(os.path.join(p, program), os.X_OK)
          for p in os.environ["PATH"].split(os.pathsep)
    )

def main():
    import argparse
    parser = argparse.ArgumentParser(description='Runs system tests.')
    parser.add_argument('-f', dest="file", metavar='FILE', help='Tries to solve a single file.')
    parser.add_argument('--z3', action='store_true', help='By default uses cvc4, this lets you use z3')
    args = parser.parse_args()
    if args.z3:
        solver = z3()
    elif which("cvc4"):
        solver = cvc4()
    else:
        solver = z3()
    if args.file is None:
        run_all_tests(solver=solver)
    else:
        run_one_test(solver=solver, file=Path(args.file))



if __name__ == '__main__':
    main()

