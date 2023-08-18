import pathlib
import subprocess
files = pathlib.Path(".").glob("*.cu")
def header(f):
    with f.open() as fp:
        return fp.readline()[len("//"):].strip()

for x in sorted(files):
    expected = header(x)
    given = subprocess.check_output(["data-dep", x]).strip().decode()
    if expected != given:
        print(f"{x}: expected={repr(expected)} given={repr(given)}")
    else:
        print("Checked", x)