import pathlib
import subprocess
base_dir = pathlib.Path(__file__).parent
exe = base_dir.parent.parent / "data-dep"
files = base_dir.glob("*.cu")
def header(f):
    with f.open() as fp:
        return fp.readline()[len("//"):].strip()

for x in sorted(files):
    expected = header(x)
    given = subprocess.check_output([exe, x]).strip().decode()
    if expected != given:
        print(f"{x}: expected={repr(expected)} given={repr(given)}")
    else:
        print(exe, x)
