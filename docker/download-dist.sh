C_TO_J="https://gitlab.com/umb-svl/c-to-json/-/jobs/artifacts/master/raw/build/c-to-json-bin.tar.bz2?job=build-rh610&inline=false"
FAIAL_INFER="https://gitlab.com/umb-svl/faial-infer/-/archive/master/faial-infer-master.zip"
FAIAL="https://gitlab.com/cogumbreiro/faial/-/jobs/artifacts/master/download?job=build-dist"
set -x
wget "${C_TO_J}" -O c-to-json.tar.bz2 &&
tar xf c-to-json.tar.bz2 &&
rm -f c-to-json.tar.bz2 &&
wget "${FAIAL_INFER}" -O faial-infer.zip &&
mkdir faial-infer &&
unzip faial-infer.zip &&
mv faial-infer-master/faial-infer faial-infer/ &&
mv faial-infer-master/src faial-infer/ &&
mv faial-infer-master/requirements.txt . &&
rm -rf faial-infer-master faial-infer.zip &&
cd bin &&
wget "${FAIAL}" -O faial.zip &&
unzip faial.zip &&
rm -f faial.zip &&
mv README.md LICENSE .. &&
cd .. &&
zip -r faial-dist.zip .