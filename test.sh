find ./extend -name "*.metta" -exec python3 ./extend/test.py {} \;
find ./tests -name "*.metta" -exec sh ./tests/test.sh {} \;
