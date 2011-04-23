#!/bin/bash

echo "cleaning executable files"

for file in `find . -executable -type f -path ./[0-9a-Az-Z]\*` ; do
    rm $file -i
done

echo "cleaning object files generated during compile time"

for file in `find . -type f -path ./[0-9a-Az-Z]\* \( -name \*.hi -or -name \*.o -or -name \*.aux -or -name \*.log -or -name \*.out \) ` ; do
    rm $file -i
done
