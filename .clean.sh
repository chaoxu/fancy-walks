#!/bin/bash

echo "cleaning executable files"

find . -executable -type f -path ./[0-9a-Az-Z]\* -delete

echo "cleaning object files generated during compile time"

find . -type f -path ./[0-9a-Az-Z]\* \( -name \*.hi -or -name \*.o -or -name \*.aux -or -name \*.log -or -name \*.out \) -delete
