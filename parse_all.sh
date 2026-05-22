#!/bin/bash

ORIGINALS_DIR="../../Problems/Originals"
TARGET_DIR="../../parsed_problems"

mkdir -p "$TARGET_DIR"

echo "Beginning translation of TPTP problems..."
echo "----------------------------------------"

for filepath in "$ORIGINALS_DIR"/*.p; do
    [ -e "$filepath" ] || continue

    filename=$(basename "$filepath")

    echo "Parsing $filename ..."

    dune exec parser "$filepath" > "$TARGET_DIR/$filename"

    if [ $? -ne 0 ]; then
        echo "Error while parsing $filename"
    fi
done

echo "----------------------------------------"
echo "Finished ! All translated files were put into $TARGET_DIR"