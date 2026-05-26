#!/bin/bash

# ==============================================================================
# PARAMETERS
# ==============================================================================
ORIGINALS_DIR="../../Problems/Originals"
TARGET_DIR="../../parsed_problems"
UNK_DIR="$TARGET_DIR/UNK"

mkdir -p "$TARGET_DIR"
mkdir -p "$UNK_DIR"

echo "Beginning translation of TPTP problems..."
echo "----------------------------------------"

for filepath in "$ORIGINALS_DIR"/*.p; do
    [ -e "$filepath" ] || continue

    filename=$(basename "$filepath")
    echo "Parsing $filename ..."

    dune exec parser "$filepath" > "$TARGET_DIR/$filename"
    exit_status=$?

    if [ $exit_status -ne 0 ]; then
        echo "Error while parsing $filename"
        rm -f "$TARGET_DIR/$filename"
        continue
    fi

    if [ -f "UNK_pos_$filename" ] || [ -f "UNK_neg_$filename" ]; then
        
        [ -f "UNK_pos_$filename" ] && mv "UNK_pos_$filename" "$UNK_DIR/"
        [ -f "UNK_neg_$filename" ] && mv "UNK_neg_$filename" "$UNK_DIR/"
        
        rm -f "$TARGET_DIR/$filename"
        
        echo "   -> Generated split files in UNK/: UNK_pos_$filename and UNK_neg_$filename"
    fi

done

echo "----------------------------------------"
echo "Finished ! All translated files were put into $TARGET_DIR (and $UNK_DIR)"