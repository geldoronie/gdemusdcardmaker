#!/bin/bash

ROOT="."
CACHE_DIR=$ROOT/cache
GAME_ROOT=$ROOT/temp/openbor
GAME_NAME="GAME TESTE"
OUTPUT=$ROOT/temp/

mkdir -p $GAME_ROOT

cp -f $ROOT/data/openbor/* $GAME_ROOT

genisoimage -C 0,11702 \
            -V OPENBOR_ENGINE \
            -G "$GAME_ROOT/IP.BIN" \
            -r \
            -J \
            -l \
            -input-charset iso8859-1 \
            -o $OUTPUT/game.iso \
            "$GAME_ROOT/1ST_READ.BIN" \
            "$GAME_ROOT/BOR.PAK" \
            "$GAME_ROOT/IP.BIN" \
            "$GAME_ROOT/README.TXT" \
            "$GAME_ROOT/DISC_ID.TXT"

./tools/cdi4dc $OUTPUT/game.iso $OUTPUT/game.cdi 

rm -r $GAME_ROOT
rm $OUTPUT/game.iso