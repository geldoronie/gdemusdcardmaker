#!/bin/bash

ROOT="."
CACHE_DIR=$ROOT/cache
GAME_ROOT=$ROOT/temp/vcdc
GAME_NAME="GAME TESTE"
OUTPUT=$ROOT/temp/

MOVIE_FILE="$GAME_ROOT/MOVIE.MPG"
LOCAL_MOVIE="/mnt/wwn-0x5000c500c4bcef7c-part1/Animes/Ghost in the Shell/[KSensei] Ghost in the Shell - 2.0 [ANSK-BDRip].mkv"

mkdir -p $GAME_ROOT

cp -f $ROOT/data/vcdc/* $GAME_ROOT

#Resampling and Rescaling
ffmpeg -i "$LOCAL_MOVIE" -qscale 5 -vf scale=640:-1  -pix_fmt yuv420p -vcodec mpeg2video -profile 0 -level 0 -bf 0 -acodec mp3 -b:a 40k -ar 22050 "$OUTPUT/MOVIE-SCALED.MPG"
#Adding Letterbox MPG2
ffmpeg -i "$OUTPUT/MOVIE-SCALED.MPG" -qscale 5 -vf pad=640:480:0:-1  -pix_fmt yuv420p -vcodec mpeg2video -profile 0 -level 0 -bf 0 -acodec mp3 -b:a 40k -ar 22050 "$GAME_ROOT/MOVIE.MPG"
#Resampling and Rescaling 
#ffmpeg -i ./MOVIE.MPG -qscale 5 -vtag DX50 -vf scale=640:-1 -pix_fmt yuv420p -vcodec mpeg4 -profile 0 -level 0 -bf 0 -acodec mp3 -b:a 40k  -ar 22050 MOVIE.AVI
#Resampling and Rescaling DIVX
#ffmpeg -i ./MOVIE.MPG -qscale 5 -vtag DX50 -vf pad=640:480:0:-1 -pix_fmt yuv420p -vcodec mpeg4 -profile 0 -level 0 -bf 0 -acodec mp3 -b:a 40k  -ar 22050 MOVIE.AVI

#DCMPLAYER
# genisoimage -C 0,11702 \
#             -V OPENBOR_ENGINE \
#             -G "$GAME_ROOT/IP.BIN" \
#             -r \
#             -J \
#             -l \
#             -input-charset iso8859-1 \
#             -o $OUTPUT/game.iso \
#             "$GAME_ROOT/1ST_READ.BIN" \
#             "$GAME_ROOT/IP.BIN" \
#             "$GAME_ROOT/MOVIE.MPG"
#DCEVO
# genisoimage -C 0,11702 \
#             -V OPENBOR_ENGINE \
#             -G "$GAME_ROOT/IP.BIN" \
#             -r \
#             -J \
#             -l \
#             -input-charset iso8859-1 \
#             -o $OUTPUT/game.iso \
#             "$GAME_ROOT/1ST_READ.BIN" \
#             "$GAME_ROOT/IP.BIN" \
#             "$GAME_ROOT/000DUMMY.DAT" \
#             "$GAME_ROOT/BACKGROUND.JPG" \
#             "$GAME_ROOT/MOVIE.AVI"
#VCDC
genisoimage -C 0,11702 \
            -V OPENBOR_ENGINE \
            -G "$GAME_ROOT/IP.BIN" \
            -r \
            -J \
            -l \
            -input-charset iso8859-1 \
            -o $OUTPUT/game.iso \
            "$GAME_ROOT/1ST_READ.BIN" \
            "$GAME_ROOT/IP.BIN" \
            "$GAME_ROOT/VCDC.JPG" \
            "$GAME_ROOT/MOVIE.MPG"

./tools/cdi4dc $OUTPUT/game.iso $OUTPUT/game.cdi 

rm -r $GAME_ROOT
rm $OUTPUT/game.iso