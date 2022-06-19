#!/bin/sh

set -xe

PORT=9000

python -m http.server $PORT --directory /home/iamparadox/.stack/snapshots/x86_64-linux/36f77524b99ee1cfdc85de799b5ec0be3833152c21bb61162c258349e7311cc4/8.10.7/doc/
