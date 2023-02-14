#!/bin/bash


# Source env
source ./script/env.sh

# docker build
docker build  -t $IMAGE  $DEV_PATH

# Run Docker locally
# docker run  -v /data/:/data/ ecap11s6
