#!/bin/sh
set -ex

# Create a volume
VOLUME_NAME=orch_build_$(echo ${GIT_URL%.git} | xargs basename)

# Look for existing volumes
VOLUME="$(docker volume ls --format {{.Name}} --filter name="$VOLUME_NAME")"

# Create it only if it doesn't exist
if [[ -z "$VOLUME" ]]; then
    docker volume create "$VOLUME_NAME"
fi

docker run -t \
    --volume "$VOLUME_NAME":/usr/src/app \
    --volume "/home/sgillespie/dev/react-boilerplate:/usr/src/repo" \
    --env GIT_URL=${GIT_URL} \
    git-builder

docker run \
    --volume "$VOLUME_NAME":/usr/src/app \
    node-builder
