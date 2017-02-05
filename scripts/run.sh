#!/bin/bash

newstyle=./dist-newstyle/build/servant-pushbullet-client-0.1.0.0/build/example/example
oldstyle=./dist/build/example/example

p=$(test -x $newstyle && echo $newstyle || echo $oldstyle)

exec $p "$@"
