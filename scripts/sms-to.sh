#!/bin/bash

run.sh --jsv sms send --dest "$(lookup-number.sh "$1")" --message "$2"
