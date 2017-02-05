#!/bin/bash

run.sh --jsv sms threads |
jq "select(.recipients[0].name | contains(\"$1\"))"
