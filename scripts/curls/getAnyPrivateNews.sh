#!/bin/sh

set -eu

curl -u "loginPrivate:private" http://localhost:5435/get_news/private
