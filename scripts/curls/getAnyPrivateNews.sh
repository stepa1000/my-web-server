#!/bin/sh

set -eu

curl -u "loginPrivate:private" http://localhost:3000/get_news/private
