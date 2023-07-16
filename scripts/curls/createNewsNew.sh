#!/bin/sh

set -eu

curl -i -v --request POST -u "loginPrivate:private" -H "Content-Type: application/json" -d @exempleNews.json  http://localhost:5435/create_news/new
