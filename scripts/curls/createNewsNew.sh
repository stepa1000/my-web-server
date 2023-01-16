#!/bin/sh

set -eu

curl -i -v --request POST -u "loginPrivate:private" -H "Content-Type: application/json" -d @exempleNews.json  http://localhost:3000/create_news/new
