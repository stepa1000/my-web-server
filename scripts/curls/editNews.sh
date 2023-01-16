#!/bin/sh

set -eu

curl -i -v -u "loginPrivate:private" -sS 'http://localhost:3000/create_news/edit?news_name=exepleName&public=true'
