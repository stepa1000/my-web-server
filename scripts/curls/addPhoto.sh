#!/bin/sh

set -eu

VAR_1=$(base64 ./photo.jpg)

curl -i -v -u "loginPrivate:private" -sS 'http://localhost:3000/create_news/edit?news_name=exepleName&new_photos=$VAR_1'
