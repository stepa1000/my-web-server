#!/bin/sh

set -eu

curl -i -v -u "loginPrivate:private" -sS 'http://localhost:5435/category/change?category_name=exempleCategory&new_name=ec'
