#!/bin/sh

set -eu

curl -i -v -u "tempAdmin:temp" -sS 'http://localhost:5435/category/create?category_name=withoutRoot'
