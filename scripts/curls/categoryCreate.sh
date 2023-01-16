#!/bin/sh

set -eu

curl -i -v -u "loginPrivate:private" -sS 'http://localhost:3000/category/create?root=General&category_name=exempleCategory'
