#!/bin/sh

set -eu

curl -i -v -sS 'http://localhost:5435/category/get_tree'
