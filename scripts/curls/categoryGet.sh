#!/bin/sh

set -eu

curl -i -v -sS 'http://localhost:3000/category/get_tree'
