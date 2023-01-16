#!/bin/sh

set -eu

curl  -u "tempAdmin:temp" -sS 'http://localhost:3000/user/create?name=namePrivate&login=loginPrivate&password=private&make_news=true&admin=true'
