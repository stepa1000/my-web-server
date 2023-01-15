#!/bin/sh

set -eu

cd "$(git rev-parse --show-toplevel)"
# find . -name '*.hs' -print0 | xargs -0n100 ormolu --mode inplace
mkdir config
mkdir logging
mkdir data
cd logging
# . text
cat <<EOF>log
EOF
cd ..
cd config
cat <<EOF>server.yaml
confAuthorization:
  confLimit: 10
confConnectionInfo:
  connectDatabase: newsdb
  connectHost: 127.0.0.1
  connectPassword: ''
  connectPort: 5432
  connectUser: stepan
confFailPathToCategoryNews: ./data/categoryNews.json
confLogger:
  preconfFilePath: ./logging/log
  preconfMinLevel: Error
confNews:
  confMaxLimit: 10
EOF
cat <<EOF>serverTest.yaml
confAuthorization:
  confLimit: 10
confConnectionInfo:
  connectDatabase: testDB
  connectHost: 127.0.0.1
  connectPassword: ''
  connectPort: 5432
  connectUser: stepan
confFailPathToCategoryNews: ./data/categoryNewsTest.json
confLogger:
  preconfFilePath: ./logging/logTest
  preconfMinLevel: Debug
confNews:
  confMaxLimit: 10
EOF
# cat <<EOF>LoggerImp.yaml
# loggerimp:
#  confFileHendler: "./logs/log.text"
#  confMinLevel: "Debug"
# EOF
# cat <<EOF>Telegram.yaml
# tokenbot: "your token"
# namebot: "your bot name"
# EOF
cd ..
cd data
cat <<EOF>categoryNews.json
["General",[]]
EOF
cat <<EOF>categoryNewsTest.json
["General",[]]
EOF
cd ..
stack test
stack build
hlint .
