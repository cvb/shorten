#!/bin/sh

defurl=jdbc:postgresql://localhost:5432/
db=shorten
if [ -n "$1" ]; then
   db=$1
fi
sbt -Dflyway.url=$defurl$db flywayMigrate
