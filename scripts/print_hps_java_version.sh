#!/bin/sh

if [ -z $1 ]; then
    echo "ERROR: Missing path to POM file!"
    exit 1
fi

pomfile=$1

grep version $pomfile | head -n 1 | sed -e s/version//g -e s/\<//g -e s/\>//g -e s/\\///g | xargs | tr -d '\n'
