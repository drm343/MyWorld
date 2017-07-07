#!/bin/bash
APP_NAME=rogue

SCRIPT_PATH=$(dirname $(readlink -f $0))
$SCRIPT_PATH/bin/$APP_NAME $SCRIPT_PATH
