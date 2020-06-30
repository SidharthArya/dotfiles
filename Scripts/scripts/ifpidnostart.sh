#!/usr/bin/env bash

all_args=("$@")
arg1=$1
arg2="${all_args[@]:1}"

if [[ "$(pidof $arg1)" == "" ]]
then
		$arg2 &
else
		echo "Already running";
fi

