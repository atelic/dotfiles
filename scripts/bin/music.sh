#! /usr/bin/bash
if [$(pidof mopidy) ]
then
	ncmpcpp
else
	mopidy </dev/null &>/dev/null &
	ncmpcpp
fi
