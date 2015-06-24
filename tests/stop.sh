#!/bin/sh

if [ -f tests/redis.pid ] ; then
	echo -e "KILL\ttests/redis.pid"
	PID=`cat tests/redis.pid`
	kill $PID
	rm -f tests/redis.pid
	rm -f tests/redis.log
	rm -f tests/redis.sock
fi
