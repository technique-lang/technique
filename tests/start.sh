#!/bin/sh

if [ ! -f tests/redis.pid ] ; then
	echo "REDIS\ttests/redis.sock"
	redis-server tests/redis.conf
	sleep 0.1
fi
redis-cli -s tests/redis.sock ping
