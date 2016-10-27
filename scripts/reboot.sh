#! /usr/bin/env zsh

set -x
resty/nginx/sbin/nginx -t && resty/nginx/sbin/nginx -s reload
