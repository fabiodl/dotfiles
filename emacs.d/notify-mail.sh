#!/bin/sh
notmuch search --format json tag:unread AND NOT tag:Sent  | jq '.[0] | "\(.authors) \(.subject)"' | xargs notify-send -t 10000 -i mail-notification
