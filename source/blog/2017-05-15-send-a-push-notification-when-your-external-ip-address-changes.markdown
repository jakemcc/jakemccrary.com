---
dated-url: true
layout: post
title: Send a push notification when your external IP address changes
date: 2017-05-15 22:15 -0500
comments: true
published: true
description: Use dig and curl to send a push notification when your external IP address
  changes.
keywords: bash, pushover, external ip
categories: [bash]
---

I need to know when my external IP address changes. Whenever it changes, I need to update an IP whitelist and need to re-login to a few sites. I sometimes don't notice for a couple of days and, during that time, some automatic processes fail.

After the last time this happened, I whipped up a script that sends me a push notification when my IP address changes.

The script uses [Pushover](https://pushover.net/) to send the push notification. Pushover is great. I have used it for years to get notifications from my headless computers. If you use the below script, replace `${PUSHOVER_TOKEN}` and `${PUSHOVER_USER}` with your own details.

```bash
#!/bin/bash

set -e

previous_file="${HOME}/.previous-external-ip"

if [ ! -e "${previous_file}" ]; then
    dig +short myip.opendns.com @resolver1.opendns.com > "${previous_file}"
fi

current_ip=$(dig +short myip.opendns.com @resolver1.opendns.com)

previous_ip=$(cat "${previous_file}")

if [ "${current_ip}" != "${previous_ip}" ]; then
    echo "external ip changed"
    curl -s --form-string "token=${PUSHOVER_TOKEN}" \
         --form-string "user=${PUSHOVER_USER}" \
         --form-string "title=External IP address changed" \
         --form-string "message='${previous_ip}' => '${current_ip}'" \
         https://api.pushover.net/1/messages.json
fi

echo "${current_ip}" > "${previous_file}"
```