#!/bin/bash

confirm() {
    # call with a prompt string or use a default
    read -r -p "${1:-Are you sure? [y/N]} " response
    case "$response" in
        [yY][eE][sS]|[yY])
            true
            ;;
        *)
            false
            ;;
    esac
}

if [ "x${CLOUDFLARE_AUTH_KEY}x" != "xx" ]; then
    confirm "Do you want to clear Cloudflare's cache of the main files? [y/N]" && \
    curl -X DELETE "https://api.cloudflare.com/client/v4/zones/05e4518e68577e8be55ec1039e4ff3f4/purge_cache" \
         -H "X-Auth-Email:jake@jakemccrary.com" \
         -H "X-Auth-Key:${CLOUDFLARE_AUTH_KEY}" \
         -H "Content-Type:application/json" \
         --data '{"files":["http://jakemccrary.com/", "http://jakemccrary.com/index.html", "http://jakemccrary.com/atom.xml", "http://jakemccrary.com/feed.json", "http://www.jakemccrary.com/", "http://www.jakemccrary.com/index.html", "http://www.jakemccrary.com/atom.xml", "http://www.jakemccrary.com/feed.json", "https://jakemccrary.com/", "https://jakemccrary.com/index.html", "https://jakemccrary.com/atom.xml", "https://jakemccrary.com/feed.json", "https://www.jakemccrary.com/", "https://www.jakemccrary.com/index.html", "https://www.jakemccrary.com/atom.xml", "https://www.jakemccrary.com/feed.json", "https://jakemccrary.com/blog/archives/", "https://jakemccrary.com/blog/archives/index.html", "https://www.jakemccrary.com/blog/archives/", "https://www.jakemccrary.com/blog/archives/index.html", "http://jakemccrary.com/blog/archives/", "http://jakemccrary.com/blog/archives/index.html", "http://www.jakemccrary.com/blog/archives/", "http://www.jakemccrary.com/blog/archives/index.html"]}'
else 
    echo "No CLOUDFLARE_AUTH_KEY set"
    exit 1
fi 
