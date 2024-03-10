#!/bin/bash

# command-line tools

# run miniflux tool (with the needed postgres database)
alias miniflux="cd ${DIR_SOURCES}/miniflux/contrib/docker-compose && docker compose -p miniflux -f basic.yml up -d"
