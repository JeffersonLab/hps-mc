#!/bin/sh
hps-mc-batch auger -D -c ~/.hpsmc -c .hpsmc -W 4 -l $PWD/logs beam_gen jobs.json
