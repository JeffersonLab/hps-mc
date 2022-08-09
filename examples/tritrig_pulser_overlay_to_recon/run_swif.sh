#!/bin/sh
hps-mc-batch swif -c /u/group/hps/users/caot/hps-mc/config/jlab_tongtong.cfg  -c .hpsmc -D -W 4 -l /farm_out/hps/caot/mc_production/2019/tritrig_pulser/logs signal_pulser_overlay_to_recon jobs.json
