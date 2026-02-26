#!/bin/bash

outdir=$1
startup=$2

rm -rf ${outdir} && mkdir ${outdir}

while [ ! -f stop_monitor ];
do
  cat /sys/cray/pm_counters/power >> ${outdir}/node_power.txt
  cat /sys/cray/pm_counters/accel0_power >> ${outdir}/gpu0_power.txt
  cat /sys/cray/pm_counters/accel1_power >> ${outdir}/gpu1_power.txt
  cat /sys/cray/pm_counters/accel2_power >> ${outdir}/gpu2_power.txt
  cat /sys/cray/pm_counters/accel3_power >> ${outdir}/gpu3_power.txt
  cat /sys/cray/pm_counters/cpu0_power >> ${outdir}/cpu0_power.txt
  cat /sys/cray/pm_counters/cpu1_power >> ${outdir}/cpu1_power.txt
  cat /sys/cray/pm_counters/cpu2_power >> ${outdir}/cpu2_power.txt
  cat /sys/cray/pm_counters/cpu3_power >> ${outdir}/cpu3_power.txt
  sleep 0.1
done

echo "Monitor stopping."
