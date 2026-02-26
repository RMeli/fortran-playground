#!/bin/bash
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=16
#SBATCH --cpus-per-task=16
#SBATCH --time=00:01:00
#SBATCH --partition=debug
#SBATCH --uenv=cp2k/2026.1:v1@daint
#SBATCH --view=cp2k
#SBATCH --partition=normal
#SBATCH --account=csstaff

monitorbuffer=5
rm -f stop_monitor

cat $0

date

export OMP_NUM_THREADS=$((SLURM_CPUS_PER_TASK - 1))
export OMP_PLACES=cores
export OMP_PROB_BIND=close

srun --overlap -n 1 -c 72 monitor.sh ${CLUSTER_NAME} ${monitorbuffer} &

srun --overlap -c ${SLURM_CPUS_PER_RASK} build/pdsygst & 
pid=$!

wait $pid

sleep ${monitorbuffer}
touch stop_monitor

date
