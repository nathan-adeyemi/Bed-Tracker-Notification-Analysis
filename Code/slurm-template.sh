#!/bin/bash
${PARTITION_OPTION}
#SBATCH --time=${TIME}
#SBATCH --nodes=${NUM_NODES}
#SBATCH --mem=${MEMORY}GB
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=${NUM_CPUS}
#SBATCH --output=/home/adeyemi.n/MH_Simulation/Bed-Tracker-Notification-Analysis/logs/${JOB_NAME}.out
#SBATCH --error=/home/adeyemi.n/MH_Simulation/Bed-Tracker-Notification-Analysis/logs/${JOB_NAME}.err

cd /home/adeyemi.n/MH_Simulation/Bed-Tracker-Notification-Analysis
module load singularity/3.5.3
module load anaconda3/2022.05

eval "$(conda shell.bash hook)"
conda activate ed_ip_simulation

singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript Code/experiments/${JOB_NAME}.R ${ADD_ARGS}
