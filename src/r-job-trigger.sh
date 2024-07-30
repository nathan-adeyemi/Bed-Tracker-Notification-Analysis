# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!

__conda_setup="$('/shared/centos7/anaconda3/2022.01/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/shared/centos7/anaconda3/2022.01/etc/profile.d/conda.sh" ]; then
        . "/shared/centos7/anaconda3/2022.01/etc/profile.d/conda.sh"
    else
        export PATH="/shared/centos7/anaconda3/2022.01/bin:$PATH"
    fi
fi
unset __conda_setup

cd /home/adeyemi.n/MH_Simulation/Bed-Tracker-Notification-Analysis
module load singularity/3.5.3
conda activate ed_ip_simulation
# echo "Beginning the R simulation"
singularity exec --bind "/scratch/:/scratch/,/work/:/work/" /shared/container_repository/rstudio/rocker-geospatial-4.2.1.sif Rscript src/r-setup.R