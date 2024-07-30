import os
import sys

sys.path.append('src')
import argparse
import numpy as np
import pandas as pd
import utils 
import re
import subprocess

from pathlib import Path
from parsers import parse_args
from runner import run_r_job
from dask import config as cfg

cfg.set({'distributed.scheduler.worker-ttl': None})

def train_fn(param) -> dict:
    if args.num_queues:
        param.update({'num_queues': args.num_queues})
    
    if args.experiment == 'observation-density':
        train_scratch_path = os.path.join(scratch_path, 'single-queue' if args.num_queues == 1 else "multi-queue") 
    else:
        train_scratch_path = scratch_path
    
    results = run_r_job(input_param=param, 
              cfg_path = yml_path, 
              cfg_opt = args.config, 
              scratch_path = train_scratch_path)
    return results

def train_wrapper(param):
        train_fn(param=param, args=args)

if __name__ == "__main__":
    
    cluster_cfg_path = "Code/configs/cluster.yaml"

    parser = argparse.ArgumentParser(
        description="Execute the ED arrival scaling paramter tuning"
    )
    
    parser.add_argument(
        "--experiment",
        choices=[re.sub("\\.R|\\.r", "", i) for i in os.listdir("Code/analyses")],
        default="observation-density",
        help="Choose the paramter tuning job to execute",
    )
    
    parser.add_argument(
        "--config",
        default='debug',
        choices=['debug','test','full']
    )
    
    parser.add_argument(
        "--backend",
        "-b",
        choices=list(utils.read_yaml(filename=cluster_cfg_path).keys()).append('single'),
        default='single',
        help="Choose the cluster configuration"
    )
    
    parser.add_argument(
        '--num-workers',
        "-nw",
        default=1,
    )
    
    parser.add_argument(
        "--time",
        "-t",
        type=str,
        default="01:00:00",
        help="How long to reserve the compute node for",
    )
    
    parser.add_argument(
        '--num-queues',
        "-nq",
        action='store',
        type=int,
        default=None
    )
    
    parser.add_argument(
        "--partition",
        "-p",
        type=str,
        default='short'
    )

    args = parser.parse_args()
    yml_path = f"Code/configs/{args.experiment}.yaml"
    scratch_path = os.path.join("/scratch/adeyemi.n/", args.experiment)
    if not os.path.exists(scratch_path):
        os.makedirs(scratch_path)

    cfg = utils.read_yaml(yml_path).get(args.config)
    if args.num_queues:
        cfg.update({"num_queues": args.num_queues})
        # Set variables from cfg
    (
        num_replications,
        results_directory,
        results_df,
        num_queues,
        worker_pool,
        seed,
    ) = parse_args(cfg=cfg, args=args)

    if not os.path.exists(results_directory):
        os.makedirs(results_directory)
        print(f'Results directory created at: {results_directory}')
    else:
        print(f'Results stored at: {results_directory}')

    # Begin Experiments
    np.random.seed(seed)

    # Generate the oringal state and observation sequences    
    if args.experiment == "repair-budget-SA":
        _ = train_fn(
            param = 
            {
                "analysis_path": "Code/analyses/observation-scenario-generator.R"
                },
            args=args
            )
    
    if args.backend == 'single':
        job_results = [train_fn(param=param) for param in results_df.to_dict(orient="records")]
    else:
        job = worker_pool.map(train_fn, results_df.to_dict(orient='records'))
        if args.backend == 'dask':
            job_results=worker_pool.gather(job)

    # Convert analysis results results to DataFrame
    job_results = pd.DataFrame(job_results, columns = list(job_results[0].keys()))

    # Combine results
    results_df = pd.concat(
        [results_df.reset_index().drop(columns=["index"]), job_results], axis=1
    )
    results_df.to_csv(
        path_or_buf=Path(os.path.join(results_directory, 'results_df.csv')).resolve()
    )
    
    if args.experiment == "repair-budget-SA":
        bash_command = ";".join([
            'source .bashrc', 
            "Rscript Code/analyses/plot-repair_budget-SA.R"
        ])
        
        subprocess.Popen(
            bash_command,
            shell = True
        )

