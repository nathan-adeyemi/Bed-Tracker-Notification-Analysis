import sys
sys.path.append("src")
import os
import pandas as pd
import numpy as np
import utils 

from itertools import product
from pathlib import Path
from dask_jobqueue import SLURMCluster
from dask.distributed import Client
from multiprocessing import Pool


def parse_args(cfg, args):
    
    if args.exp_name == 'observation-density':
        results_directory = os.path.join(cfg.get("results_directory"), 'single-queue' if args.num_queues == 1 else "multi-queue") 
    else:
        results_directory = cfg.get("results_directory") 
    
    num_replications = 1 if not cfg.get("num_replications") else cfg.get("num_replications")
    results_directory = os.path.join(results_directory, args.config)
    num_queues = cfg.get("num_queues")
    seed = cfg.get("seed")
    
    results_df = []
    sub_df = []
    for param in cfg.get('params'):
        param = cfg.get('params').get(param)
        if param.get("q"):
            p = np.around(np.arange(
                start=param.get("min"),
                stop=param.get("max") + param.get("q"),
                step=param.get("q"),),
                          decimals = 2)
        elif param.get('num_random_samples'):
            p = np.random.uniform(param.get('min'),
                                  param.get('max'),
                                  param.get('num_samples')
            )
        elif param.get('num_samples'):
            p = np.floor(np.arange(
                start=param.get("min"),
                stop=param.get("max") +(param.get("max")/param.get("num_samples")),
                step=param.get("max")/param.get("num_samples"),
            ))
            
        if param.get('type') == "int":
            p = np.floor(p)
        sub_df.append(p)
        
    sub_df = pd.DataFrame(product(*sub_df), columns=list(cfg.get("params").keys()))
    for rep in range(0,num_replications,1):
        temp = sub_df.copy()
        temp['replication'] = int(rep + 1)
        results_df.append(temp)
        
    results_df = pd.concat(results_df)
    
    cluster = parse_cluster_cfg(cli_args=args)
    return num_replications, results_directory, results_df, num_queues, cluster, seed


def parse_cluster_cfg(cli_args):
    
    worker_pool = None
    if not cli_args.backend == 'single':
        cluster_cfg = utils.read_yaml(Path("Code/configs/cluster.yaml").resolve()).get(cli_args.backend).get(cli_args.config)    
        
    if cli_args.backend == 'dask':
        cluster_cfg['log_directory'] = os.path.join(cluster_cfg.get('log_directory'), cli_args.exp_name, cli_args.config)
        num_workers = int(cli_args.num_workers)
        cluster_cfg.update({"walltime": cli_args.time, "queue": cli_args.partition})
        # Set up Dask distr cluster and client
        cluster = SLURMCluster(**cluster_cfg)
        cluster.scale(num_workers)

        worker_pool = Client(cluster, timeout="600s")
        utils.wait_for_cluster(
            client=worker_pool, expected_workers=num_workers, timeout=7200, check_interval=15
        )
        
        for module in ['parsers','utils','runner']:
            worker_pool.upload_file(f"src/{module}.py")
    elif cli_args.backend == 'multiprocessing':
        worker_pool = Pool(cli_args.num_cpus)
    return worker_pool

