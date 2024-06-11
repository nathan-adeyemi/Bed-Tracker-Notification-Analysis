# slurm-launch.py
# Usage:
# python slurm-launch.py --exp-name test \
#     --command "rllib train --run PPO --env CartPole-v0"

import argparse
import subprocess
import sys
import time
import os
import random
import string
from pathlib import Path


def generate_random_string(n_char):
    # Define the character set: you can include letters, digits, and punctuation
    char_set = string.ascii_letters + string.digits
    # Generate a random string of the specified length
    random_string = ''.join(random.choices(char_set, k=n_char))
    return random_string

template_file = os.path.join(os.path.dirname(__file__),"slurm-template.sh")
JOB_NAME = "${JOB_NAME}"
NUM_NODES = "${NUM_NODES}"
TIME = "${TIME}"
NUM_CPUS_PER_NODE = "${NUM_CPUS}"
MEMORY = "${MEMORY}"
PARTITION_OPTION = "${PARTITION_OPTION}"


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--exp-name",
        type=str,
        required=True,
        help="The job name and path to logging file (exp_name.log).",
    )
    parser.add_argument(
        "--num-nodes", "-n", type=int, default=1, help="Number of nodes to use."
    )

    parser.add_argument(
        "--num-cpus",
        "-nc",
        type=int,
        default=1,
        help="Number of CPUs to use in each node. (Default: 1)",
    )
    
    parser.add_argument(
        "--partition",
        "-p",
        type=str,
    )
    
    parser.add_argument(
        '--memory',
        '-m',
        type=int,
        default='1'
    )
    
    parser.add_argument(
        "--time",
        "-t",
        type=str,
        default="01:00:00",
        help = "How long to reserve the compute node for"
    )
    
    args = parser.parse_args()
    
    job_name = args.exp_name

    partition_option = (
        "#SBATCH --partition={}".format(args.partition) if args.partition else ""
    )

    # ===== Modified the template script =====
    with open(template_file, "r") as f:
        text = f.read()
    text = text.replace(JOB_NAME, job_name)
    text = text.replace(NUM_NODES, str(args.num_nodes))
    text = text.replace(NUM_CPUS_PER_NODE, str(args.num_cpus))
    text = text.replace(PARTITION_OPTION, partition_option)
    text = text.replace(TIME, str(args.time))
    text = text.replace(MEMORY,str(args.memory))
    text = text.replace(
        "# THIS FILE IS A TEMPLATE AND IT SHOULD NOT BE DEPLOYED TO " "PRODUCTION!",
        "# THIS FILE IS MODIFIED AUTOMATICALLY FROM TEMPLATE AND SHOULD BE "
        "RUNNABLE!",
    )

    # ===== Save the script =====
    script_file = f"Code/experiments/{generate_random_string(10)}.sh"
    with open(script_file, "w") as f:
        f.write(text)

    # ===== Submit the job =====
    print("Starting to submit job!")
    subprocess.Popen(["sbatch", f"--job-name={job_name}", script_file])
    print(
        "Job submitted! Script file is at: <{}>. Log file is at: <{}>".format(
            script_file, "{}.log".format(job_name)
        )
    )
    time.sleep(5)
    subprocess.Popen(['rm', '-f', script_file])
    sys.exit(0)