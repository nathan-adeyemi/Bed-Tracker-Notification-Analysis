import yaml
import time
import string
import random
import os
import shutil
import glob 

def read_yaml(filename):
    with open(filename, "r") as file:
        data = yaml.safe_load(file)
    return data

def wait_for_cluster(client, expected_workers, timeout=600, check_interval=10):
    start_time = time.time()
    while True:
        # Get the number of currently connected workers
        n_workers = len(client.scheduler_info()["workers"])

        if n_workers >= expected_workers:
            print(f"Cluster is ready with {n_workers} workers.")
            break

        # Check for timeout
        elapsed_time = time.time() - start_time
        if elapsed_time > timeout:
            raise TimeoutError("Timeout waiting for Dask cluster to be ready.")

        # Wait before checking again
        time.sleep(check_interval)
        print(
            f"Waiting for cluster to be ready... Currently {n_workers} workers connected."
        )

def generate_random_string(n_char: int = 10):
    # Define the character set: you can include letters, digits, and punctuation
    char_set = string.ascii_letters + string.digits
    # Generate a random string of the specified length
    random_string = "".join(random.choices(char_set, k=n_char))
    return random_string


def find_previous_replications(target_dir, pattern):
    matching_files = glob.glob(os.path.join(target_dir, pattern))
    new_subfolder = "old"
    for root, dirs, files in traverse_directories(root_dir=target_dir):
        for file in files:
            if os.path.join(root, file) not in matching_files:
                shutil.move(
                    os.path.join(root, file), os.path.join(root, new_subfolder, file)
                )
        for dir in dirs:
            if os.path.join(root, dir) not in matching_files:
                shutil.move(
                    os.path.join(root, dir), os.path.join(root, new_subfolder, dir)
                )
def traverse_directories(root_dir):
    """Traverses a directory and its subdirectories recursively.

    Args:
        root_dir (str): The root directory to start from.

    Yields:
        tuple: A tuple containing the current directory path, a list of subdirectories, and a list of files.
    """
    for root, dirs, files in os.walk(root_dir):
        yield root, dirs, files
