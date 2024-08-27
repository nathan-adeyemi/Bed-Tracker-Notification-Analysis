import sys
sys.path.append('src')

import os
import utils
import subprocess
import json
import socket
import re
import time
from pathlib import Path

def run_r_job(input_param: dict, 
              cfg_path: str, 
              cfg_opt: str, 
              scratch_path: str) -> dict:
        
    # trial_name = f"trial-{utils.generate_random_string(n_char = 6)}"
    trial_path = Path(os.path.join(scratch_path, dict_to_path(input_param))).resolve()
    server, port = _find_and_connect(server_wait_tol=600)
    subprocess_env = os.environ.copy()
    subprocess_env.update(
        {
            "trial_path": trial_path,
            "cfg_type": str(cfg_opt),
            "cfg_path": str(cfg_path),
            "port": str(port),
        }
    )

    try:
        subprocess.Popen(
            ["bash", Path("src/r-job-trigger.sh").resolve()],
            env=subprocess_env,
        )
    except Exception as e:
        print(f"Error starting shell process: {e}")

    client, _ = server.accept()
    client.sendall(json.dumps(input_param).encode())
    results_dict = json.loads(read_json_con(socket=client))

    return results_dict

def _find_and_connect(server_wait_tol: int = 1) -> socket.socket:
    cont = True
    while cont:
        try:
            port = _find_available_port()

            # Create a socket
            server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

            # Bind the socket to a specific address and port
            server_address = ("localhost", port)
            server.bind(server_address)
            cont = False
        except OSError as e:
            if e.errno == 98:
                print(
                    f"Port {port} is already in use. Retrying in {server_wait_tol} seconds..."
                )
                time.sleep(server_wait_tol)
            else:
                cont = False  # Exit the loop and raise the exception
                raise

    # Listen for incoming connections
    server.listen(1)

    return server, port

def _find_available_port(print_port=False):
    # Function to find an available port

    # Create a socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Bind to a random port
    s.bind(("localhost", 0))

    # Get the actual port number
    _, port = s.getsockname()

    # Close the socket
    s.close()

    if print_port:
        print(port)

    return port

def read_json_con(socket, max_length: int = 256):
    json_string = ""

    while True:
        new_piece = socket.recv(max_length).decode("utf-8")
        json_string += new_piece
        if len(new_piece) < max_length:
            break

    return json_string

def dict_to_filename(config):
    # Create key-value pairs and replace periods with underscores
    items = [f"{key}-{str(value).replace('.', '_')}" for key, value in config.items()]
    # Join the items with an underscore
    filename = '_'.join(items)
    # Sanitize the filename (this step ensures that the filename is valid)
    sanitized_filename = re.sub(r'[^a-zA-Z0-9_-]', '_', filename)
    return sanitized_filename

def dict_to_path(config):
    # Extract the first two keys for the folder structure
    keys_to_filter = {'replication', 'num_queues'}
    folder_keys = [key for key in list(config.keys()) if key not in keys_to_filter]
    # Create the folder path
    folder_path = []
    for key in folder_keys:
        value = config[key]
        folder_path.append(f"{key}-{str(value).replace('.', '_')}")
    
    # Join folder path components with OS-specific separator
    folder_path = os.path.join(*folder_path)

    # Sanitize the full path (optional, can be skipped if not needed)
    sanitized_path = re.sub(r'[^a-zA-Z0-9_/.-]', '_', folder_path)
    
    return sanitized_path