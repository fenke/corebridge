# AUTOGENERATED! DO NOT EDIT! File to edit: ../nbs/02_rscriptbridge.ipynb.

# %% auto 0
__all__ = ['get_asset_path', 'get_save_path', 'install_R_package', 'calc_hash_from_flowobject', 'calc_hash_from_input_files',
           'check_script_inputs', 'check_script_output', 'generate_checksum_file', 'run_script']

# %% ../nbs/02_rscriptbridge.ipynb 4
import json, os, fcntl, time
import subprocess
import hashlib

from functools import reduce

# %% ../nbs/02_rscriptbridge.ipynb 7
def get_asset_path(script_name, assets_dir:str): 
    return os.path.join(assets_dir, script_name)
def get_save_path(datafile_name, save_dir): 
    return os.path.join(save_dir, datafile_name)


# %% ../nbs/02_rscriptbridge.ipynb 31
def install_R_package(pkg:str|list):
    """
    Checks and if neccesary installs an R package

    Parameters
    ----------
    pkg : str|list
        name(s) of the package(s)
    """

    if isinstance(pkg, str):
        pkg = [pkg]

    for pkg_i in pkg:
        run_script_result = subprocess.run(['Rscript','-e', f"library({pkg_i})"], capture_output=True)
        if run_script_result.returncode != 0:
            print(f"Installing {pkg_i}")
            run_script_result = subprocess.run(['Rscript','-e', f"install.packages({pkg_i}, repos='https://cloud.r-project.org')"], capture_output=True)
        else:
            print(f"Library {pkg_i} already installed")
            
        print(run_script_result.stderr.decode('UTF-8'))



# %% ../nbs/02_rscriptbridge.ipynb 46
def calc_hash_from_flowobject(flow_object:dict)->str:
    return hashlib.md5(repr(flow_object).encode('UTF-8')).hexdigest()

def calc_hash_from_input_files(flow_object:dict, save_dir:str)->str:
    hashobj = hashlib.md5()

    # iterate over files in input_files
    for input_file in flow_object['in']:
        with open(os.path.join(save_dir, input_file), 'rb') as f:
            # loop till the end of the file
            chunk = 0
            while chunk != b'':
                # read only 1024 bytes at a time
                chunk = f.read(1024)
                hashobj.update(chunk)

    return hashobj.hexdigest()

# %% ../nbs/02_rscriptbridge.ipynb 50
def check_script_inputs(flow_object:dict, save_dir:str)->bool:
    """ 
    Check if the input files for a script are up-to-date, returns True if up-to-date.
    """

    checksum_file = get_save_path(f"input-checksum-{calc_hash_from_flowobject(flow_object)}", save_dir)
    md5_check_result = subprocess.run(
        ['md5sum', '-c', checksum_file], 
        cwd=save_dir,
        capture_output=True)
    
    return int(md5_check_result.returncode) == 0

# %% ../nbs/02_rscriptbridge.ipynb 53
def check_script_output(flow_object:dict, save_dir:str)->bool:
    """ 
    Check if the output files for a script exist, returns True if they all exist.
    """

    return all([
        os.path.isfile(get_save_path(F, save_dir)) 
        for F in flow_object['out']
    ])

# %% ../nbs/02_rscriptbridge.ipynb 56
def generate_checksum_file(flow_object:dict, save_dir:str)->bool:
    """Generates the checksum file for a given flow object"""

    input_files = flow_object['in']
    md5_encode_result = subprocess.run(
        ['md5sum','-b']+
        input_files, 
        cwd=save_dir,
        capture_output=True)
    
    checksum_file = get_save_path(f"input-checksum-{calc_hash_from_flowobject(flow_object)}", save_dir)
    with open(checksum_file, 'wt') as cf:
        cf.write(md5_encode_result.stdout.decode('UTF-8'))

    return md5_encode_result.returncode == 0 and check_script_inputs(flow_object, save_dir)

# %% ../nbs/02_rscriptbridge.ipynb 65
def run_script(flow_object, assets_dir:str, save_dir:str):
    """ Run a script in R 
        args:
            flow_object: dict of flow object
        returns:
            bool: True if a follow-up script might need to be run, False if not

    """
    print(f"Running script {flow_object['name']}")
    # Check if output exists and inputs have not changed and return False if 
    # output exists and inputs have not changed
    if check_script_output(flow_object, save_dir) and check_script_inputs(flow_object, save_dir):
        return True
    
    # Create the lock file
    lock_file = get_save_path(f"lock-{calc_hash_from_input_files(flow_object, save_dir)}", save_dir)
    with open(lock_file, 'wt') as cf:
        try:
            print(f"Locking {lock_file}")
            # Get exclusive lock on the file
            fcntl.flock(cf, fcntl.LOCK_EX | fcntl.LOCK_NB)

            # run the script
            run_script_result = subprocess.run(
                ['Rscript', '--vanilla', get_asset_path(flow_object['name'], assets_dir)],
                cwd=save_dir,
                capture_output=True
            )
            
            # check the return code
            if run_script_result.returncode:
                cf.write(f"Run returned code {run_script_result.returncode}\n")
                cf.write('STDOUT------------\n', run_script_result.stdout.decode('UTF-8'),'\n')
                cf.write('STDERR------------\n', run_script_result.stderr.decode('UTF-8'),'\n')
                return False

        except BlockingIOError as locked_error:
            print(locked_error)
            return False

    
    # check the output and generate the checksum file
    return check_script_output(flow_object, save_dir) and generate_checksum_file(flow_object, save_dir)
    
