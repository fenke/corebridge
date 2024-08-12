# AUTOGENERATED! DO NOT EDIT! File to edit: ../nbs/02_rscriptbridge.ipynb.

# %% auto 0
__all__ = ['syslog', 'read_chunk_size', 'RScriptProcess', 'get_asset_path', 'get_save_path', 'install_R_package',
           'calc_hash_from_flowobject', 'calc_hash_from_files', 'calc_hash_from_input_files',
           'calc_hash_from_data_files', 'check_script_inputs', 'check_script_output', 'generate_checksum_file',
           'run_rscript_wait', 'run_rscript_nowait']

# %% ../nbs/02_rscriptbridge.ipynb 5
import json, os, fcntl, logging
import subprocess
import hashlib
from collections import namedtuple

from .aicorebridge import AICoreModule

# %% ../nbs/02_rscriptbridge.ipynb 7
syslog = logging.getLogger(__name__)

# %% ../nbs/02_rscriptbridge.ipynb 9
def get_asset_path(script_name, assets_dir:str): 
    return os.path.join(assets_dir, script_name)
def get_save_path(datafile_name, save_dir): 
    return os.path.join(save_dir, datafile_name)


# %% ../nbs/02_rscriptbridge.ipynb 37
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
        print(f"Installing package {pkg_i} ...")
        run_script_result = subprocess.run(['Rscript','-e', f"library({pkg_i})"], capture_output=True)
        if run_script_result.returncode != 0:
            print(f"Installing {pkg_i}")
            run_script_result = subprocess.run(['Rscript','-e', f"install.packages({pkg_i}, repos='https://cloud.r-project.org')"], capture_output=True)
        else:
            print(f"Library {pkg_i} already installed")
            
        print(run_script_result.stderr.decode('UTF-8'))



# %% ../nbs/02_rscriptbridge.ipynb 52
read_chunk_size = 1024 * 32
def calc_hash_from_flowobject(flow_object:dict)->str:
    '''Calculate a unique hash for a given flow object'''
    return hashlib.md5(repr(flow_object).encode('UTF-8')).hexdigest()

def calc_hash_from_files(files:list, save_dir:str)->str:
    '''Calculate hash from the contents of the input files'''
    hashobj = hashlib.md5()

    # iterate over files 
    for data_file in files:
        full_name = os.path.join(save_dir, data_file)
        if not os.path.isfile(full_name):
            continue
        
        with open(full_name, 'rb') as f:
            # loop till the end of the file
            while True:
                # read only 1024 bytes at a time
                chunk = f.read(read_chunk_size)
                if not chunk:
                    break
                
                hashobj.update(chunk)
        
    return hashobj.hexdigest()

def calc_hash_from_input_files(flow_object:dict, save_dir:str)->str:
    '''Calculate hash from the contents of the input files for a given flow object'''
    return calc_hash_from_files(flow_object['in'], save_dir)

def calc_hash_from_data_files(flow_object:dict, save_dir:str)->str:
    '''Calculate hash from the contents of the input files for a given flow object'''
    return calc_hash_from_files(flow_object['in'] + flow_object['out'], save_dir)


# %% ../nbs/02_rscriptbridge.ipynb 57
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

# %% ../nbs/02_rscriptbridge.ipynb 60
def check_script_output(flow_object:dict, save_dir:str)->bool:
    """ 
    Check if the output files for a script exist, returns True if they all exist.
    """

    return all([
        os.path.isfile(get_save_path(F, save_dir)) 
        for F in flow_object['out']
    ])

# %% ../nbs/02_rscriptbridge.ipynb 63
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

# %% ../nbs/02_rscriptbridge.ipynb 72
def run_rscript_wait(flow_object, assets_dir:str, save_dir:str):
    """ Run a script in R 
        args:
            flow_object: dict of flow object
        returns:
            bool: True if a follow-up script might need to be run, False if not

    """
    syslog.debug(f"Running script {flow_object['name']}")
    # Check if output exists and inputs have not changed and return False if 
    # output exists and inputs have not changed
    if check_script_output(flow_object, save_dir) and check_script_inputs(flow_object, save_dir):
        return True
    
    # Create the lock file
    lock_file = get_save_path(f"lock-{calc_hash_from_flowobject(flow_object)}", save_dir)
    with open(lock_file, 'wt') as cf:
        try:
            syslog.debug(f"Locking {lock_file}")
            # Get exclusive lock on the file, is released on file close
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
            syslog.debug(locked_error)
            return False

    
    # check the output and generate the checksum file
    return check_script_output(flow_object, save_dir) and generate_checksum_file(flow_object, save_dir)
    

# %% ../nbs/02_rscriptbridge.ipynb 80
RScriptProcess = namedtuple('RScriptProcess', ['lock_file', 'popen'])

def run_rscript_nowait(flow_object, assets_dir:str, save_dir:str) -> RScriptProcess:
    """ Run a script in R 
        args:
            flow_object: dict of flow object
            assets_dir: path to the assets directory
            save_dir: path to the save directory
        returns:
            RScriptProcess: Popen container object for the script
    """
    
    syslog.debug(f"Starting script {flow_object['name']}")

    # check lockfile ---------------------------------------------------------------
    lock_name = calc_hash_from_flowobject(flow_object)
    if run_rscript_nowait.lock_objects.get(lock_name): 
        lock_object = run_rscript_nowait.lock_objects[lock_name]
        if not lock_object.lock_file.closed:
            syslog.debug(f"Lockfile is open for {flow_object['name']} ({lock_name})")
            
            if lock_object.popen is not None:
                syslog.debug(f"No process running for {flow_object['name']} ({lock_name})")
            elif lock_object.popen.poll() is None:
                syslog.debug(f"Script is still running for {flow_object['name']} ({lock_name})")
                return lock_object
            else:
                syslog.debug(f"Script has finished for {flow_object['name']} ({lock_name})")
                # since poll return not-None the script has finished so close the lockfile
                lock_object.lock_file.close()


    # Check if output exists and inputs have not changed and return False if 
    # output exists and inputs have not changed
    if check_script_output(flow_object, save_dir) and check_script_inputs(flow_object, save_dir):
        syslog.debug(f"Output and inputs are up-to-date for {flow_object['name']}")
        return run_rscript_nowait.lock_objects.get(lock_name)

    # Create the lock file -----------------------------------------------------------
    syslog.debug(f"Creating lockfile for {flow_object['name']} ({lock_name})")
    cf = open(get_save_path(f"lock-{lock_name}", save_dir), 'wt')
    
    try:
        # Set lock on lockfile
        fcntl.flock(cf, fcntl.LOCK_EX | fcntl.LOCK_NB)

        # run the script
        popen = subprocess.Popen(
            ['Rscript', '--vanilla', get_asset_path(flow_object['name'], assets_dir)],
            cwd=save_dir,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding='UTF-8',
        )

        run_rscript_nowait.lock_objects[lock_name] =  RScriptProcess(cf, popen)
            
    except BlockingIOError as locked_error:
        cf.close()
        syslog.error(locked_error)

    syslog.debug(f"Done with {flow_object['name']}")

    return run_rscript_nowait.lock_objects.get(lock_name)

run_rscript_nowait.lock_objects = {}
