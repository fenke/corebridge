# AUTOGENERATED! DO NOT EDIT! File to edit: ../nbs/02_rscriptbridge.ipynb.

# %% auto 0
__all__ = ['syslog', 'read_chunk_size', 'RScriptProcess', 'get_asset_path', 'get_rscript_libpath', 'get_save_path',
           'get_rscript_env', 'check_rscript_libs', 'check_rscript_lib', 'install_R_package_wait', 'unpack_assets',
           'calc_hash_from_flowobject', 'calc_hash_from_files', 'calc_hash_from_input_files',
           'calc_hash_from_data_files', 'check_script_inputs', 'check_script_output', 'generate_checksum_file',
           'run_rscript_wait', 'run_rscript_nowait', 'release_script_lock', 'AICoreRScriptModule',
           'snake_case_to_camel_case', 'recursive_flatten_nested_data']

# %% ../nbs/02_rscriptbridge.ipynb 5
import os, logging, json, hashlib
import typing,fcntl, subprocess
import traceback
import pandas as pd, numpy as np, rdata

from functools import reduce

from collections import namedtuple
from fastcore.basics import patch_to, patch

import corebridge
from .core import *


# %% ../nbs/02_rscriptbridge.ipynb 7
syslog = logging.getLogger(__name__)

# %% ../nbs/02_rscriptbridge.ipynb 10
def get_asset_path(script_name, assets_dir:str): 
    return os.path.join(assets_dir, script_name)
def get_rscript_libpath(save_dir:str):
    return os.path.join(save_dir, 'libs')
def get_save_path(datafile_name:str, save_dir:str): 
    return os.path.join(save_dir, datafile_name)


# %% ../nbs/02_rscriptbridge.ipynb 43
def get_rscript_env(libfolder:str):
    if os.environ.get('R_LIBS_USER'):
        return dict(**os.environ)
    else:
        return dict(**os.environ, R_LIBS_USER=str(libfolder))

# %% ../nbs/02_rscriptbridge.ipynb 50
def check_rscript_libs(libs:list, libfolder:str):
    """Quick check if for all the R packages in libs a folder exists in libfolder"""
    return all([os.path.exists(os.path.join(libfolder, L)) for L in libs])

def check_rscript_lib(lib:str, libfolder:str) -> bool:
    """Checks if a R package is installed in libfolder

    Parameters
    ----------
    lib : str
        name of the package
    libfolder : str
        path to the library folder

    Returns
    -------
    bool
        True if the package is installed, False otherwise
    """

    run_script_result = subprocess.run(['Rscript','-e', f"library({lib})"], env=get_rscript_env(libfolder), capture_output=True)
    if run_script_result.returncode != 0:
        print('STDERR\n', run_script_result.stderr.decode('UTF-8'))
        print('STDOUT\n', run_script_result.stdout.decode('UTF-8'))
    return run_script_result.returncode == 0

# %% ../nbs/02_rscriptbridge.ipynb 56
def install_R_package_wait(pkg:str|list, workdir:str, repo='https://cloud.r-project.org'):
    """
    Checks and if neccesary installs an R package

    Parameters
    ----------
    pkg : str|list
        name(s) of the package(s)
    """

    if isinstance(pkg, str):
        return install_R_package_wait([pkg], libfolder, repo)
    
    libfolder=os.path.join(workdir, 'libs')
    os.makedirs(libfolder, exist_ok=True)
    syslog.debug(f"Using libfolder {libfolder} for packages")
    
    env = dict(os.environ)
    env['R_LIBS_USER'] = os.path.abspath(libfolder) 
    syslog.debug(F"Using libfolder {env['R_LIBS_USER']} for R_LIBS_USER")

    
    for pkg_i in pkg: # ['generics', 'timechange', 'rlang', 'stringi'] + 
        print(f"\nInstalling package {pkg_i}, testing attach ...")
        if not check_rscript_lib(pkg_i, libfolder):
            print(f"Package {pkg_i} not attached. Installing {pkg_i}")
            run_script_install = subprocess.run([
                    'Rscript','-e', 
                    f"install.packages('{pkg_i}', repos='{repo}', lib='{libfolder}', dependencies=TRUE)"
                ], capture_output=True, env=env)
            
            if run_script_install.returncode != 0:
                print(f"installing {pkg_i}, returned code {run_script_install.returncode} ... ")
                print('STDOUT--------------\n', run_script_install.stdout.decode('UTF-8'))
                print('STDERR--------------\n', run_script_install.stderr.decode('UTF-8'))

            elif not check_rscript_lib(pkg_i, libfolder): # not in cache
                print(f"Attach after installing for {pkg_i} failed ... install logs below")
                print('STDOUT--------------\n', run_script_install.stdout.decode('UTF-8'))
                print('STDERR--------------\n', run_script_install.stderr.decode('UTF-8'))
            else:
                print(f"Attach after installation was successful. Library {pkg_i} appears to have been installed")

        else:
            print(f"Attach successful. Library {pkg_i} appears to have been installed")
            



# %% ../nbs/02_rscriptbridge.ipynb 62
def unpack_assets(assets_dir:str, save_dir:str):
    """
    Unpack the assets folder to the save_dir
    """
    unpack_result = subprocess.Popen(
        ['unzip', '-un', '-d', save_dir, os.path.join(assets_dir, '*.zip')],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )
    return unpack_result

# %% ../nbs/02_rscriptbridge.ipynb 79
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
    return calc_hash_from_files(list(flow_object['in'].values()), save_dir)

def calc_hash_from_data_files(flow_object:dict, save_dir:str)->str:
    '''Calculate hash from the contents of the input files for a given flow object'''
    return calc_hash_from_files(list(flow_object['in'].values()) + list(flow_object['out'].values()), save_dir)


# %% ../nbs/02_rscriptbridge.ipynb 84
def check_script_inputs(flow_object:dict, workdir:str)->bool:
    """ 
    Check if the input files for a script are up-to-date, returns True if up-to-date.
    """
    checksum_filename = f"input-checksum-{calc_hash_from_flowobject(flow_object)}"
    md5_check_result = subprocess.run(
        ['md5sum', '-c', checksum_filename], 
        cwd=workdir,
        capture_output=True)
    syslog.debug(f"Checksum check result for Flow object: {flow_object['name']}: {md5_check_result.returncode}, checksum file: {checksum_filename}")
    
    return int(md5_check_result.returncode) == 0

# %% ../nbs/02_rscriptbridge.ipynb 87
def check_script_output(flow_object:dict, workdir:str)->bool:
    """ 
    Check if the output files for a script exist, returns True if they all exist.
    """
    files_exist = [
        os.path.isfile(get_save_path(F, workdir)) 
        for F in flow_object['out'].values()
    ]
    syslog.debug(f"Output files for Flow object: {flow_object['name']}: {list(zip(flow_object['out'], files_exist))}")
    return all(files_exist)

# %% ../nbs/02_rscriptbridge.ipynb 90
def generate_checksum_file(flow_object:dict, workdir:str)->bool:
    """Generates the checksum file for a given flow object"""

    input_files = list(flow_object['in'].values())
    md5_encode_result = subprocess.run(
        ['md5sum','-b']+
        input_files, 
        cwd=workdir,
        capture_output=True)
    
    checksum_filename = f"input-checksum-{calc_hash_from_flowobject(flow_object)}"
    syslog.debug(f"Checksum file for Flow object: {flow_object['name']} created return {md5_encode_result.returncode}, checksum file: {checksum_filename}")
    with open(os.path.join(workdir, checksum_filename), 'wt') as cf:
        cf.write(md5_encode_result.stdout.decode('UTF-8'))

    return md5_encode_result.returncode == 0 and check_script_inputs(flow_object, workdir)

# %% ../nbs/02_rscriptbridge.ipynb 99
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
                cf.write(f"STDOUT------------\n{run_script_result.stdout.decode('UTF-8')}\n")
                cf.write(f"STDERR------------\n{run_script_result.stderr.decode('UTF-8')}\n")
                return False

        except BlockingIOError as locked_error:
            syslog.debug(locked_error)
            return False

    
    # check the output and generate the checksum file
    return check_script_output(flow_object, save_dir) and generate_checksum_file(flow_object, save_dir)
    

# %% ../nbs/02_rscriptbridge.ipynb 106
RScriptProcess = namedtuple('RScriptProcess', ['flow_object', 'lock_file', 'stdout','stderr', 'popen_args', 'popen'])

#### Asynchronous RScript processing ------------------------------------------------

def run_rscript_nowait(
        flow_object, 
        workdir:str, 
        libfolder:str=None,
        pkg_repo:str='https://cloud.r-project.org') -> RScriptProcess:
    
    """ Run a script in R 
        args:
            flow_object: dict of flow object
            workdir: working directory
            pkg_repo: CRAN package repository
        returns:
            RScriptProcess: Popen container object for the script
    """
    
    syslog.debug(f"Starting rscript for {flow_object['name']}")

    # lockfile -------------------------------------------------------------------
    os.makedirs(os.path.abspath(os.path.join(workdir, 'temp')), exist_ok=True)
    def get_temp_path(lname):
        return os.path.abspath(os.path.join(workdir, 'temp', lname))
    
    lock_name = 'run_flow_object-'+calc_hash_from_flowobject(flow_object)

    # lock maintenance
    if run_rscript_nowait.lock_objects.get(lock_name): 
        lock_object = run_rscript_nowait.lock_objects[lock_name]
        if not lock_object.lock_file.closed:
            syslog.debug(f"Lockfile is open for {flow_object['name']} ({lock_name})")
            # If the lockfile is open, check if the process is still running
            
            if lock_object.popen is None:
                syslog.debug(f"No process running for {flow_object['name']} ({lock_name})")
            elif lock_object.popen.poll() is None:
                syslog.debug(f"Script is still running for {flow_object['name']} ({lock_name})")
                return lock_object
            else:
                syslog.debug(f"Script has finished for {flow_object['name']} ({lock_name}), returned {lock_object.popen.returncode}")
                # since poll return not-None the script has finished so close the lockfile
                lock_object.lock_file.close()
                lock_object.stdout.close()
                lock_object.stderr.close()
                if lock_object.popen.returncode != 0:
                    syslog.error(f"Script failed for {flow_object['name']} ({lock_name}), returned {lock_object.popen.returncode}")
                    syslog.error(f"Args were: {lock_object.popen_args}")
                    with open(lock_object.stdout.name, 'rb') as so:
                        syslog.error(f"STDOUT\n{so.read().decode('UTF-8')}")
                    with open(lock_object.stderr.name, 'rb') as se:
                        syslog.error(f"STDERR\n{se.read().decode('UTF-8')}")
                else:
                    syslog.debug(f"Script was successful for {flow_object['name']} ({lock_name})")
                    generate_checksum_file(flow_object, os.path.abspath(workdir))

                #os.remove(lock_object.stdout.name)
                #os.remove(lock_object.stderr.name)


    # Check if output exists and inputs have not changed and return False if 
    # output exists and inputs have not changed
    if check_script_output(flow_object, os.path.abspath(workdir)) and check_script_inputs(flow_object, os.path.abspath(workdir)):
        syslog.debug(f"Output and inputs are up-to-date for {flow_object['name']}")
        return run_rscript_nowait.lock_objects.get(lock_name)

    if not all([os.path.exists(get_save_path(fname, os.path.abspath(workdir))) for fname in flow_object['in'].values()]):
        syslog.debug(f"Inputs missing for {flow_object['name']}")
        return run_rscript_nowait.lock_objects.get(lock_name)
    # Create the lock file -----------------------------------------------------------
    syslog.debug(f"Preparing to run scripts for {flow_object['name']}, creating lockfile ({lock_name})")
    cf = open(get_temp_path(f"lock-{lock_name}"), 'wt')
    
    try:
        # Set lock on lockfile
        fcntl.flock(cf, fcntl.LOCK_EX | fcntl.LOCK_NB)

        so = open(get_temp_path(f"stdout-{lock_name}"), 'wt')
        se = open(get_temp_path(f"stderr-{lock_name}"), 'wt')

        # check libs
        if not libfolder:
            libfolder=os.path.abspath(os.path.join(workdir, 'libs'))
            
        os.makedirs(libfolder, exist_ok=True)
        syslog.debug(f"Using libfolder {libfolder} for packages")
        
        env = dict(os.environ)
        env['R_LIBS_USER'] = libfolder
        syslog.debug(F"Using libfolder {env['R_LIBS_USER']} for R_LIBS_USER")
        
        if not check_rscript_libs(flow_object['libs'], libfolder):
            for pkg_i in flow_object['libs']:
                syslog.debug(f"Checking lib {pkg_i} for {flow_object['name']} ({lock_name})")
                if not check_rscript_lib(pkg_i, libfolder):
                    syslog.debug(f"Starting installation of {pkg_i} for {flow_object['name']} ({lock_name})")
                    popen_args = [
                            'Rscript','-e', 
                            f"install.packages('{pkg_i}', repos='{pkg_repo}', lib='{libfolder}', dependencies=TRUE)",
                        ]
                    run_script_install = subprocess.Popen(
                        popen_args, 
                        cwd=os.path.abspath(workdir),
                        stdout=so,
                        stderr=se,
                        encoding='UTF-8',
                        env=env,
                    )
                    run_rscript_nowait.lock_objects[lock_name] =  RScriptProcess(flow_object, cf, so, se, popen_args, run_script_install)
                    return run_rscript_nowait.lock_objects.get(lock_name)
                    
        
        syslog.debug(f"Libs are up-to-date, starting script for {flow_object['name']} ({lock_name})")
        # run the script
        popen_args = ['Rscript', flow_object['name']]
        popen_run = subprocess.Popen(
            popen_args,
            cwd=os.path.abspath(workdir),
            stdout=so,
            stderr=se,
            encoding='UTF-8',
            env=env,
        )

        run_rscript_nowait.lock_objects[lock_name] =  RScriptProcess(flow_object, cf, so, se, popen_args, popen_run)
            
    except BlockingIOError as locked_error:
        cf.close()
        #syslog.error(f"{flow_object['name']} is locked, cannot run", exc_info=locked_error)

    syslog.debug(f"Done with {flow_object['name']}.")

    return run_rscript_nowait.lock_objects.get(lock_name)

run_rscript_nowait.lock_objects = {}

# %% ../nbs/02_rscriptbridge.ipynb 107
def release_script_lock(flow_object, save_dir):
    process = run_rscript_nowait.lock_objects.get(flow_object['name'])
    if process.popen and process.popen.poll() is not None:
        syslog.debug(f"Closing lockfile {process.lock_file.name}")
        process.lock_file.close()

# %% ../nbs/02_rscriptbridge.ipynb 113
class AICoreRScriptModule(AICoreModuleBase):
    def __init__(self, 
                flow_mapping:dict, # scripts flow map
                save_dir:str, # path where the module can keep files 
                assets_dir:str, # path to support files (scripts, metadata, etc)
                cran_repo:str='https://cloud.r-project.org', # CRAN repo
                *args, **kwargs):
        
        super().__init__(save_dir, assets_dir, *args, **kwargs)
    
        self.corebridge_version = corebridge.__version__

        self.flow_mapping = flow_mapping
        self.cran_repo = cran_repo

        self.data_files_map = {
            D:F
            for P in self.flow_mapping.values()
            for D,F in P['in'].items()
        }

        for N in self.data_files_map.keys():
            print(N, os.path.isfile(self.get_save_path(self.data_files_map.get(N))), self.get_save_path(self.data_files_map.get(N)))
        # list assets
        print('Assets:\n', subprocess.run(['ls', '-la', assets_dir], capture_output=True).stdout.decode('UTF-8'))

        self.unpack_result = unpack_assets(assets_dir, self.get_rscript_workdir())
        # list working directory
        #print('Working directory:\n', subprocess.run(['ls', '-l', '*.R', save_dir], capture_output=True).stdout.decode('UTF-8'))

        self.flow_results = {
            flow_object['name']:run_rscript_nowait(
                flow_object, 
                workdir=self.get_rscript_workdir(),
                libfolder=self.get_rscript_libpath(),
                pkg_repo=self.cran_repo
            )
            for flow_object in self.flow_mapping.values()
        }

        self.update_flow()
        
        syslog.info(f"RScriptModule initialized with {len(flow_mapping)} flow objects.")

    # def get_asset_path(self,script_name): 
    #     return os.path.abspath(os.path.join(self.init_kwargs['assets_dir'], script_name))
    def get_rscript_libpath(self):
        return os.path.abspath(os.path.join(self.init_kwargs['save_dir'], 'libs'))
    def get_rscript_workdir(self):
        return os.path.abspath(os.path.join(self.init_kwargs['save_dir'], 'workdir'))
    def get_save_path(self, datafile_name:str): 
        return os.path.abspath(os.path.join(self.init_kwargs['save_dir'], 'workdir', datafile_name))
    
    def get_flow_status(self):
        return [
            f"process {name} for {process.flow_object['name']} pollstatus: {process.popen.poll()}, args: {process.popen_args}"
            for name, process in self.flow_results.items()
            if process and process.popen
        ]
        

    

# %% ../nbs/02_rscriptbridge.ipynb 115
@patch
def update_flow(self:AICoreRScriptModule):
    workdir = self.get_rscript_workdir()
    libfolder = self.get_rscript_libpath()

    for flow_object in self.flow_mapping.values():
        
        syslog.debug(f"Update {flow_object['name']}, output: {check_script_output(flow_object, os.path.abspath(workdir))}, inputs: {check_script_inputs(flow_object, os.path.abspath(workdir))}")
        if (
            not check_script_output(flow_object, os.path.abspath(workdir)) 
            or not check_script_inputs(flow_object, os.path.abspath(workdir))
        ):
            if self.flow_results[flow_object['name']]: 
                process = self.flow_results[flow_object['name']]
                if process.popen.poll() is None:
                    syslog.debug(f"Process is still running: {flow_object['name']}, args: {process.popen_args}")
                    return self.get_flow_status()
                else:
                    syslog.debug(f"Process finished: {flow_object['name']}, args: {process.popen_args}, returncode: {process.popen.poll()}")
                

            syslog.debug(f"Updating for {flow_object['name']}, starting at {workdir}")

            self.flow_results[flow_object['name']] = run_rscript_nowait(
                flow_object, 
                workdir=workdir, 
                libfolder=libfolder,
                pkg_repo=self.cran_repo
            )

    syslog.info(f"RScriptModule flow update complete.")
    return self.get_flow_status()


# %% ../nbs/02_rscriptbridge.ipynb 126
def snake_case_to_camel_case(snake_case:str) -> str:
    splittext = snake_case.split('_')
    return ''.join([x.capitalize() if n > 0 else x for x,n in zip(splittext, range(len(splittext)))])

def recursive_flatten_nested_data(
        data:dict, 
        column_prefix:str='',
        camel_case=False) -> dict:
    
    if isinstance(data, np.ndarray):
        return {column_prefix:data}
    
    if isinstance(data, list):
        return reduce(
            lambda R, X: dict(**R, **X) if R else X,
            [
                recursive_flatten_nested_data(value, f"{column_prefix}_{i+1}_", camel_case)
                for i, value in enumerate(data)
             
            ],
            {}

        )
    
    if isinstance(data, dict):
        
        #if len(data.keys()) == 0:
        #    return data
        if len(data.keys()) > 1:
            return reduce(
                lambda R, X: dict(**R, **X) if R else X,
                [
                    recursive_flatten_nested_data(
                        value, 
                        snake_case_to_camel_case(column_prefix+'_'+str(key)) if camel_case else column_prefix+'_'+str(key),
                        camel_case
                    )
                    for key, value in data.items()
                ],
                {}
                
            )
        else:
            key = list(data.keys())[0]
            value = data[key]
            if column_prefix:
                column_name = snake_case_to_camel_case(column_prefix+'_'+str(key)) if camel_case else column_prefix+'_'+str(key)
            else:
                column_name = snake_case_to_camel_case(str(key)) if camel_case else str(key)
            return recursive_flatten_nested_data(
                value, column_name, camel_case
            )
                

# %% ../nbs/02_rscriptbridge.ipynb 130
@patch
def write_uploaded_data(
    self:AICoreRScriptModule, 
    df:pd.DataFrame, 
    tag:str=None,
    **kwargs):

    csv_filename = self.get_save_path(self.data_files_map.get(tag, tag))
    syslog.debug(f"Writing {df.shape[0]} rows to {csv_filename}")

    df.reset_index().to_csv(csv_filename, index=False, date_format='%Y-%m-%d %H:%M:%S')

@patch
def read_data(self:AICoreRScriptModule, tag:str=None, camel_case=False, **kwargs):
    
    rdata_filename = self.get_save_path(self.data_files_map.get(tag, tag))
    converted = rdata.read_rda(rdata_filename)

    flattened = recursive_flatten_nested_data(converted, camel_case=camel_case)
    df = pd.DataFrame(flattened)
    syslog.debug(f"Read {df.shape[0]} rows from {rdata_filename} for {tag} (camel_case={camel_case})")

    time_column = [k for k,v in df.dtypes.to_dict().items() if 'float' not in str(v)][0]
    df.set_index( pd.DatetimeIndex(df[time_column]), inplace=True)
    df.index.name = 'time'
    df.drop(time_column, axis=1, inplace=True)

    return df
                                        


@patch
def infer(
    self:AICoreRScriptModule, 
    data:dict, 
    *_, 
    **kwargs):

    """ 
    Infer method for the RScriptModule
    """

    try:

        msg=[
            f"Startup time: {self.init_time.isoformat()}",
            f"Corebridge version: {self.corebridge_version}",
            f"init_args: {self.init_args}, init_kwargs: {self.init_kwargs}",
        ]
        
        msg += self.update_flow()
        # Pickup params, pop those that are not intended for the processor
        writeTag = kwargs.pop('writeTag', None)
        readTag = kwargs.pop('readTag', None)
        camelCase = bool(kwargs.pop('camelCase', False))
        msg.append(f"writeTag: {writeTag}, readTag: {readTag}, camelCase: {camelCase}")

        lastSeen = kwargs.pop('lastSeen', False)
        recordformat = kwargs.pop('format', "records").lower()
        timezone = kwargs.get('timezone', 'UTC')
        msg.append(f"lastSeen: {lastSeen}, recordformat: {recordformat}, timezone: {timezone}")

        reversed = kwargs.pop('reversed', False)

        if writeTag:

            df = set_time_index_zone(timeseries_dataframe_from_datadict(
            data, ['datetimeMeasure', 'time'], recordformat), timezone)

            df.sort_index(inplace=True)

            syslog.debug(f"Writing {df.shape[0]} rows to {writeTag}")
            self.write_uploaded_data(df, writeTag)

        if readTag:
            result = self.read_data(readTag, camel_case=camelCase)

            if reversed:
                result = result[::-1]

            syslog.debug(f"Read {result.shape[0]} rows from {readTag}")

            return {
                'msg':msg,
                'data': timeseries_dataframe_to_datadict(
                    result if not lastSeen else result[-1:],
                    recordformat=recordformat,
                    timezone=timezone,
                    popNaN=True)
            }
        
        return {
            'msg':msg + self.get_flow_status(),
            'data': []
        }

    except Exception as err:
        msg.append(''.join(traceback.format_exception(None, err, err.__traceback__)))
        syslog.exception(f"Exception {str(err)} in infer()")
        return {
            'msg': f"Unexpected {err=}, {type(err)=}",
            'data': []
        }


