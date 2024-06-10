# AUTOGENERATED! DO NOT EDIT! File to edit: ../nbs/01_aicorebridge.ipynb.

# %% auto 0
__all__ = ['syslog', 'AICoreModule']

# %% ../nbs/01_aicorebridge.ipynb 4
import typing
import logging
import traceback
import inspect
import datetime
import json
import pandas as pd
import numpy as np

from fastcore.basics import patch_to, patch
from .core import *
from . import __version__


# %% ../nbs/01_aicorebridge.ipynb 5
syslog = logging.getLogger(__name__)

# %% ../nbs/01_aicorebridge.ipynb 6
try:
    print(f"Loading {__name__} {__version__} from {__file__}")
except:
    pass

# %% ../nbs/01_aicorebridge.ipynb 7
class AICoreModule(): pass

# %% ../nbs/01_aicorebridge.ipynb 8
@patch
def __init__(self:AICoreModule, 
             processor:typing.Callable, # data processing function
             save_dir:str, # path where the module can keep files 
             assets_dir:str,
             *args, **kwargs):
    
    self.init_time = datetime.datetime.now(datetime.UTC)
    self._init_processor(processor)

    self.init_args = args
    self.init_kwargs = dict(
        **kwargs,
        assets_dir=assets_dir,
        save_dir=save_dir
    )



# %% ../nbs/01_aicorebridge.ipynb 9
@patch
def _init_processor(
        self:AICoreModule, 
        processor:typing.Callable):
    """Initializes processor related variables on self"""
    
    self.processor = processor
    self.processor_signature = inspect.signature(self.processor)
    self.processor_params = dict(self.processor_signature.parameters)
    self.return_param = self.processor_params.pop('return', None)
    self.data_param, *self.call_params = list(self.processor_params.keys())


# %% ../nbs/01_aicorebridge.ipynb 10
# can be overloaded
@patch
def call_processor(self:AICoreModule, calldata, **callargs):
    return self.processor(calldata, **callargs)


# %% ../nbs/01_aicorebridge.ipynb 12
@patch
def infer(self:AICoreModule, data:dict, *_, **kwargs):
    try:

        msg=[
            f"Startup time: {self.init_time.isoformat()}",
            f"{self.processor.__name__}({self.processor_signature})",             
            f"init_args: {self.init_args}, init_kwargs: {self.init_kwargs}",
        ]

        lastSeen = kwargs.pop('lastSeen', False)
        recordformat = kwargs.pop('format', "records").lower()
        reversed = kwargs.pop('reversed', False)
        timezone = kwargs.get('timezone', 'UTC')
        msg.append(f"lastSeen: {lastSeen}, recordformat: {recordformat}, timezone: {timezone}")

        calldata = self.get_call_data(
            data, 
            recordformat=recordformat,
            timezone=timezone,
            reversed=reversed)
        
        msg.append(f"calldata shape: {calldata.shape}")

        callargs = self.get_callargs(**kwargs)

        for arg, val in callargs.items():
            msg.append(f"{arg}: {val}")
            
        result = self.call_processor(calldata, **callargs)
        msg.append(f"result shape: {result.shape}")

        return {
            'msg':msg,
            'data': timeseries_dataframe_to_datadict(
                result if not lastSeen else result[-1:],
                recordformat=recordformat,
                timezone=timezone,
                reversed=reversed)
        }
    except Exception as err:
        msg.append(''.join(traceback.format_exception(None, err, err.__traceback__)))
        syslog.exception(f"Exception {str(err)} in infer()")
        return {
            'msg': f"Unexpected {err=}, {type(err)=}",
            'data': []
        }


# %% ../nbs/01_aicorebridge.ipynb 14
@patch
def get_callargs(self:AICoreModule, **kwargs):
    "Get arguments for the processor call"

    # Remove null / None values
    kwargs = {k:v for k,v in kwargs.items() if v is not None}
    
    metadata = kwargs.pop('metadata', {}) # TODO: historic metadata

    return {
        K:self.processor_signature.parameters[K].annotation(
            self.init_kwargs.get(
                K,
                kwargs.get(
                    K,
                    metadata.get(
                        K, 
                        self.processor_signature.parameters[K].default
                    )
                )
            )
        )
        for K in self.call_params
    }


# %% ../nbs/01_aicorebridge.ipynb 15
@patch
def get_call_data(
        self:AICoreModule, 
        data:dict, 
        recordformat='records', 
        timezone='UTC', 
        reversed=False):
    
    "Convert data to the processor signature"

    df = set_time_index_zone(timeseries_dataframe_from_datadict(
        data, ['datetimeMeasure', 'time'], recordformat), timezone)

    if reversed:
        df = df[::-1]

    if self.processor_params[self.data_param].annotation == pd.DataFrame:
        return df
    elif len(df.columns) > 1:
        df.index = (df.index - datetime.datetime(1970,1,1, tzinfo=datetime.timezone.utc)) / datetime.timedelta(seconds=1)
        return df.to_records(index=True)
    else:
        df.index = (df.index - datetime.datetime(1970,1,1, tzinfo=datetime.timezone.utc)) / datetime.timedelta(seconds=1)
        return df.reset_index().to_numpy()
        
