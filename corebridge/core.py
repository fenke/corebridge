# AUTOGENERATED! DO NOT EDIT! File to edit: ../nbs/00_core.ipynb.

# %% auto 0
__all__ = ['ResamplerMethods', 'ReSamplerPeriods', 'init_console_logging', 'set_time_index_zone', 'timeseries_dataframe',
           'timeseries_dataframe_from_datadict', 'pop_nan_values', 'timeseries_dataframe_to_datadict',
           'timeseries_dataframe_resample', 'AICoreModuleBase']

# %% ../nbs/00_core.ipynb 3
import typing, logging
import os, datetime
import numpy as np
import pandas as pd

from fastcore.basics import patch_to, patch
from . import __version__


# %% ../nbs/00_core.ipynb 4
def init_console_logging(name=None, level=logging.INFO, timestamp=True):
    '''Setup none-blocking stream handler for sending loggin to the console.'''

    # Only if no handlers defined.
    if not logging.getLogger(name).handlers:

        logger = logging.getLogger()
        logger.setLevel(level)

        console = logging.StreamHandler()
        console.setLevel(level)

        # set a format which is simpler for console use
        if timestamp:
            formatter = logging.Formatter("%(asctime)s %(levelname)s\t%(process)d\t%(name)s\t%(filename)s\t%(lineno)d\t%(message)s", datefmt='%Y-%m-%dT%H:%M:%S%z')
        else:
            formatter = logging.Formatter("%(levelname)s\t%(process)d\t%(name)s\t%(filename)s\t%(lineno)d\t%(message)s")
            
        #formatter = logging.Formatter("%(asctime)s %(levelname)s\t%(process)d\t%(name)s\t%(filename)s\t%(lineno)d\t%(message)s", datefmt='%Y-%m-%dT%H:%M:%S%z')

        # tell the handler to use this format
        console.setFormatter(formatter)

        # add the handler to the root logger
        logger.addHandler(console)
        return logger
    else:
        logging.getLogger(name).info(f'There already is a logger installed for {name}.')


# %% ../nbs/00_core.ipynb 7
try:
    logging.getLogger(__name__).info()(f"Loading {__name__} from {__file__}")
except:
    pass

# %% ../nbs/00_core.ipynb 10
def set_time_index_zone(
        df:pd.DataFrame,    # Dataframe to set or convert the timeindex on
        timezone            # Timezone to set
    ) :
    """
    Sets the time zone of the index of a pandas DataFrame.

    Args:
        df (pd.DataFrame): The DataFrame whose index time zone is to be set.
        timezone (str): The desired time zone.

    Returns:
        pd.DataFrame: The modified DataFrame with its index time zone set 
        to the specified time zone.

    Raises:
        None

    Examples:
        >>> df = pd.DataFrame({'A': [1, 2, 3]}, index=pd.DatetimeIndex(['2022-01-01', '2022-01-02', '2022-01-03']))
        >>> set_time_index_zone(df, 'Europe/Berlin')
                     A
        2022-01-01  1
        2022-01-02  2
        2022-01-03  3
        DatetimeIndex: 3 entries, 2022-01-01 01:00:00+01:00 to 2022-01-03 01:00:00+01:00
    """
    
    if isinstance(df.index, pd.DatetimeIndex):
        df.index.name = 'time'
        if not hasattr(df.index, 'tz')  or not df.index.tz or not df.index.tz:
            df.index = df.index.tz_localize('UTC').tz_convert(timezone)
        elif str(df.index.tz) != timezone:
            df.index = df.index.tz_convert(timezone)

    return df


# %% ../nbs/00_core.ipynb 14
def timeseries_dataframe(
        data:typing.Union[pd.DataFrame, pd.Series, dict, np.ndarray, np.recarray], 
        timezone='UTC', 
        columnnames=None):
    
    """Convert various tabular data formats to timeseries DataFrame

    Args:
        data (Union[pd.DataFrame, pd.Series, dict, np.ndarray, np.recarray]): The input data to be converted.
        timezone (str, optional): The timezone to set for the index of the DataFrame. Defaults to 'UTC'.
        columnnames (Optional[List[str]]): The column names to use for the DataFrame. Defaults to None.

    Returns:
        pd.DataFrame: The converted timeseries DataFrame with the index set to the specified timezone.
    """

    if isinstance(data, pd.DataFrame):
        df = data.copy()
        df.index = pd.DatetimeIndex(df.index).round('ms')

    elif isinstance(data, pd.Series):
        df = pd.DataFrame(data)
        df.index = pd.DatetimeIndex(df.index).round('ms')

    elif isinstance(data, dict):
        # assume a dict/mapping of individual arrays representing timeseries 
        df = pd.DataFrame({
            C:pd.Series(data=A[:,1], index=pd.DatetimeIndex(A[:,0]*1e9)) if isinstance(A, np.ndarray) else A
            for C,A in data.items()
        })

    elif data.dtype.names is not None:
        # structured or recarray, we use column names from the recarray
        df = pd.DataFrame(
            data=data.view(dtype=np.float64).reshape(data.shape[0],len(data.dtype))[:,range(1,len(data.dtype))],
            index=pd.DatetimeIndex(data.view(dtype=np.float64).reshape(data.shape[0],len(data.dtype))[:,0] * 1e9),
            columns=data.dtype.names[1:]
        )

    else:
        if data.shape[0] > 0:
            # column names, either 'value' if there is only one column, or
            # value_0, value_1 .... value_nn when more the one column is present
            if data.shape[1]>2:
                columns=[f"value_{str(i+1)}" for i in range(data.shape[1]-1)] if not columnnames else [f"{str(i)}" for i in columnnames[1:]]
            else:
                columns=['value']

            df = pd.DataFrame(
                data=data[:, 1:],
                index=pd.DatetimeIndex(data[:,0]*1e9),
                columns=columns
            )
        else:
            return pd.DataFrame()

    return set_time_index_zone(df, timezone)

# %% ../nbs/00_core.ipynb 17
def timeseries_dataframe_from_datadict(
        data:dict, 
        timecolumns=None,
        recordformat='records'):
        
    """
    Converts a data dict into a pandas DataFrame based on the specified record format. 
    Parameters:
        - data: A dictionary containing the data to convert.
        - timecolumns: A list of column names to be treated as time columns.
        - recordformat: A string specifying the format of the data records ('records', 'table', 'split', 'index', 'tight').
    Returns:
        - df: A pandas DataFrame with a DatetimeIndex representing the converted data.
    """

    orient = recordformat.lower()
    assert orient in ['records', 'table', 'split', 'index', 'tight']
    assert timecolumns, 'No time columns specified'

    if orient == 'records':
        # data is a structured ndarray, sequence of tuples or dicts, or DataFrame
        df = pd.DataFrame.from_records(data)
        time_columns_in_df = [C for C in df.columns if C in timecolumns]
        if not  time_columns_in_df:
            #syslog.error(f"No  column in records {df.columns} matches specification in time columns {timecolumns}, assuming first column is time")
            time_column = df.columns[0]
        else:
            time_column = time_columns_in_df[0]

    elif orient == 'table':
        # data is in pandas table format
        time_column = data['schema']['primaryKey'][0]
        df = pd.DataFrame.from_dict(data['data']).set_index(data['schema']['primaryKey'])
        df.index.name = 'time'
    else:
        # data  is formatted according to 'orient' parameter (pandas)
        df = pd.DataFrame.from_dict(data, orient=orient)
        time_column = df.index.name


    df.columns = list(df.columns)
    df[time_column] = pd.to_datetime(df[time_column],utc=True,format='ISO8601')
    df.set_index(time_column, inplace=True)
    df.index = pd.DatetimeIndex(df.index).round('ms')
    
    df.index.name = 'time'

    return df


# %% ../nbs/00_core.ipynb 23
def pop_nan_values(data):
    """
    Recursively pop keys with nan values from dict or lists with dicts.

    Args:
        data (Union[list, dict]): The data to be processed.

    Returns:
        Union[list, dict]: The processed data with keys with nan values removed.
    """
    
    if isinstance(data, list):
        return [pop_nan_values(v) for v in data if pd.notnull([v]).any()]
    elif isinstance(data, dict):
        return {k:pop_nan_values(v) for k, v in data.items() if pd.notnull([v]).any()}
    else:
        return data

# %% ../nbs/00_core.ipynb 35
def timeseries_dataframe_to_datadict(
        data:typing.Union[pd.DataFrame, pd.Series, dict], 
        recordformat:str='records', 
        timezone:str='UTC',
        popNaN:bool=False):

    """
    Convert a timeseries DataFrame or Series into a dictionary representation.

    Args:
        data (Union[pd.DataFrame, pd.Series, dict]): The input data to be converted. It can be a pandas DataFrame, Series, or a dictionary.
        recordformat (str, optional): The format of the output records. Defaults to 'records'.
        timezone (str, optional): The timezone to use for the DataFrame index. Defaults to 'UTC'.
        popNaN (bool, optional): Whether to remove NaN values from the output dictionary. Defaults to False.

    Returns:
        Union[dict, list]: The converted dictionary representation of the input data. If `popNaN` is True, it returns a dictionary with NaN values removed. Otherwise, it returns a dictionary or a list of dictionaries depending on the `recordformat` parameter.
    """
        
    orient = recordformat.lower()

    normalized_data = timeseries_dataframe(data, timezone=timezone)
    if isinstance(normalized_data.index, pd.DatetimeIndex):
        if timezone == 'UTC':
            print(f"Normalized, UTC")
            normalized_data.index = normalized_data.index.strftime("%FT%R:%SZ")
        else:
            print(f"Normalized, {timezone}")
            normalized_data.index = normalized_data.index.map(lambda x: x.isoformat(timespec='milliseconds'))
                 
    if orient == 'records':
        records = normalized_data.reset_index().to_dict(orient='records')
    else:
        records =  normalized_data.to_dict(orient=orient)

    if popNaN and normalized_data.isna().any(axis=None):
            return pop_nan_values(records)
    
    return records    


# %% ../nbs/00_core.ipynb 51
#def interpolate_timeseries(sampler, period, method_args):


ResamplerMethods = dict(
    count=lambda R: R.count(),
    median=lambda R: R.median(),
    mean=lambda R: R.mean(),
    min=lambda R: R.min(),
    max=lambda R: R.max(),
    sum=lambda R: R.sum(),
    std=lambda R: R.std(),
    var=lambda R: R.var(),
    nearest=lambda R: R.nearest(),
)

ReSamplerPeriods = dict(
    H='h', T='min', S='sec', L='ms', U='us', N='ns'
)

def timeseries_dataframe_resample(df:pd.DataFrame, period:str, method:str):
    """
    Resamples a time-series DataFrame on the specified period and method.

    Parameters:
        df (pd.DataFrame): The input time-series DataFrame.
        period (str): The resampling period.
        method (str): The resampling method. Can be a string of multiple methods separated by ';'.
        method_args (dict, optional): Additional arguments for the resampling method.

    Returns:
        pd.DataFrame: The resampled DataFrame.
    """
    sampler = df.resample(ReSamplerPeriods.get(period, str(period)))

    dataframes = [df]
    for M in str(method).split(';'):
        sdf = ResamplerMethods.get(M)(sampler)
        sdf.columns = [f"{C}_{M}" for C in df.columns]
        dataframes.append(sdf)

    return pd.concat(dataframes, axis=1, join='outer')



# %% ../nbs/00_core.ipynb 55
class AICoreModuleBase:
    pass


# %% ../nbs/00_core.ipynb 56
@patch
def __init__(self:AICoreModuleBase, 
            save_dir:str, # path where the module can keep files 
            assets_dir:str, # path to support files (scripts, metadata, etc)
            *args, **kwargs):
    
    self.init_time = datetime.datetime.now(datetime.UTC)
    self.aicorebridge_version = __version__

    self.init_args = args
    self.init_kwargs = dict(
        **kwargs,
        assets_dir=assets_dir,
        save_dir=save_dir
    )

