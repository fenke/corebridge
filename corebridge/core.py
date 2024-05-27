# AUTOGENERATED! DO NOT EDIT! File to edit: ../nbs/00_core.ipynb.

# %% auto 0
__all__ = ['timeseries_dataframe', 'set_time_index_zone', 'timeseries_dataframe_from_datadict',
           'timeseries_dataframe_to_datadict']

# %% ../nbs/00_core.ipynb 3
import typing
import os
import numpy as np
import pandas as pd

# %% ../nbs/00_core.ipynb 4
try:
    print(f"Loading {__name__} from {__file__}")
except:
    pass

# %% ../nbs/00_core.ipynb 6
def timeseries_dataframe(
        data:typing.Union[pd.DataFrame, pd.Series, dict, np.ndarray, np.recarray], 
        timezone='UTC', 
        columnnames=None):
    
    """Convert various data formats to timeseries DataFrame"""

    if isinstance(data, pd.DataFrame):
        df = data

    elif isinstance(data, pd.Series):
        df = pd.DataFrame(data)

    elif isinstance(data, dict):
        # dict/mapping of individual timeseries
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

    df.index.name = 'time'
    if not df.index.tz:
        df.index = df.index.tz_localize('UTC').tz_convert(timezone)
    elif str(df.index.tz) != timezone:
        df.index = df.index.tz_convert(timezone)

    return set_time_index_zone(df, timezone)

# %% ../nbs/00_core.ipynb 7
def set_time_index_zone(df:pd.DataFrame, timezone):

    df.index.name = 'time'
    if not df.index.tz:
        df.index = df.index.tz_localize('UTC').tz_convert(timezone)
    elif str(df.index.tz) != timezone:
        df.index = df.index.tz_convert(timezone)

    return df


# %% ../nbs/00_core.ipynb 8
def timeseries_dataframe_from_datadict(
        data:dict, 
        timecolumns,
        recordformat='records'):
        
    "Convert data dict to dataframe"

    orient = recordformat.lower()
    assert orient in ['records', 'table']
    
    if orient == 'records':
        df = pd.DataFrame.from_records(data)
        time_column = [C for C in df.columns if C in timecolumns][0]

    elif orient == 'table':
        time_column = data['schema']['primaryKey'][0]
        df = pd.DataFrame.from_dict(data['data']).set_index(data['schema']['primaryKey'])
        df.index.name = 'time'
    else:
        time_column = [C for C in df.columns if C in timecolumns][0]


    df.columns = list(df.columns)
    df[time_column] = pd.to_datetime(df[time_column],utc=True,format='ISO8601')
    df.set_index(time_column, inplace=True)
    #df.index = pd.DatetimeIndex(df.index).round('ms')
    
    df.index.name = 'time'

    return df


# %% ../nbs/00_core.ipynb 9
def timeseries_dataframe_to_datadict(
        data:typing.Union[pd.DataFrame, pd.Series, dict], 
        recordformat:str='split', 
        timezone:str='UTC', 
        reversed:bool=False):
    
    orient = recordformat.lower()

    normalized_data = timeseries_dataframe(data, timezone=timezone)
    normalized_data.index = normalized_data.index.map(lambda x: x.isoformat())
    
    if reversed:
        normalized_data = normalized_data[::-1]

    if orient == 'records':
        records = normalized_data.reset_index().to_dict(orient='records')
    else:
        records =  normalized_data.to_dict(orient=orient)
    

    if normalized_data.isna().any(axis=None):
        return [ {k:v for k,v in m.items() if pd.notnull(v)} for m in records]
    else:
        return records



