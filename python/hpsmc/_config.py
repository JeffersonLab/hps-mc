"""! @package _config
Global config utilities for initialization.
"""

import configparser
import logging
import sys
import os
from os.path import expanduser


def _read_global_config():
    """! Read global configuration files.
    @param filename  name of json file
    @return a ConfigParser object with the configuration settings
    """
    config = configparser.ConfigParser()
    config_files = config.read([
        os.path.join(expanduser("~"), ".hpsmc"),
        os.path.abspath(".hpsmc")])
    return config, config_files

def convert_config_value(val):
    """! Convert config value to Python readable value."""
    if val == 'True' or val == 'true':
        return True
    elif val == 'False' or val == 'false':
        return False
    try:
        if val.contains('.'):
            floatval = float(val)
            return floatval
    except BaseException:
        pass
    try:
        intval = int(val)
        return intval
    except BaseException:
        pass
    return val
