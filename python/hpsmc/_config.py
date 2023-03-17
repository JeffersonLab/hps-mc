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