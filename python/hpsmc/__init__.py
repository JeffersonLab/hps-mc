import shutil
import os
import sys
import configparser
import logging

from os.path import expanduser

from ._logging import config_logging

# To play nice with Lustre, but only effective as of python 3.8
shutil.COPY_BUFSIZE = 1024 * 1024

# Setup the parser for loading global configuration settings.
global_config = configparser.ConfigParser()

# By default look for files called ".hpsmc" in the user home and current directories.
_config_files = global_config.read([
    os.path.join(expanduser("~"), ".hpsmc"), 
    os.path.abspath(".hpsmc")])

# Set a new log level if one was provided in the user configuration.
if 'HPSMC' in global_config: 
    # Get the log level from the configuration or use a default.
    if 'loglevel' in global_config['HPSMC']:
        _loglevel = logging.getLevelName(global_config['HPSMC']['loglevel'])
    else:
        _loglevel = logging.INFO

    # Get a log file location from the configuration or print to the terminal.
    if 'logfile' in global_config['HPSMC']:
        _logstream = open(global_config['HPSMC']['logfile'], 'w')
    else:
        _logfile = sys.stdout

# Enable default logging configuration.
global_logger = config_logging(stream=_logstream, level=_loglevel)

# Print the global config to the log.
if len(_config_files) > 0:
    global_logger.info("Read hpsmc config: {}".format(_config_files))
    global_logger.info("Config settings: {}".format(\
        {section: dict(global_config[section]) for section in global_config}))
else:
    global_logger.info("No config files were found at default locations!")
