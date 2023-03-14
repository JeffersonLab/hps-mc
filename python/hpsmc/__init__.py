import shutil
import os
import sys
import configparser
import logging

from os.path import expanduser

from ._logging import create_logger

# To play nice with Lustre, but only effective as of python 3.8
shutil.COPY_BUFSIZE = 1024 * 1024

# Setup the parser for loading global configuration settings.
global_config = configparser.ConfigParser()

# By default look for files called ".hpsmc" in the user home and current directories.
_config_files = global_config.read([
    os.path.join(expanduser("~"), ".hpsmc"), 
    os.path.abspath(".hpsmc")])

# Default log settings.
_loglevel = logging.INFO
_logstream = sys.stdout

# Set a new log level if one was provided in the user configuration.
if 'HPSMC' in global_config: 
    # Get the log level from the configuration or use a default.
    if 'loglevel' in global_config['HPSMC']:
        _loglevel = logging.getLevelName(global_config['HPSMC']['loglevel'])

    # Get a log file location from the configuration or print to the terminal.
    if 'logfile' in global_config['HPSMC']:
        _logstream = open(global_config['HPSMC']['logfile'], 'w')

# Configure the global logger settings.
global_logger = logging.getLogger("hpsmc")
global_logger.propagate = False
global_logger.handlers = [] 
global_logger.setLevel(_loglevel)
handler = logging.StreamHandler(_logstream)
handler.setLevel(logging.DEBUG)
handler.setFormatter(logging.Formatter('%(name)s:%(levelname)s %(message)s'))
global_logger.addHandler(handler)

# Print the global config to the log.
if len(_config_files) > 0:
    global_logger.info("Read hpsmc config: {}".format(_config_files))
    global_logger.info("Config settings: {}".format(\
        {section: dict(global_config[section]) for section in global_config}))
else:
    global_logger.info("No config files were found at default locations!")
