import shutil
import os
import sys
import configparser
import logging

from os.path import expanduser

# To play nice with Lustre, but only effective as of python 3.8
shutil.COPY_BUFSIZE = 1024 * 1024

# Setup the parser for loading global configuration settings.
# It is okay to access this variable from other modules like job.
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
# This object should not be accessed directly.
_global_logger = logging.getLogger("hpsmc")
_global_logger.propagate = False
_global_logger.handlers = []
_global_logger.setLevel(_loglevel)
_handler = logging.StreamHandler(_logstream)
_handler.setLevel(logging.DEBUG)
_handler.setFormatter(logging.Formatter('%(name)s:%(levelname)s %(message)s'))
_global_logger.addHandler(_handler)

# Print a log message showing what global config files were found and loaded.
if len(_config_files) > 0:
    _global_logger.info("Config files found: {}".format(_config_files))
    #_global_logger.info("Config settings: {}".format(\
    #    {section: dict(global_config[section]) for section in global_config}))
else:
    _global_logger.info("No config files were found at default locations!")
