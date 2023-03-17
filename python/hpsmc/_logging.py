"""! @package _config
Global logging utilities for initialization.
"""
import logging
import sys

def _setup_logging(config):

    loglevel = logging.INFO
    logstream = sys.stdout

    # Set a new log level if one was provided in the user configuration.
    if 'HPSMC' in config:
        # Get the log level from the configuration or use a default.
        if 'loglevel' in config['HPSMC']:
            loglevel = logging.getLevelName(config['HPSMC']['loglevel'])

        # Get a log file location from the configuration or print to the terminal.
        if 'logfile' in config['HPSMC']:
            logstream = open(config['HPSMC']['logfile'], 'w')

    # Configure the global logger settings.
    # This object should not be accessed directly.
    logger = logging.getLogger("hpsmc")
    logger.propagate = False
    logger.handlers = []
    logger.setLevel(loglevel)
    handler = logging.StreamHandler(logstream)
    handler.setLevel(logging.DEBUG)
    handler.setFormatter(logging.Formatter('%(name)s:%(levelname)s %(message)s'))
    logger.addHandler(handler)

    return logger