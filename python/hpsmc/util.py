"""! Miscellaneous utility functions."""

import sys
import logging

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
    except:
        pass
    try:
        intval = int(val)
        return intval
    except:
        pass
    return val

def config_logging(stream=sys.stdout, level=logging.DEBUG, logname='hpsmc'):
    """!
    Configure logging by setting an output stream and level (both optional).
    Any handlers already registered will be replaced by calling this method.
    """
    #print("Config logging: " + str(stream) + " " + str(level) + " " + logname)
    logger = logging.getLogger(logname)
    logger.propagate = False
    logger.handlers = [] # Reset handlers in case this is called more than once
    logger.setLevel(level)
    handler = logging.StreamHandler(stream)
    handler.setLevel(level)
    handler.setFormatter(logging.Formatter('%(name)s:%(levelname)s %(message)s'))
    logger.addHandler(handler)
    return logger
