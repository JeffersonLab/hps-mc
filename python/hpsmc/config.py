import os, logging

logger = logging.getLogger("hpsmc.config")
logger.setLevel(logging.INFO)

from os.path import expanduser

import ConfigParser as configparser

config_files = [expanduser("~") + os.sep + ".hpsmc", ".hpsmc"]

# Ensure config is only loaded once
try:
    parser
except:
    parser = configparser.ConfigParser()
    logger.debug("Reading config from: %s" % str(config_files))
    parser.read(config_files)
    for section in parser.sections():
        logger.debug("[" + section + "]")        
        for i,v in parser.items(section): 
            logger.debug("%s=%s" % (i, v))
                              
    logger.debug("Done parsing config!")

# Load from a non-default file location
def load(self, path):
    logger.debug("Loading config from '%s'" % path)
    parser.read(path)
    
def convert_value(val):
    if val == 'True' or val == 'true': 
        return True
    elif val == 'False' or val == 'false':
        return False
    try:
        if val.contains('.'):
            floatval = float(value)
            return floatval
    except:
        pass
    try:
        intval = int(val)
        return intval
    except:
        pass
    return val
