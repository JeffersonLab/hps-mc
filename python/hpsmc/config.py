import os, logging, configparser

from os.path import expanduser

logger = logging.getLogger("hpsmc.config")

config_files = [expanduser("~") + os.sep + ".hpsmc", ".hpsmc"]

# Ensure config is only loaded once
try:
    parser
except:
    parser = configparser.ConfigParser()    
    logger.info("Reading config from: %s" % str(config_files))
    parser.read(config_files)
    #print("Read config items...")
    #for section in parser.items():
    #    for item in section:
    #        print(item)
    logger.info("Done parsing config!")

# Load from a non-default file location
def load(self, path):
    logger.info("Loading config from '%s'" % path)
    parser.read(path)