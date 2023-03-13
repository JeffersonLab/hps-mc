import logging
import sys

def create_logger(stream=sys.stdout, level=logging.DEBUG, logname='hpsmc'):
    """!
    Configure logging by setting an output stream and level (both optional).
    Any handlers already registered will be replaced by calling this method.
    """
    logger = logging.getLogger(logname)
    logger.propagate = False
    logger.handlers = []  # Reset handlers in case this is called more than once
    logger.setLevel(level)
    handler = logging.StreamHandler(stream)
    handler.setLevel(level)
    handler.setFormatter(logging.Formatter('%(name)s:%(levelname)s %(message)s'))
    logger.addHandler(handler)
    return logger
