import json, sys, logging

def load_json_data(filename):
    rawdata = open(filename, 'r').read()
    return json.loads(rawdata)

def convert_config_value(val):
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

def config_logging(stream=sys.stdout, level=logging.INFO):
    """
    Configure logging by setting an output stream and level (both optional).
    Any handlers already registered will be replaced by calling this method.
    """
    global_logger = logging.getLogger('hpsmc')
    global_logger.handlers = [] # Reset handlers in case this is called more than once
    global_logger.setLevel(level)
    handler = logging.StreamHandler(stream)
    handler.setFormatter(logging.Formatter('%(name)s:%(levelname)s %(message)s'))
    global_logger.addHandler(handler)
    