import json

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
