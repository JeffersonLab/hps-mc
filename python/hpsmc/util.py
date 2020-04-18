import json

def load_json_data(filename):
    rawdata = open(filename, 'r').read()
    return json.loads(rawdata)
