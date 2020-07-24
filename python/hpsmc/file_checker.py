# TODO: Delete me

import os, json

def check(j):    
    jsonfile = j.application.args[1]
    with open(jsonfile) as datafile:
        data = json.load(datafile)    
    output_dir = ""
    if "output_dir" in data:
        output_dir = data["output_dir"]
    if "output_files" in data:
        outputfiles = data["output_files"]
        for k,v in outputfiles.items():
            fpath = v
            if not os.path.isabs(v):
                fpath = output_dir + os.path.sep + v
            if not os.path.exists(fpath):
                return False
    return True
