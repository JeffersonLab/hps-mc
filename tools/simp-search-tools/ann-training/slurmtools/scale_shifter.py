import joblib
import numpy as np

import sys

arg1 = sys.argv[1]

MASS=arg1

scaler = joblib.load("scaler_2021_v9_pass5_run42_QualCuts_"+str(int(MASS))+"_v3.pkl")

np.savez(
    "scaler_arrays_"+str(MASS)+"_v3.npz",
    mean=scaler.mean_.astype(np.float32),
    scale=scaler.scale_.astype(np.float32)
)
