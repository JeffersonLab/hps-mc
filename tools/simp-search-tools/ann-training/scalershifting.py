import joblib
import numpy as np

MASS=210
scaler = joblib.load("scaler_2021_v9_pass5_run42_QualCuts_"+str(MASS)+"_v3.pkl")

np.savez(
    "scaler_arrays_"+str(MASS)+"_v3.npz",
    mean=scaler.mean_.astype(np.float32),
    scale=scaler.scale_.astype(np.float32)
)