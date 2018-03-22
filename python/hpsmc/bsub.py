from hpsmc.batch import LSF

if __name__ == "__main__":
    batch = LSF()
    batch.parse_args()
    batch.submit() 
