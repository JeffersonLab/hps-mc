from hpsmc.batch import Local

if __name__ == "__main__":
    batch = Local()
    batch.parse_args()
    batch.submit() 
