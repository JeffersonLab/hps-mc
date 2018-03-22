from hpsmc.batch import Auger

if __name__ == "__main__":
    batch = Auger()
    batch.parse_args()
    batch.submit() 
