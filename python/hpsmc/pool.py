from hpsmc.batch import Pool

if __name__ == "__main__":
    batch = Pool()
    batch.parse_args()
    batch.submit()
