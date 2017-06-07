from batch import Auger

if __name__ == "__main__":
    submit = Auger()
    submit.parse_args()
    submit.submit_all()
