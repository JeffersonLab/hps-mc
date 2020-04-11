# hpsmc python package
import logging, sys

global_logger = logging.getLogger("hpsmc")
global_logger.setLevel(logging.DEBUG)
handler = logging.StreamHandler(sys.stdout)
handler.setFormatter(logging.Formatter('%(name)s:%(levelname)s %(message)s'))
global_logger.addHandler(handler)
