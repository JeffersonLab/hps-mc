import shutil

from .util import config_logging

# Enable default logging config
config_logging()

# To play nice with Lustre, but only effective as of python 3.8
shutil.COPY_BUFSIZE = 1024 * 1024
