import shutil

from ._config import _read_global_config
from ._logging import _setup_logging

# To play nice with Lustre, but only effective as of python 3.8
shutil.COPY_BUFSIZE = 1024 * 1024

# Load the global configuration settings.
# Accessing this variable from other modules like job is fine.
global_config, _config_files = _read_global_config()

# Setup global logging.
# This should not be accessed directly from other modules.
_global_logger = _setup_logging(global_config)

# Print a log message showing what global config files were found and loaded.
if len(_config_files) > 0:
    _global_logger.info("Config files found: {}".format(_config_files))
else:
    _global_logger.warn("No config files were found at default locations! (`~/.hpsmc` or `.hpsmc` in your current directory)")
