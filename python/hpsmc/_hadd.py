"""! running hadd from in hps-mc jobs"""

import shutil
import re
import os
import logging

from hpsmc.component import Component

class hadd(Component):
    """! Run ROOT's histogram/TTree merger 
    
    Required: 
    - **inputs**: input ROOT files to merge
    - **outputs**: *single* output file to merge into
    Optional:
    - **ncores**: number of cores to supply to hadd (default 1)
    """

    logger = logging.getLogger('hpsmc.tools.hadd')

    def __init__(self, **kwargs) :
        self.output_ext = 'root'
        self.ncores = None
        super().__init__('hadd', command='hadd', **kwargs)

    def optional_parameters(self) :
        return ['ncores']

    def cmd_args(self) :
        a = []
        if self.ncores is not None :
            a += ['-j',self.ncores]
        if len(self.outputs) != 1 :
            raise ValueError('hadd makes no sense without exactly one output')
        a += self.outputs
        if self.inputs is None or len(self.inputs) == 0 :
            raise ValueError('hadd makes no sense without at least one input')
        a += self.inputs
        return a

