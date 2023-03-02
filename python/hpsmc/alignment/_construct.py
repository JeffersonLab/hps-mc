"""! construct a LCDD and recompile within hps-mc jobs"""

from hpsmc.component import Component

class ConstructDetector(Component) :
    """! construct an LCDD from a compact.xml and recompile necessary parts of hps-java
    
    This is a Component interface to the hps-mc-construct-detector script.
    """

    def __init__(self) :
        # config
        self.java_dir = None
        self.hps_java_bin_jar = None

        # required job
        self.detector = None
        
        # optional job
        self.next_detector = None

        super().__init__('ConstructDetector',
                         command='hps-mc-construct-detector')

    def required_config(self) :
        return ['java_dir', 'hps_java_bin_jar']

    def required_parameters(self) :
        return ['detector']

    def optional_parameters(self) :
        return ['next_detector']

    def cmd_args(self) :
        return [ self.detector if self.next_detector is not None else self.next_detector, 
            '-p', self.java_dir, '-jar', self.hps_java_bin_jar ]

