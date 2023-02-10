"""! running pede from in hps-mc jobs"""

from hpsmc.component import Component

class PEDE(Component):
    """! Run pede minimizer over input bin files for alignment

    """

    def __init__(self, **kwargs) :
        self._pede_steering_file = None
        super().__init__('pede',
                command='pede', 
                **kwargs
                )

    def _write_pede_steering_file(self) :
        return '/path/to/pede/steering/file'

    def required_parameters() :
        return ['inputs']

    def optional_parameters() :
        return ['subito']

    def cmd_args(self) :
        a = [self._pede_steering_file]
        if self.subito :
            a += ['-s']
        return a

    def setup(self) :
        """pre-run initialization"""
        self._pede_steering_file = _write_pede_steering_file()

    def cleanup(self) :
        """post-run de-initialization"""
        # copy pede output files to output directory
        pass

