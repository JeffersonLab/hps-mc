"""! running pede from in hps-mc jobs"""

import shutil
import re
import os
import logging

from hpsmc.component import Component
from ._parameter import Parameter
from ._util import getBeamspotConstraintsFloatingOnly

class PEDE(Component):
    """! Run pede minimizer over input bin files for alignment
    """

    logger = logging.getLogger('hpsmc.tools.PEDE')

    # each function is a "mask" in the sense that
    #    it takes in a Parameter and should return True
    #    if it should be floated and False if not
    parameter_groups = {
        'all' : lambda _p : True,
        'allsensors' : lambda p : (p.mp_layer_id < 23),
        'tu' : lambda p : (p.direction == 1 and p.trans_rot == 1),
        'rw' : lambda p : (p.direction == 3 and p.trans_rot == 2)
        }

    def __init__(self, **kwargs) :
        self._pede_steering_file = None
        self.to_float = []
        self.param_map = None
        self.pede_minimization = None

        self.subito = False
        self.constraint_file = None
        self.previous_fit = None
        self.beamspot_constraints = False
        self.survey_constraints = False

        self.output_ext = 'res'

        super().__init__('pede', command='pede', **kwargs)

    def _write_pede_steering_file(self) :
        PEDE.logger.info(f'Parameter Map: {self.param_map}')
        parameters = Parameter.parse_map_file(self.param_map)

        if self.previous_fit is not None :
            PEDE.logger.info(f'Loading previous fit: {self.previous_fit}')
            Parameter.parse_pede_res(self.previous_fit, 
                destination = parameters, 
                skip_nonfloat = False)

        # define which parameters are floating
        for pattern in map(Pattern,self.to_float) :
            for p in parameters.values() : 
                if pattern.match(p) :
                    PEDE.logger.debug(f'Floating parameter {p}')
                    p.float()
    
        # build steering file for pede
        pede_steering_file = 'pede-steer.txt'
        with open(pede_steering_file,'w') as psf :
            # write out input mille binary files
            psf.write('CFiles\n')
            for ipf in self.inputs :
                psf.write(ipf+'\n')
    
            # external constraint file
            if self.constraint_file is not None :
                PEDE.logger.info(f'Adding constraint file {self.constraint_file}')
                psf.write('\n')
                psf.write('!Constraint file\n')
                psf.write(constraint_file+'\n')
    
            # list parameters
            psf.write('\nParameter\n')
            for i, p in parameters.items() : 
                psf.write(p.pede_format() + '\n')
    
            # survey constraints
            if self.survey_constraints :
                PEDE.logger.info('Applying survey constraints')
                PEDE.logger.warn('UNTESTED - not sure if this is correct (Tom E)')
                survey_meas_tu = 0.05
                psf.write('\n!Survey constraints tu\n')
                for p, name in param_map.items() :
                    if p.module_number() == 0 and p.direction == 1 and p.trans_rot == 1 :
                        psf.write('\nMeasurement 0.0 %.3f\n' % survey_meas_tu)
                        psf.write('%s 1.0\n' & p)
                psf.write("\n\n")
            
            # apply beamspotConstraint (This I think is not correct)
            if self.beamspot_constraints:
                PEDE.logger.error('Beamspot constraints not implemented!')
                return -1
                psf.write(getBeamspotConstraintsFloatingOnly(param_map))
                psf.write("\n\n")
            
            psf.write("\n\n")
            PEDE.logger.info(f'Appending minimization settings from {self.pede_minimization}')
            # determine MP minimization settings
            with open(self.pede_minimization) as minfile :
                for line in minfile :
                    psf.write(line)
        
        return pede_steering_file
    
    
    def _print_pede_res(self) :
        # print parameters that were floated so user can see results
        parameters = Parameter.parse_pede_res('millepede.res', skip_nonfloat=True)
        PEDE.logger.info('Deduced Parameters')
        for i, p in parameters.items() :
            if p.active :
                PEDE.logger.info(f'  {p}')
        return

    def required_parameters(self) :
        return ['to_float']

    def required_config(self) :
        return ['param_map', 'pede_minimization']

    def optional_parameters(self) :
        return ['subito','constraint_file','previous_fit','beamspot_constraints','survey_constraints']

    def cmd_args(self) :
        a = [self._pede_steering_file]
        if self.subito :
            a += ['-s']
        return a

    def setup(self) :
        """pre-run initialization"""
        self._pede_steering_file = self._write_pede_steering_file()

    def cleanup(self) :
        """post-run de-initialization"""
        # copy pede output files to output directory
        self._print_pede_res()

