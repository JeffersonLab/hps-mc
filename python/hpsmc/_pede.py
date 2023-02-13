"""! running pede from in hps-mc jobs"""

import os
import logging

from .component import Component
from ._alignment import Parameter

class PEDE(Component):
    """! Run pede minimizer over input bin files for alignment

    """

    logger = logging.getLogger('hpsmc.tools.PEDE')

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
        for f in self.to_float :
            idn = None
            if f.isnumeric() :
                # string is a number, assume it is the idn
                idn = int(f)
            elif f.lower() == 'all' :
                # all parameters should be floated
                PEDE.logger.info('Floating all parameters')
                for p in parameters.values() :
                    p.float()
                continue
            elif f.lower() == 'allsensors' :
                # all parameters for individual sensors should be floated
                PEDE.logger.info('Floating all parameters for individual sensors')
                for p in parameters.values() :
                    if p.mp_layer_id < 23 :
                        p.float()
                continue
            else:
                # look for sensor name
                for probe_id, p in parameters.items() :
                    if p.name == f :
                        idn = prob_id
                        break
    
                if idn is None :
                    raise ValueError(f'Parameter {f} not found in parameter map.')
    
            if idn not in parameters :
                raise ValueError(f'Parameter {idn} not found in parameter map.')
    
            PEDE.logger.info(f'Floating parameter {idn}')
            parameters[idn].float()
    
        # build steering file for pede
        pede_steering_file = 'pede-steer.txt'
        with open(pede_steering_file,'w') as psf :
            # write out input mille binary files
            psf.write('CFiles\n')
            # scan each entry provided on command line,
            #  recursively entering subdirectories and including
            #  all '*.bin' files found
            for ipf in self.inputs :
                ipf = os.path.realpath(ipf)
                if os.path.isfile(ipf) and ipf.endswith('.bin') :
                    psf.write(ipf+'\n')
                elif os.path.isdir(ipf) :
                    for root, dirs, files in os.walk(ipf) :
                        for name in files :
                            if name.endswith('.bin') :
                                psf.write(os.path.join(root,name)+'\n')
    
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
                psf.write('\n!Survey constraints tu\n')
                for p, name in param_map.items() :
                    if p.module_number() == 0 :
                        continue
                    if p.direction() == 'u' and p.type() == 't' :
                        psf.write('\nMeasurement 0.0 %.3f\n' % survey_meas_tu)
                        psf.write('%s 1.0\n' & p)
                psf.write("\n\n")
            
            # apply beamspotConstraint (This I think is not correct)
            if self.beamspot_constraints:
                PEDE.logger.warn('Beamspot constraints not implemented, ignoring!')
                #psf.write(buildSteering.getBeamspotConstraints(paramMap))
                #psf.write(buildSteering.getBeamspotConstraintsFloatingOnly(pars))
                #psf.write("\n\n")
            
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
        return ['inputs', 'to_float']

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


class ApplyPedeRes(Component) :
    """! Apply a millepede.res file to a detector description

    This job component loads a result file into memory and the
    goes line-by-line through a detector description, updating
    the lines with any parameters that have updated values in the 
    result file.
    """

    def __init__(self) :
        # config
        self.java_dir

        # required job
        self.res_file = None
        self.detector = None

        # optional job
        self.bump = True
        self.force = False

        super().__init__('ApplyPedeRes')

    def required_config(self) :
        return ['java_dir']

    def required_parameters(self) :
        return ['res_file','detector']

    def optional_parameters(self) :
        return ['bump','force']

    def _detector_dir(self) :
        return os.path.join(self.java_dir, 'detector-data', 'detectors', self.detector)

    def execute(self, log_out, log_err) :
        if self.bump :
            # deduce source directory and check that it exists
            src_path = self._detector_dir()
            if not os.path.isdir(src_path) :
                PEDE.logger.error(f'Detector {self.detector} is not in hps-java')
                return 1
            
            # deduce iter value, using iter0 if there is no iter suffix
            matches = re.search('.*iter([0-9]*)', self.detector)
            if matches is None :
                PEDE.logger.error('No "_iterN" suffix on detector name.')
                return 2
            else :
                i = int(matches.group(1))
                self.detector = self.detector.replace(f'_iter{i}',f'_iter{i+1}')
    
            # deduce destination path, and make sure it does not exist
            dest_path = self._detector_dir()
            if os.path.isdir(dest_path) and not self.force :
                PEDE.logger.error(f'Detector {self.detector} already exists and so it cannot be created')
                return 3
    
            # make copy
            shutil.copytree(src_path, dest_path, dirs_exist_ok = True)
    
        # now we have bumped or not, so reconstruct detector path and check that it exists
        path = self._detector_dir()
        if not os.path.isdir(path) :
            PEDE.logger.error(f'Detector {self.detector} is not in hps-java')
            return 4
    
        # make sure compact exists
        detdesc = os.path.join(path,'compact.xml')
        if not os.path.isfile(detdesc) :
            PEDE.logger.error(f'Detector {self.detector} has no compact.xml in {path} to apply parameter to.')
            return 5
    
        # get list of parameters and their MP values
        parameters = Parameter.parse_pede_res(pede_res, skip_nonfloat=True)
    
        # modify file in place
        original_cp = detdesc + '.prev'
        shutil.copy2(detdesc, original_cp)
        f = open(detdesc,'w')
        with open(detdesc,'w') as f :
            with open(original_cp) as og :
                for line in og :
                    if 'millepede_constant' not in line :
                        f.write(line)
                        continue
    
                    line_edited = False
                    for i in parameters :
                        if str(i) in line :
                            # the parameter with ID i is being set on this line
                            # format:
                            #   (whitespace) <millepede_constant name="<id>" value="<val>"/>
    
                            # get to value
                            i_value = line.find('value')
                            pre_val = line[:i_value]
                            post_val = line[i_value:]
    
                            # get to opening "
                            quote_open = post_val.find('"')
                            pre_val += post_val[:quote_open+1]
                            post_val = post_val[quote_open+1:]
    
                            # get to closing "
                            quote_close = post_val.find('"')
                            value = post_val[:quote_close]
                            post_val = post_val[quote_close:]
    
                            new_value = f'{value} {parameters[i].compact_value()}'
    
                            if interactive :
                                doit = typer.confirm(f'Update {i} from "{value}" to "{new_value}"?')
                                if doit :
                                    f.write(f'{pre_val}{new_value}{post_val}')
                                    line_edited = True
                            else :
                                f.write(f'{pre_val}{new_value}{post_val}')
                                line_edited = True
    
                            break
                    
                    if not line_edited :
                        f.write(line)
    
        # remove original copy if bumped since the previous iteration will have the previous version
        if bump :
            os.remove(original_cp)
            
        return 0
