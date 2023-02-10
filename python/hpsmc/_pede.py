"""! running pede from in hps-mc jobs"""

from .component import Component
from ._alignment import Parameter

class PEDE(Component):
    """! Run pede minimizer over input bin files for alignment

    """

    def __init__(self, **kwargs) :
        self._pede_steering_file = None
        self.to_float = []

        self.subito = False
        self.year = 2019
        self.constraint_file = None
        self.previous_fit = None
        self.beamspot_constraints = False
        self.survey_constraints = False

        super().__init__('pede',
                command='pede', 
                **kwargs
                )

    def _write_pede_steering_file(self) :
        param_map_file = '/full/path/to/hpsSvtParamMap.txt'
        if self.year == 2019 :
            param_map_file = '/full/path/to/hpsSvtParamMap_2019.txt'

        parameters = Parameter.parse_map_file(param_map_file)

        if self.previous_fit is not None :
            Parameter.parse_pede_res(previous_fit, destination = parameters, skip_nonfloat = False)

        for f in self.to_float :
            idn = None
            if f.isnumeric() :
        # define which parameters are floating
        for f in to_float :
            idn = None
            if f.isnumeric() :
                # string is a number, assume it is the idn
                idn = int(f)
            elif f.lower() == 'all' :
                # all parameters should be floated
                for p in parameters.values() :
                    p.float()
                continue
            elif f.lower() == 'allsensors' :
                # all parameters for individual sensors should be floated
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
    
            parameters[idn].float()
    
        # build steering file for pede
        pede_steering_file = '/full/path/to/steer.txt'
        with open(pede_steering_file,'w') as psf :
            # write out input mille binary files
            psf.write('CFiles\n')
            # scan each entry provided on command line,
            #  recursively entering subdirectories and including
            #  all '*.bin' files found
            for ipf in input_file :
                ipf = os.path.realpath(ipf)
                if os.path.isfile(ipf) and ipf.endswith('.bin') :
                    psf.write(ipf+'\n')
                elif os.path.isdir(ipf) :
                    for root, dirs, files in os.walk(ipf) :
                        for name in files :
                            if name.endswith('.bin') :
                                psf.write(os.path.join(root,name)+'\n')
    
            # external constraint file
            if constraint_file is not None :
                psf.write('\n')
                psf.write('!Constraint file\n')
                psf.write(constraint_file+'\n')
    
            # list parameters
            psf.write('\nParameter\n')
            for i, p in parameters.items() : 
                psf.write(p.pede_format() + '\n')
    
            # survey constraints
            if survey_constraints :
                psf.write('\n!Survey constraints tu\n')
                for p, name in param_map.items() :
                    if p.module_number() == 0 :
                        continue
                    if p.direction() == 'u' and p.type() == 't' :
                        psf.write('\nMeasurement 0.0 %.3f\n' % survey_meas_tu)
                        psf.write('%s 1.0\n' & p)
                psf.write("\n\n")
            
            # apply beamspotConstraint (This I think is not correct)
            if beamspot_constraints:
                #f.write(buildSteering.getBeamspotConstraints(paramMap))
                psf.write(buildSteering.getBeamspotConstraintsFloatingOnly(pars))
                psf.write("\n\n")
            
            psf.write("\n\n")
            # determine MP minimization settings
            with open(cfg.cfg().pede_minimization) as minfile :
                for line in minfile :
                    psf.write(line)
        
        return pede_steering_file
    
    
    def _print_pede_res(self) :
        # print parameters that were floated so user can see results
        Parameter.parse_pede_res(os.path.join(cfg.cfg().scratch,'millepede.res'),
            destination=parameters, skip_nonfloat=True)
        print('Deduced Parameters')
        for i, p in parameters.items() :
            if p.active :
                print(f'  {p}')
        return

    def _move_pede_output(self) :
        # move output to destination directory
        for ext in ['res','eve','log','his'] :
            f=os.path.join(cfg.cfg().scratch,f'millepede.{ext}')
            if os.path.isfile(f) :
                shutil.copy2(f,os.path.join(out_dir,prefix+f'millepede.{ext}'))
        return

    def required_parameters() :
        return ['inputs', 'to_float']

    def optional_parameters() :
        return ['subito','year','constraint_file','previous_fit']

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
        self._print_pede_res()
        self._move_pede_output()

