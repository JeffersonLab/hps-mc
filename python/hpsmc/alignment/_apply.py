"""! applying pede results to a detector in hps-mc jobs"""

import shutil
import re
import os
import logging

from hpsmc.component import Component
from ._parameter import Parameter

class ApplyPedeRes(Component) :
    """! Apply a millepede.res file to a detector description

    This job component loads a result file into memory and the
    goes line-by-line through a detector description, updating
    the lines with any parameters that have updated values in the 
    result file.
    """

    logger = logging.getLogger('ApplyPedeRes')

    def __init__(self) :
        # config
        self.java_dir = None

        # required job
        self.detector = None

        # optional job
        self.res_file = 'millepede.res'
        self.bump = True
        self.force = False

        super().__init__('ApplyPedeRes')

    def required_config(self) :
        return ['java_dir']

    def required_parameters(self) :
        return ['detector']

    def optional_parameters(self) :
        return ['res_file','bump','force']

    def _detector_dir(self) :
        return os.path.join(self.java_dir, 'detector-data', 'detectors', self.detector)

    def cmd_line_str(self) :
        return 'custom python execute'

    def execute(self, log_out, log_err) :
        if self.bump :
            # deduce source directory and check that it exists
            src_path = self._detector_dir()
            if not os.path.isdir(src_path) :
                ApplyPedeRes.logger.error(f'Detector {self.detector} is not in hps-java')
                return 1
            
            # deduce iter value, using iter0 if there is no iter suffix
            matches = re.search('.*iter([0-9]*)', self.detector)
            if matches is None :
                ApplyPedeRes.logger.error('No "_iterN" suffix on detector name.')
                return 2
            else :
                i = int(matches.group(1))
                self.detector = self.detector.replace(f'_iter{i}',f'_iter{i+1}')
    
            # deduce destination path, and make sure it does not exist
            dest_path = self._detector_dir()
            if os.path.isdir(dest_path) and not self.force :
                ApplyPedeRes.logger.error(f'Detector {self.detector} already exists and so it cannot be created')
                return 3
    
            # make copy
            shutil.copytree(src_path, dest_path, dirs_exist_ok = True)
    
        # now we have bumped or not, so reconstruct detector path and check that it exists
        path = self._detector_dir()
        if not os.path.isdir(path) :
            ApplyPedeRes.logger.error(f'Detector {self.detector} is not in hps-java')
            return 4
    
        # make sure compact exists
        detdesc = os.path.join(path,'compact.xml')
        if not os.path.isfile(detdesc) :
            ApplyPedeRes.logger.error(f'Detector {self.detector} has no compact.xml in {path} to apply parameter to.')
            return 5
    
        # get list of parameters and their MP values
        parameters = Parameter.parse_pede_res(self.res_file, skip_nonfloat=True)
    
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
    
                            f.write(f'{pre_val}{new_value}{post_val}')
                            line_edited = True
                            break
                    
                    if not line_edited :
                        f.write(line)
    
        # remove original copy if bumped since the previous iteration will have the previous version
        if self.bump :
            os.remove(original_cp)
            
        return 0
