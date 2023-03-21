"""! applying parameter values to detector compact.xml in hps-mc jobs"""

import shutil
import re
import os
import logging
import json

logger = logging.getLogger('alignment.apply')

from hpsmc.component import Component
from ._parameter import Parameter
from ._pattern import Pattern

class _DetectorEditor(Component) :
    """! Abstract component to hold shared functionality for
    compoents that edit the detectors in hps-java

    **This component should never be used directly.**

    Required Config:
    ```
    [<Component>]
    java_dir = /full/path/to/hps-java
    ```

    Required Job:
    - **detector**: name of detector we are starting from

    Optional Parameters:
    - **next_detector**: name of detector to write to
    - **force**: ignore if the detector we are writing to already exists
    """

    def __init__(self, name, **kwargs) :
        # config
        self.java_dir = None

        # required job
        self.detector = None

        # optional job
        self.next_detector = None
        self.force = False

        super().__init__(name, **kwargs)

    def required_config(self) :
        return ['java_dir']

    def required_parameters(self) :
        return ['detector']

    def optional_parameters(self) :
        return ['force','next_detector']

    def _detector_dir(self, det_name) :
        return os.path.join(self.java_dir, 'detector-data', 'detectors', det_name)

    def _deduce_next_detector(self, bump = False) :
        """! deduce what the next detector should be given how the component has been configured

        The component parameter **bump** is an argument here since it is only
        a valid parameter for some components inheriting from this function.
        """
        if bump or self.next_detector is not None :
            logger.info('Creating new detector directory.')
            # deduce source directory and check that it exists
            src_path = self._detector_dir(self.detector)
            if not os.path.isdir(src_path) :
                raise ValueError(f'Detector {self.detector} is not in hps-java')
            
            if self.next_detector is None :
                logger.info('Deducing next detector name from current name')
                # deduce iter value, using iter0 if there is no iter suffix
                matches = re.search('.*iter([0-9]*)', self.detector)
                if matches is None :
                    raise ValueError('No "_iterN" suffix on detector name.')
                else :
                    i = int(matches.group(1))
                    self.next_detector = self.detector.replace(f'_iter{i}',f'_iter{i+1}')

            logger.info(f'Creating new detector named "{self.next_detector}"')
        else :
            logger.info(f'Operating on assumed-existing detector "{self.detector}"')
            self.next_detector = self.detector

    def _to_compact(self, parameter_set, detname, save_prev = True, prev_ext = 'prev'):
        """! write the input parameter set into the input compact.xml file

        Update the millepede parameters in the destination compact.xml with the
        parameters stored in the parameter_set map.

        Parameters
        ----------
        parameter_set : dict
            dict mapping parameter ID number to Parameter instance
        detname : str
            name of detector whose compact.xml we should edit
        save_prev : bool, optional
            whether to save a copy of the original compact.xml before we edited it
        prev_ext : str, optional
            extension to add to the original compact.xml if it is being saved
        """

        def _change_xml_value(line, key, new_val, append = True) :
            """!change an XML line to have a new value

            Assuming that the key and value are on the same line,
            we can do some simple string arithmetic to find which
            part of the string needs to be replaced.

            Format:
                
                xml-stuff key="value" other-xml-stuff

            We make the replacement by finding the location of 'key'
            in the line, then finding the next two quote characters.
            The stuff in between those two quote characters is replaced
            or appended with new_val and everything else in the line 
            is left the same.

            The updated line is returned as a new string.
            """

            i_key = line.find(key)
            pre_value = line[:i_key]
            post_value = line[i_key:]

            quote_open = post_value.find('"')+1
            pre_value += post_value[:quote_open]
            post_value = post_value[quote_open:]

            quote_close = post_value.find('"')
            og_value = post_value[:quote_close]
            post_value = post_value[quote_close:]

            new_value = f'{new_val}'
            if append :
                new_value = f'{og_value} {new_val}'

            return f'{pre_value}{new_value}{post_value}'


        # modify file in place
        dest = os.path.join(self._detector_dir(detname),'compact.xml')
        if not os.path.isfile(dest) :
            raise ValueError(f'{detname} does not have a compact.xml to modify.')
        logger.info(f'Writing compact.xml at {dest}')
        original_cp = dest + '.' + prev_ext
        shutil.copy2(dest, original_cp)
        f = open(dest,'w')
        with open(dest,'w') as f :
            with open(original_cp) as og :
                for line in og :
                    if 'info name' in line :
                        # update detector name
                        f.write(_change_xml_value(line, 'name', detname, append = False))
                        line_edited = True
                        continue

                    if 'millepede_constant' not in line :
                        f.write(line)
                        continue
    
                    line_edited = False
                    for i in parameter_set :
                        if str(i) in line :
                            # the parameter with ID i is being set on this line
                            f.write(_change_xml_value(
                                line, 'value', parameters[i].compact_value(), append = True
                            ))
                            line_edited = True
                            break
                    
                    if not line_edited :
                        f.write(line)
    
        # remove original copy if bumped since the previous iteration will have the previous version
        if not save_prev :
            os.remove(original_cp)
        

    def _update_readme(self, detname, msg) :
        """! Update the readme for the passed detector name

        Includes a timestamp at the end of the passed message.
        """

        # update/create a README to log how this detector has evolved
        log_path = os.path.join(self._detector_dir(detname), 'README.md')
        logger.info(f'Updating README.md at {log_path}')
        with open(log_path, 'a') as log :
            from datetime import datetime
            log.write(f'# {detname}\n')
            log.write(msg)
            log.write(f'_auto-generated note on {str(datetime.now())}_\n')
            log.flush() # need manual flush since we leave after this
        return
        
class ApplyPedeRes(_DetectorEditor) :
    """! Apply a millepede.res file to a detector description

    This job component loads a result file into memory and the
    goes line-by-line through a detector description, updating
    the lines with any parameters that have updated values in the 
    result file.

    Required Config:
    ```
    [ApplyPedeRes]
    java_dir = /full/path/to/hps-java
    ```

    Required Parameters:
    - **detector**: name of detector to apply parameters to

    Optional Parameters:
    - **res\_file**: path to millepede results file (default: 'millepede.res')
    - **bump**: generate the next detector name by incrementing the iter number of the input detector (default: True)
    - **force**: override the next detector path (default: False)
    - **next\_detector**: provide name of next detector, preferred over **bump** if provided (default: None)
    """


    def __init__(self) :
        # optional job
        self.res_file = 'millepede.res'
        self.bump = True

        # hidden job parameters
        self.to_float = 'UNKNOWN'

        super().__init__('ApplyPedeRes')

    def optional_parameters(self) :
        return super().optional_parameters() + ['res_file','bump','to_float']

    def cmd_line_str(self) :
        return 'custom python execute'

    def execute(self, log_out, log_err) :
        self._deduce_next_detector(self.bump)

        # deduce destination path, and make sure it does not exist
        dest_path = self._detector_dir(self.next_detector)
        if os.path.isdir(dest_path) and not self.force:
            raise ValueError(f'Detector {self.next_detector} already exists and so it cannot be created. Use "force" to overwrite an existing detector.')

        # make copy if the destination is not the same as the origin
        if self.next_detector != self.detector :
            # we already checked if the destination exists and the dirs_exist_ok parameter
            #   to shutil.copytree is only available in newer python versions
            #   so we remove the destination here now that we know (1) we can if it exists
            #   and (2) it is not the same as the source
            if os.path.isdir(dest_path):
                shutil.rmtree(dest_path)
            shutil.copytree(self._detector_dir(self.detector), dest_path)

        # remove invalid copies of LCDD from next_detector path
        for detname in (self.detector, self.next_detector) :
            lcdd_file = os.path.join(self._detector_dir(self.next_detector), f'{detname}.lcdd')
            if os.path.isfile(lcdd_file) :
                os.remove(lcdd_file)

        # remove invalid properties file
        properties_file = os.path.join(self._detector_dir(self.next_detector), 'detector.properties')
        if os.path.isfile(properties_file) :
            os.remove(properties_file)
    
        # get list of parameters and their MP values
        parameters = Parameter.parse_pede_res(self.res_file, skip_nonfloat=True)

        self._to_compact(parameters, self.next_detector)
        self._update_readme(self.next_detector, f"""
Compact updated by applying results from a run of pede

### Parameters Floated
{json.dumps(self.to_float, indent = 2)}

""")
        return 0

class WriteMisalignedDet(_DetectorEditor) :
      """! write a detector intentionally misaligned relative to another one

      Required Config:
      ```
      [WriteMisalignedDet]
      java_dir = /full/path/to/hps-java
      param_map = /full/path/to/parameter/map.txt
      ```

      Required Job:
      - **detector** : name of detector to base our misalignment on
                       (and write to if no **next\_detector** is given)
      - **parameters** : dictionary of parameters to the change that should be applied
        - each key in this dictionary is a hpsmc.alignment._pattern.Pattern so it can specify a single parameter or a group of parameters
      """

      def __init__(self) :
          # required config
          self.param_map = None

          # required job
          self.parameters = None

          super().__init__('WriteMisalignedDet')

      def required_config(self) :
          return super().required_config() + ['param_map']

      def required_parameters(self) :
          return super().required_parameters() + ['parameters']

      def cmd_line_str(self) :
          return 'custom python execute'

      def execute(self, out, err) :
          # translate pattern strings from JSON into Pattern objects
          patterns = [
              (Pattern(parameter_str), val_change)
              for parameter_str, val_change in self.parameters.items()
              ]
          
          full_parameters = Parameter.parse_map_file(self.param_map)

          parameters_to_apply = {}
          for idn, param in full_parameters.items() :
              for pattern, val_change in patterns :
                  if pattern.match(param) :
                      parameters_to_apply[idn] = param
                      parameters_to_apply[idn].value = val_change
                      break

          self._deduce_next_detector()

          src_det = self._detector_dir(self.detector)
          if not os.path.isdir(src_det) :
              raise ValueError(f'{src_det} detector does not exist.')

          dest_same_as_src = (self.next_detector is None)
          if dest_same_as_src and not self.force :
              raise ValueError(f'Need to explicitly use the "force" parameter if you want to write to an existing detector.')

          if dest_same_as_src :
              dest_det = src_det
          else :
              dest_det = self._detector_dir(self.next_detector) 
              if not os.path.isdir(dest_det) :
                  shutil.copytree(src_det, dest_det)
              elif not self.force :
                  raise ValueError('{dest_det} detector already exists. Use "force" if you want to write to an existing detector.') 

          self._to_compact(parameters_to_apply, dest_det, save_prev = self.force)
          self._update_readme(self.detector if dest_same_as_src else self.next_detector,
              f"""
Detector written by applying an intentional misalignment to {self.detector}.

### Misalignment Applied
{json.dumps(self.parameters, indent=2)}

""")

class ConstructDetector(_DetectorEditor) :
    """! construct an LCDD from a compact.xml and recompile necessary parts of hps-java
    
    This is a Component interface to the hps-mc-construct-detector script.

    Required Config:
    ```
    [ConstructDetector]
    java_dir = /full/path/to/hps-java
    hps_java_bin_jar = /full/path/to/hps-java/bin.jar
    ```

    Required Parameters:
    - **detector**: name of detector to construct (unless next\_detector is provided)

    Optional Parameters:
    - **bump**: generate the next detector name by incrementing the iter number of the input detector (default: True)
    - **force**: override the next detector path (default: False)
    - **next\_detector**: provide name of next detector, preferred over **bump** if provided (default: None)
    """

    def __init__(self) :
        # config
        self.hps_java_bin_jar = None

        # optional job
        #    only used when in the same job as ApplyPedeRes
        self.bump = True

        # detector we will actuall construct
        self.detector_to_construct = None

        super().__init__('ConstructDetector',
                         command='hps-mc-construct-detector')

    def required_config(self) :
        return super().required_config() + ['hps_java_bin_jar']

    def optional_parameters(self) :
        return super().optional_parameters() + ['bump']

    def setup(self) :
        """Called after configured but before running

        We deduce which detector we will be running with,
        attempting to mimic the logic in ApplyPedeRes.execute
        so that we compile the same detector that pede results
        were written into.
        """
        self._deduce_next_detector(self.bump)
        
    def cmd_args(self) :
        return [ self.next_detector, '-p', self.java_dir, '-jar', self.hps_java_bin_jar ]

