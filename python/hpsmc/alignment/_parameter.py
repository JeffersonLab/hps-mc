"""! representation of alignment parameters"""

import re

class Parameter :
    """!
    Representation of a single alignment parameter

    This class also contains helpful functions for operating on sets of alignment
    parameters e.g. parsing the map file or pede res file

    Attributes
    ----------
    id : int
        pede ID number as written in compact.xml, pede steering, and map files
    name : str
        human-readable name as written in map file
    half : int
        1 is for top and 2 is for bottom
    trans_rot : int
        1 is for translation and 2 is for rotation
    direction : int
        1 is for 'u', 2 is for v, and 3 is for w
    mp_layer_id : int
        "layer" ID number in millepede (i.e. axial and stereo sensors are separated)
    val : float
        value of parameter (if loaded from res file)
    error : float
        error of parameter (if loaded from res file)
    active : bool
        true if parameter is floating, false otherwise
    """

    idn_str_pattern = re.compile('^[12][12][123][0-9][0-9]$')

    def __init__(self, idn, name, half, trans_rot, direction, mp_layer_id) :
        self._id = int(idn)
        self._name = name
        self._half = int(half) # 1 or 2
        self._trans_rot = int(trans_rot)
        self._direction = int(direction)
        self._mp_layer_id = int(mp_layer_id)

        self._val = 0.0
        self._error = -1.0
        self._active = False

    def float(self, yes = True) :
        """!Set whether this parameter is floating/active or not"""
        self._active = yes

    def module(self) :
        """!Get the module number from the millepede layer number

        We group sensors in pairs to form modules and the layer number
        for millepede counts up from the furthest upstream sensor. Thus,
        we do integer-division by two to get the module number.
        """
        if self._mp_layer_id < 9 :
            return self._mp_layer_id // 2
        else :
            return 4 + (self._mp_layer_id - 9) // 4

    def layer(self) :
        """!Get the human layer number

        This is the module number but shifted by one since the first layer is
        layer 1
        """
        return self.module() + 1

    def id(self) :
        return self._id

    def direction(self) :
        return self._direction

    def individual(self) :
        """!Get whether  this Parameter represents a single sensor (True)
        or a structural component holding two or more sensors (False)
        """
        return self._mp_layer_id < 23

    def translation(self) :
        """!True if Parameter represents a translation"""
        return self._trans_rot == 1

    def rotation(self) :
        """!True if Parameter represents a rotation"""
        return self._trans_rot == 2

    def top(self) :
        """!Does this parameter represent a component on the top half (True)
        or bottom (False)
        """
        return (self._half == 1)

    def bottom(self) :
        """!True if Parameter is in bottom half, False if in top half"""
        return (self._half == 2)

    def axial(self) :
        """!Get whether this Parameter represents a single axial sensor (True)
        or something else (False)
        """
        return self.individual() and (self._mp_layer_id % 2 == 0)

    def stereo(self) :
        """!Get whether this Parameter represents a single stereo sensor (True)
        or something else (False)
        """
        return self.individual() and (self._mp_layer_id % 2 == 1)

    def front(self) :
        """!True if Parameter is single sensor in front half, False otherwise"""
        return self.individual() and (self._mp_layer_id < 9)

    def back(self) :
        """!True if Parameter is single sensor in back half, False otherwise"""
        return self.individual() and (self._mp_layer_id > 8)

    def hole(self) :
        """!True if Parameter is a single sensor in back half on the hole side, Flase otherwise"""
        return self.back() and (self._mp_layer_id % 4 == 1 or self._mp_layer_id % 4 == 2)

    def slot(self) :
        """!True if Parameter is a single sensor in back half on the slot side, Flase otherwise"""
        return self.back() and (self._mp_layer_id % 4 == 0 or self._mp_layer_id % 4 == 3)

    def from_map_file_line(line) :
        """parse a line from the map file

        we assume that the constructor's arguments are in the same
        order as a line in the sensor map file
        """
        return Parameter(*line.split())

    def from_idn(idn) :
        """! Deduce the categorical flags from the ID number
        
        Each ID number is five digis.
        In regex terms...

        [12][12][123][0-9][0-9]
         |             |- last two digis are sensor ID number 
         |        |------ direction 1==u, 2==v, 3==w
         |    |---------- transformation 1==translation, 2==rotation
         |--------------- detector half 1==top, 2==bottom

        So we just need to break it down by modulo and integer
        division /OR/ do some str conversion nonsense in python.
        """

        idn = str(idn)
        if len(idn) != 5 :
            raise ValueError(f'Bad ID Number: {idn} is not five digis')
        
        if not Parameter.idn_str_pattern.match(idn) :
            raise ValueError(f'Bad ID Number: {idn} does not match the ID pattern')

        # idn is good pattern, procede with str hackiness
        digits = [*idn] # digits is list of characters in str
        # convert digits into flag values
        half = int(digits[0])
        trans_rot = int(digits[1])
        direction = int(digits[2])
        mp_layer_id = int(digits[3]+digits[4])
        return Parameter(int(idn), idn, half, trans_rot, direction, mp_layer_id)

    def parse_map_file(map_filepath) :
        """! load the entire parameter set from a map file

        Returns
        -------
        dict
            map from ID number to a Parameter
        """
        parameters = {}
        with open(map_filepath) as mf :
            for line in mf :
                # skip header line
                if 'MilleParameter' in line :
                    continue
                p = Parameter.from_map_file_line(line)
                parameters[p.id] = p
        return parameters

    def __from_res_file_line(self, line) :
        """! Assumes line is for the same parameter as stored in self

        A line in the pede result file has either 3 or 5 columns.

        1. ID
        2. value
        3. activity (0.0 if floating, -1.0 if not)
        4. (if active) value
        5. (if active) error in value
        
        We ignore the first column and assume that we are only calling
        this function if the line has already been deduced to correspond
        to the parameter we represent.
        """
        elements = line.split()
        # elements[0] is the ID number and we assume it is correct
        self._val = float(elements[1])
        self._active = (float(elements[2]) >= 0.0)
        if len(elements) > 4 :
            self._val = float(elements[3])
            self._error = float(elements[4])

    def parse_pede_res(res_file, destination = None, skip_nonfloat = False) :
        """! parse a pede results file
        
        Parse the results file into a dictionary. If no destination dictionary
        is provided, a new dictionary is created with the ID numbers as keys
        and Parameter instances as values. Since this mapping is created without
        the sensor mapping, the rest of the Parameter attributes are assigned
        non-sensical values. 

        Parameters
        ----------
        res_file : str
            path to results file we are going to parse
        destination : dict, optional
            if provided, load the values from the file into parameters in this dict
        skip_nonfloat : bool, optional
            skip non-floating parameters 
        """
        parameters = {}
        with open(res_file) as rf :
            for line in rf :
                # skip header line
                if 'Parameter' in line :
                    continue
                idn = int(line.split()[0])
                if destination is None :
                    p = Parameter.from_idn(idn)
                    p.__from_res_file_line(line)
                    if p.active or not skip_nonfloat :
                        parameters[p.id] = p
                else :
                    if idn not in destination :
                        raise ValueError(f'Attempting to load parameter {idn} which is not in parameter map')
                    if destination[idn].active or not skip_nonfloat :
                        destination[idn].__from_res_file_line(line)
        return parameters if destination is None else None

    def pede_format(self) :
        """! Print this parameter as it should appear in the pede steering file"""
        return f'{self._id} {self._val} {0.0 if self._active else -1.0} {self._name}'

    def compact_value(self) :
        """! Print the value of this parameter as it should be inserted into the compact

        **including** the operator (either + or -)

        This is where we handle whether the sign flips (translations) or doesn't (rotations)
        """
        # rotation, same sign as value
        op = '+' if self._val > 0 else '-'
        if self._trans_rot == 1 :
            # translation, flip operator
            op = '-' if self._val > 0 else '+'

        return f'{op} {abs(self._val)}'

    def __repr__(self) :
        """! Representation of this parameter"""
        return str(self._id)

    def __str__(self) :
        """! Human printing of this parameter"""
        s = repr(self)
        if self._trans_rot == 1 :
            # translation
            #   stored as mm, print as um
            s += f' {self._val*1000} +- {self._error*1000} um'
        else :
            # rotation
            #   stored as rad, print as mrad
            s += f' {self._val*1000} +- {self._error*1000} mrad'
        return s

