"""! pattern that can match one or more Parameters"""

from ._parameter import Parameter


class Pattern:
    """! Pattern that can match one or more paramters

    This is an internal class and should not be used outside of this module

    A single "pattern" is a str that is one of the unary keywords OR
    has a binary keyword followed by an "=" character followed by one
    of that keywords values. All whitespace in a pattern is removed so
    spaces can be added for easier readability.

    Syntax
    ------
    Each pattern is made up of one or more operations where each operation
    is separated by an ampersand "&" to reflect that a Pattern takes the
    logical-and of the operations listed.

      'op1 & op2 & op3 ...'

    Each operation can be one of three things.

    1. An ID number.
    2. A unary operation.
    3. A binary operation.

    ID numbers are the 5-digit millepede ID number for that parameter which encodes all
    of the location information within it. It specifies a single millepede parameter.

    Unary operations are special keywords that correspond to different logical grouping
    of parameters. Some unary operations correspond to functions returning True/False
    of the Parameter class while others are simple shortenings of common Pattern strings
    (a.k.a. "aliases").

    Binary operations are a string formatted like "kw = val" where a *single* equal
    sign is used. kw is a keyword corresponding to select functions from the Parameter
    class while val is a keyword corresponding to specific options for each kw.
    """

    NUM_MODULES = 7

    def __validate_id(i):
        """!Make sure input is a valid ID number, otherwise return NotImplemented

        A valid ID number is a 5-digit integer matching the Parameter.idn_str_pattern
        regular expression.
        """
        if Parameter.idn_str_pattern.match(i):
            return int(i)
        else:
            return NotImplemented

    def __validate_module(m):
        """!Make sure input is a valid module number, otherwise return NotImplemented

        A valid module number is an index counting from 0 at the front of the detector
        up to NUM_MODULES-1
        """
        try:
            m = int(m)
            if m < 0 or m > NUM_MODULES-1:
                return NotImplemented
            return m
        except ValueError:
            return NotImplemented

    def __validate_layer(l):
        """!Make sure input is a valid layer number, otherwise return NotImplemented

        A valid layer number is an index counting axial/stereo pairs from 1 at
        the front of the detector up to NUM_MODULES
        """
        try:
            l = int(l)
            if l < 1 or l > NUM_MODULES:
                return NotImplemented
            return l
        except ValueError:
            return NotImplemented

    # each one of these keywords (both binary and unary) are
    #  the names of functions in the Parameter class which return
    #  that value for the Parameter
    binary_keywords = {
        'id': __validate_id,
        'module': __validate_module,
        'layer': __validate_layer,
        'direction': {'u': 1, 'v': 2, 'w': 3}
    }
    unary_keywords = [
        'top',
        'bottom',
        'translation',
        'rotation',
        'individual',
        'axial',
        'stereo',
        'front',
        'back',
        'hole',
        'slot'
    ]

    # aliases are just a shortening of common Pattern strings
    #    that are drop-in replacements
    aliases = {
        'bot': 'bottom',
        'trans': 'translation',
        'rot': 'rotation',
        'tu': 'direction=u & translation',
        'rw': 'direction=w & rotation'
    }

    def _add_check(self, c):
        """! add a check into our list of checks, making sure it is the correct format"""
        if not isinstance(c, tuple) or len(c) != 2:
            raise ValueError("Checks have to be 2-tuples")

        kw = c[0].strip()

        if kw in Pattern.binary_keywords.keys():
            # check is a binary check
            possible_values = Pattern.binary_keywords[kw]
            val = c[1].strip()
            if isinstance(possible_values, dict):
                if val in possible_values.keys():
                    self._checks.append((kw, possible_values[val]))
                else:
                    raise ValueError(f'Value {val} for {kw} not recognized. Possible values:\n{possible_values.keys()}')
            else:
                v = possible_values(val)
                if v != NotImplemented:
                    self._checks.append((kw, v))
                else:
                    raise ValueError(f'Value {val} for {kw} is not recognized.')
        elif kw in Pattern.unary_keywords:
            self._checks.append((kw, c[1]))
        elif kw in Pattern.aliases:
            self.__init__(Pattern.aliases[kw], first=False)
        else:
            raise ValueError(f'{kw} is not a recognized keyword')

    def __init__(self, pattern, *, first=True):
        if first:
            self._checks = []
            self._og_str = str(pattern)

        if not isinstance(pattern, (int, str)):
            raise ValueError(f'Pattern {pattern} must be an int or str')

        # simplest pattern which is just the ID number
        if isinstance(pattern, int) or pattern.isnumeric():
            # make sure pattern is casted to str for the regex testing
            self._add_check(('id', str(pattern)))
        else:
            # now complicated patterns
            ops = pattern.split('&')
            for op in ops:
                if '=' in op:
                    # binary operation
                    split = op.split('=')
                    if len(split) != 2:
                        raise ValueError(f'{op} does not have two sides of the equality')
                    self._add_check(tuple(split))
                else:
                    # unary operation
                    value = True
                    if op.startswith('!'):
                        op = op[1:]
                        value = False

                    self._add_check((op, value))

    def match(self, p):
        if not isinstance(p, Parameter):
            return NotImplemented

        for kw, val in self._checks:
            if getattr(p, kw)() != val:
                return False

        return True

    def __eq__(self, p):
        return self.match(p)

    def __repr__(self):
        return self._og_str
