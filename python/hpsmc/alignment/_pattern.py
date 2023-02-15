"""! pattern that can match one or more Parameters"""

from ._parameter import Parameter

class Pattern :
    """! Pattern that can match one or more paramters

    This is an internal class and should not be used outside of this module

    A single "pattern" is a str that is one of the unary keywords OR
    has a binary keyword followed by an "=" character followed by one
    of that keywords values.
    """

    # each one of these keywords (both binary and unary) are
    #  the names of functions in the Parameter class which return
    #  that value for the Parameter
    binary_keywords = {
        'id' : lambda v : int(v) if Parameter.idn_str_pattern.match(v) else NotImplemented,
        'half' : {'top' : 1, 'bot' : 2, 't' : 1, 'b' : 2 },
        'operation' : {'t' : 1, 'translation' : 1, 'trans' : 1, 'r' : 2, 'rotation' : 2, 'rot' : 2 },
        'direction' : {'u' : 1, 'v' : 2, 'w' : 3}
    }
    unary_keywords = [
        'top',
        'bot',
        'trans',
        'rot',
        'individual',
        'axial',
        'stereo',
    ]

    aliases = {
        'tu' : 'direction=u & operation=t',
        'rw' : 'direction=w & operation=r'
    }

    def _add_check(self, c) :
        """! add a check into our list of checks, making sure it is the correct format"""
        if not isinstance(c, tuple) or len(c) != 2 :
            raise ValueError("Checks have to be 2-tuples")

        kw = c[0].strip()

        if kw in Pattern.binary_keywords.keys() :
            # check is a binary check
            possible_values = Pattern.binary_keywords[kw]
            val = c[1].strip()
            if isinstance(possible_values, dict) :
                if val in possible_values.keys() :
                    self._checks.append((kw, possible_values[val]))
                else :
                    raise ValueError(f'Value {val} not recognized. Possible values: {possible_values.keys()}')
            else :
                v = possible_values(val)
                if v != NotImplemented :
                    self._checks.append((kw,v))
                else :
                    raise ValueError(f'Value {val} for {kw} is not recognized')
        elif kw in Pattern.unary_keywords :
            self._checks.append((kw,c[1]))
        elif kw in Pattern.aliases :
            self.__init__(Pattern.aliases[kw], first = False) 
        else :
            raise ValueError(f'{kw} is not a recognized keyword')

    def __init__(self, pattern, *, first = True) :
        if first :
            self._checks = []
            self._og_str = str(pattern)

        if not isinstance(pattern, (int,str)) :
            raise ValueError(f'Pattern {pattern} must be an int or str')

        # simplest pattern which is just the ID number
        if isinstance(pattern, int) or pattern.isnumeric() :
            # make sure pattern is casted to str for the regex testing
            self._add_check(('id',str(pattern)))
        else :
            # now complicated patterns
            ops = pattern.split('&') 
            for op in ops :
                if '=' in op :
                    # binary operation
                    split = op.split('=')
                    if len(split) != 2 :
                        raise ValueError(f'{op} does not have two sides of the equality')
                    self._add_check(tuple(split))
                else :
                    # unary operation
                    value = True
                    if op.startswith('!') :
                        op = op[1:]
                        value = False

                    self._add_check((op, value))

    def match(self, p) :
        if not isinstance(p, Parameter) :
            return NotImplemented

        for kw, val in self._checks :
            if getattr(p, kw)() != val :
                return False

        return True

    def __eq__(self, p) :
        return self.match(p)

    def __repr__(self) :
        return self._og_str

