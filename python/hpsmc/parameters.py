class Parameter(object):

    """
    Class for holding information about a component parameter (not used yet)
    """

    def __init__(self,
                 name,
                 description='',
                 optional=True,
                 default_value=None,
                 read_from_dict=True,
                 read_from_args=True):
        self.name = name
        self.description = description
        self.optional = optional
        self.default_value = default_value
        self.value = None

        self.read_from_dict = read_from_dict
        self.read_from_args = read_from_args

    def get_description(self):
        return self.description

    def get_env_name(self):
        return self.name.upper()

    def get_name(self):
        return self.name

    def get_type(self):
        return type('')

    def get_value(self):
        return self.value

    def get_str_value(self):
        return str(self.value)

    def get_default_value(self):
        return self.default_value

    def set_value_from_default(self):
        if self.default_value is not None:
            self.value = self.default_value

    def set_value(self, value):
        self.value = self.convert_value(value)

    def is_optional(self):
        return self.optional

    def is_required(self):
        return not self.optional

    def is_default(self):
        return self.value == self.default_value

    def convert_value(self, value):
        return value

    def to_dict(self):
        return {'name': self.name,
                'description': self.description,
                'optional': self.optional,
                'default_value': self.default_value,
                'value': self.value,
                'type': self.get_type(),
                'read_from_dict': self.read_from_dict,
                'read_from_args': self.read_from_args
            }

    def __str__(self):
        return str(self.to_dict())

    def is_set(self):
        return self.value is not None

    def invalid(self):
        return self.value is None and self.optional is False

class FloatParameter(Parameter):

    def get_type(self):
        return type(1.0)

    def convert_value(self, value):
        return float(value)

class IntParameter(Parameter):

    def get_type(self):
        return type(1)

    def convert_value(self, value):
        return int(value)

class BoolParameter(Parameter):

    def get_type(self):
        return type(True)

    def convert_value(self, value):
        if value in [0, False, 'false', 'False', 'no', 'No']:
            return False
        else:
            return True

class ParameterSet(object):

    def __init__(self, *args):
        self.parameters = {}
        self.add(*args)

    def add(self, *args):
        for param in args:
            if self.has(param.get_name()):
                raise Exception("Parameter already exists: {}".format(param.get_name()))
            self.parameters[param.get_name()] = param

    def get(self, name):
        if not self.has(name):
            raise Exception("Parameter does not exist: {}".format(name))
        return self.parameters[name]

    def has(self, name):
        return name in list(self.parameters.keys())

    def parameter_names(self):
        return list(self.parameters.keys())

    def __str__(self):
        return str([p.to_dict() for p in self.parameters.values()])

    def set_defaults(self):
        for param in self.parameters.values():
            param.set_value_from_default()

    def load_from_dict(self, json_dict):
        for k,v in json_dict.items():
            if self.has(k):
                p = self.get(k)
                if p.read_from_dict:
                    self.get(k).set_value(v)
                    print('load from dict: {} = {}'.format(k, v))

    def load_from_args(self, **kwargs):
        print('load from args: {}'.format(kwargs))
        for k,v in kwargs.items():
            if self.has(k):
                p = self.get(k)
                print('found param: {}'.format(k))
                if p.read_from_args:
                    p.set_value(v)
                    print('load from args: {} = {}'.format(k, v))

    def validate(self):
        return len(self.get_invalid_parameters()) > 0

    def get_invalid_parameters(self):
        invalid_parameters = []
        for p in self.parameters.values():
            if p.invalid():
                invalid_parameters.append(p)
        return invalid_parameters

class DummyParameterObject:

    def __init__(self, **kwargs):

        self.parameters = ParameterSet(Parameter     ('myparam',     'a param',          optional=False, value='foobarbaz'),
                                       FloatParameter('float',       'a float',          optional=True,  default_value=123.4),
                                       IntParameter  ('int',         'an int',           optional=True,  value=1234),
                                       BoolParameter ('bool',        'a bool',           optional=True,  value=True, default_value=False),
                                       BoolParameter ('boolfromstr', 'a bool from str',  optional=True,  value='False'),
                                       IntParameter  ('intfromstr',  'an int from str',  optional=True,  value='1234'),
                                       Parameter     ('required',    'a required param', optional=False, default_value='blerp'))

        self.parameters.load_from_args(**kwargs)

if __name__ == '__main__':

    dummy_obj = DummyParameterObject(myparam='dingus')
    params = dummy_obj.parameters

    print("Initial parameters...")
    print(params)

    example_dict = {'myparam': 'bongle',
                    'float': 5.678,
                    'int': 5678,
                    'bool': False,
                    'boolfromstr': 'True',
                    'intfromstr': '5678'}
    params.load_from_dict(example_dict)
    print("Parameters after load...")
    print(params)

    params.set_defaults()

    params.validate()
