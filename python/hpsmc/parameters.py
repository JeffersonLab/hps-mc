class Parameter(object):
    """
    Class for holding information about a component parameter (not used yet)
    """
        
    def __init__(self, name, description='', optional=True, value=None, 
                 default_value=None, read_from_init=False, read_from_params=True):
        self.name = name
        self.description = description
        self.optional = optional
        self.default_value = default_value
        if value is not None:
            self.value = self.convert_value(value)
            if type(self.value) != self.get_type():
                raise Exception("Value '%s' has wrong type %s" % (self.value, type(self.value)))
        if value is None and default_value is not None:
            self.set_from_default_value()
        #if value is None and default_value is None:
        #    raise Exception('Both value and default_value were None')
    
    def get_description(self):
        return self.description
    
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
        
    def set_from_default_value(self):
        self.value = self.default_value
    
    def set_value(self, str):
        self.value = self.convert_value(value)
        
    def is_optional(self):
        return True
 
    def is_default(self):
        return self.value == self.default_value
        
    def convert_value(self, value):
        return value
    
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
    
    def __init__(self, paramlist = []):
        self.parameters = {}
        for p in paramlist:
            self.add(p)
    
    def add(self, *args):
        for param in args:
            if param.get_name() in self.parameters.keys():
                raise Exception("Parameter with name '%s' already exists." % parameter.get_name())
        
    def get(self, name):
        if not exists(name):
            raise Exception("No parameter called '%s'" % name)
        return self.parameters[name]
    
    def has(self, name):
        return name in self.parameters.keys()
    
    def parameters(self):
        return self.parameters
    
    def parameter_names(self):
        return self.parameters.keys()
            
if __name__ == '__main__':
    p = Parameter('myparam', 'this does a thing', optional=False, value='foobarbaz', default_value='wut')
    print(vars(p))
    
    f = FloatParameter('myfloat', 'this is a float', optional=True, value=123.4, default_value=1.0)
    print(vars(f))
    
    i = IntParameter('myint', 'this is an int', optional=False, value=1234, default_value=42)
    print(vars(i))
    
    b = BoolParameter('mybool', 'this is a bool', optional=True, value=True, default_value=False)
    print(vars(b))
    
    b2 = BoolParameter('myboolfromstring', 'this is a bool from string', value='False')
    print(vars(b2))
    
    p = Parameter('anotherparam', 'this is another param', optional=True, default_value='something')
    print(vars(p))
    
    i2 = IntParameter('myintfromstr', 'this is an int from str', value='1234')
    print(vars(i2))