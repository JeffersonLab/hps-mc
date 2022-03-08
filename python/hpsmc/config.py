import sys
import os
import configparser

class ConfigItem:

    def __init__(self,
                 name,
                 env_name=None,
                 description='',
                 optional=True,
                 default=None,
                 read_from_env=False,
                 read_from_config=True):
        self.name = name
        if env_name is None:
            self.env_name = self.name.upper()
        else:
            self.env_name = env_name
        self.description = description
        self.optional = optional
        self.default = default
        self.value = None
        self.read_from_env = read_from_env
        self.read_from_config = read_from_config

    def set_from_default(self):
        print('Setting value from default: {} = {}'.format(self.name, self.default))
        self.value = self.default

    def set_from_env(self):
        if os.environ.get(self.env_name) is not None and self.read_from_env:
            self.value = os.environ.get(self.env_name, None)
            print('Set value from env: {} = {}'.format(self.env_name, self.value))

    def is_required(self):
        return not self.optional

    def is_set(self):
        return self.value is not None

    def invalid(self):
        return self.is_required() and not self.is_set()

    def to_dict(self):
        return {
            'name': self.name,
            'env_name': self.env_name,
            'description': self.description,
            'optional': self.optional,
            'default': self.default,
            'value': self.value,
            'read_from_env': self.read_from_env,
            'read_from_config': self.read_from_config}

    def __str__(self):
        return str(self.to_dict())

class Config:

    def __init__(self, section, items = {}):
        self.section = section
        self.items = {}
        if len(items):
            self.add_items(items)

    def add_items(self, items):
        for config_item in items:
            self.add_item(config_item)

    def add_item(self, config_item):
        if self.has_item(config_item.name):
            raise Exception('Config item already exists: {}:{}'.format(self.section, config_item.name));
        self.items[config_item.name] = config_item

    def get_item(self, name):
        if not self.has_item(name):
            raise Exception('Config item does not exist: {}'.format(name))
        return self.items[name]

    def has_item(self, name):
        return name in self.items.keys()

    def load_defaults(self):
        for item in self.items.values():
            item.set_from_default()

    def load_from_env(self):
        for item in self.items.values():
            item.set_from_env()

    def load(self, parser):
        """
        Load from reference to configparser object.
        """
        if parser.has_section(self.section):
            print('Found section: {}'.format(self.section))
            for name,value in parser.items(self.section):
                print('Processing item: {} = {}'.format(name, value))
                if self.has_item(name):
                    c = self.get_item(name)
                    if c.read_from_config:
                        c.value = value
                        print('Set value from config: {} = {}'.format(c.name, value))

    def get_invalid_items(self):
        invalid_items = []
        for item in self.items.values():
            if item.invalid():
                invalid_items.append(item)
        return invalid_items

    def validate(self):
        return len(self.get_invalid_items()) == 0

    def __str__(self):
        return str([p.to_dict() for p in self.items.values()])

class DummyConfigObject:

    def __init__(self):
        self.config = Config('Dummy',
                             items=[ConfigItem('something'),
                                    ConfigItem('another', optional=False),
                                    ConfigItem('mojo', optional=False, default='bad'),
                                    ConfigItem('vibes', optional=False, read_from_env=True)
                                    ])

if __name__ == '__main__':

    if len(sys.argv) < 2:
        raise Exception('not enough args')

    config_file = sys.argv[1]

    parser = configparser.ConfigParser()
    parsed = parser.read([config_file])

    obj = DummyConfigObject()
    obj_config = obj.config

    valid = obj_config.validate()
    print('valid (initial): {}'.format(valid))
    for item in obj_config.get_invalid_items():
        print(item)

    obj_config.load_defaults()
    valid = obj_config.validate()
    print('valid (after defaults): {}'.format(valid))
    for item in obj_config.get_invalid_items():
        print(item)

    obj_config.load(parser)
    print('valid (after load): {}'.format(valid))
    for item in obj_config.get_invalid_items():
        print(item)

    os.environ.__setitem__('VIBES', 'yes')
    obj_config.load_from_env()
    valid = obj_config.validate()
    print('valid (after env): {}'.format(valid))
    for item in obj_config.get_invalid_items():
        print(item)

    print(obj_config)
