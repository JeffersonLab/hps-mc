"""! Utility script for printing help about component classes."""

from component import Component
from job import Job

# These are here so all available components appear in the globals dict.
from tools import *
from generators import *

# These are all interfaces and not runnable components, which should be ignored.
_ignore = ('Component', 'EventGenerator', 'StdHepTool', 'JavaTool', 'MG')


def print_component(v):
    """! Accepts Component class and prints info about it."""
    try:
        if isinstance(v, str):
            obj = eval(v)()
            v = obj.__class__
        else:
            obj = eval(v.__name__)()
        print('%s(%s)' % (v.__name__, ','.join([b.__name__ for b in v.__bases__])))
        print('')
        print('    DESCRIPTION: \n        %s' % v.__doc__.strip())
        print('')
        print('    REQUIRED PARAMETERS:')
        for pname in obj.required_parameters():
            print('        - %s' % pname)
        print('')
        print('    OPTIONAL PARAMETERS:')
        for pname in obj.optional_parameters():
            print('        - %s' % pname)
        print('')
        print('    CONFIG:')
        for cname in obj.required_config():
            print('        - %s' % cname)
        print('')
        try:
            print('    APPEND TOKEN:\n        %s' % obj.append_tok)
        except BaseException:
            pass
        print('')
        try:
            print('    OUTPUT EXTENSION:\n        %s' % obj.output_ext)
        except BaseException:
            pass
        print('')
    except Exception as e:
        print(e)


def print_components():
    """! Print info for all Component classes."""
    print("AVAILABLE COMPONENTS: ")
    for k in sorted(globals().keys()):
        v = globals()[k]
        if isinstance(v, Component.__class__):
            if v.__name__ not in _ignore:
                print('    {}'.format(v.__name__))


def print_job_script(script_path):

    job = Job()
    job.script = script_path

    # Dummy parameters so certain scripts don't crash when loaded.
    job.input_files = {'signal': 'signal',
                       'beam': 'beam'}
    job.output_files = {'dummy.out': 'dummy.out'}
    job.params['run_params'] = '4pt55'
    job.params['nevents'] = 9999

    job._load_script()

    print('JOB SCRIPT: {}'.format(job.script))
    print('')
    print('    DESCRIPTION: {}'.format(job.description))
    print('')
    print('    COMPONENTS:')
    for component in job.components:
        print('        {}'.format(component.__class__.__name__))
    print('')
    print('    PTAGS:')
    for ptag in job.ptags:
        print('        {}'.format(ptag))


if __name__ == '__main__':
    print_components()
