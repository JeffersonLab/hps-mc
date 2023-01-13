import unittest
import os

from hpsmc.generators import EventGenerator, EGS5, MG, MG4, MG5
from hpsmc.component import Component
from hpsmc.run_params import RunParameters


class TestEventGenerator(unittest.TestCase):

    def test_required_params(self):
        event_generator = EventGenerator('generator')
        self.assertEqual(event_generator.required_parameters(), ['nevents'])


class TestEGS5(unittest.TestCase):

    def test_init(self):
        egs5 = EGS5()
        self.assertEqual(egs5.bunches, 5e5)

    def test_init_kwrags(self):
        egs5 = EGS5(inputs="input/file.root", nevents=100)
        self.assertEqual(egs5.nevents, 100)
        self.assertEqual(egs5.inputs, "input/file.root")

    def test_required_params(self):
        egs5 = EGS5()
        self.assertEqual(egs5.required_parameters(), ['seed', 'run_params'])

    def test_optional_params(self):
        egs5 = EGS5()
        self.assertEqual(egs5.optional_parameters(), ['bunches', 'target_thickness'])

    def test_output_files(self):
        egs5 = EGS5()
        egs5_moller = EGS5(name='moller')
        self.assertEqual(egs5.output_files(), ['beam.stdhep'])
        self.assertEqual(egs5_moller.output_files(), ['moller.stdhep'])

    def test_set_parameters(self):
        egs5 = EGS5()
        params = {'seed': 1, 'run_params': '1pt1'}
        egs5.set_parameters(params)
        self.assertEqual(egs5.seed, 1)

    def test_setup(self):
        egs5 = EGS5()
        params = {'seed': 1, 'run_params': '1pt1', 'bunches': 1}
        egs5.set_parameters(params)
        egs5.setup()
        egs5_dir = os.getenv("HPSMC_DIR", None) + '/share/generators/egs5'
        self.assertEqual(egs5.egs5_dir, egs5_dir)
        self.assertEqual(egs5.egs5_data_dir, egs5_dir + '/data')
        self.assertEqual(egs5.egs5_config_dir, egs5_dir + '/config')
        runparameters = RunParameters(params['run_params'])
        self.assertEqual(egs5.target_z, runparameters.get("target_z"))

        with open("seed.dat", 'r') as seed_file:
            seed_vals = [line.split() for line in seed_file]
        self.assertEqual(seed_vals[0][0], str(1))
        ## this test fails because the precision of target_z in seed.dat is not the same as in run_params!!!
        ## for now: use %f here
        target_thickness = '%f' % (runparameters.get("target_z"))
        self.assertEqual(seed_vals[0][1], target_thickness)
        beam_energy = '%f' % (runparameters.get("beam_energy"))
        self.assertEqual(seed_vals[0][2], beam_energy)
        self.assertEqual(seed_vals[0][3], str(runparameters.get("num_electrons") * params['bunches']))

        # remove created symlinks
        ## \todo find better solution with total path to test directory
        os.remove('data')
        os.remove('pgs5job.pegs5inp')
        os.remove('seed.dat')


class test_MG(unittest.TestCase):

    def test_init(self):
        mg = MG('mg')
        self.assertEqual(mg.param_card, "param_card.dat")
        self.assertEqual(mg.event_types, ['unweighted', 'weighted'])
        self.assertEqual(mg.name, 'mg')

    def test_output_files(self):
        mg = MG('mg')
        self.assertEqual(mg.output_files(), ['mg_unweighted_events.lhe.gz', 'mg_events.lhe.gz'])

    def test_required_params(self):
        mg = MG('mg')
        self.assertEqual(mg.required_parameters(), ['nevents', 'run_params'])

    def test_optional_params(self):
        mg = MG('mg')
        self.assertEqual(mg.optional_parameters(), ['seed', 'param_card', 'apmass', 'map', 'mpid', 'mrhod'])

    def test_set_parameters(self):
        mg = MG('mg')
        params = {'nevents': 1, 'run_params': '1pt1', 'seed': 1, 'param_card': 'param_card.dat', 'apmass': 1, 'map': 1, 'mpid': 1, 'mrhod': 1}
        mg.set_parameters(params)
        self.assertEqual(mg.nevents, 1)
        self.assertEqual(mg.run_params, '1pt1')
        self.assertEqual(mg.seed, 1)
        self.assertEqual(mg.param_card, 'param_card.dat')
        self.assertEqual(mg.apmass, 1)
        self.assertEqual(mg.map, 1)
        self.assertEqual(mg.mpid, 1)
        self.assertEqual(mg.mrhod, 1)
        self.assertEqual(mg.run_card, 'run_card_' + params['run_params'] + '.dat')

    def test_setup(self):
        mg = MG('mg')
        params = {'nevents': 1, 'run_params': '1pt1', 'seed': 1, 'param_card': 'param_card.dat', 'apmass': 1, 'map': 1, 'mpid': 1, 'mrhod': 1}
        mg.set_parameters(params)
        mg.setup()
        mg_dir = os.getenv("HPSMC_DIR", None) + '/share/generators'
        self.assertEqual(mg.madgraph_dir, mg_dir)


class test_MG4(unittest.TestCase):

    def test_init(self):
        mg4 = MG4()
        self.assertEqual(mg4.name, 'ap')
        self.assertEqual(mg4.event_types, ['unweighted', 'weighted'])

    def test_init_BH(self):
        mg4 = MG4('BH')
        self.assertEqual(mg4.name, 'BH')

    def test_init_RAD(self):
        mg4 = MG4('RAD')
        self.assertEqual(mg4.name, 'RAD')

    def test_init_TM(self):
        mg4 = MG4('TM')
        self.assertEqual(mg4.name, 'TM')

    def test_init_trigger(self):
        mg4 = MG4('trigg')
        self.assertEqual(mg4.name, 'trigg')

    def test_init_tritrig(self):
        mg4 = MG4('tritrig')
        self.assertEqual(mg4.name, 'tritrig')

    def test_init_wab(self):
        mg4 = MG4('wab')
        self.assertEqual(mg4.name, 'wab')

    def test_init_exception(self):
        self.assertRaises(Exception, lambda: MG4('some_invalid_name'))

    def test_get_install_dir(self):
        mg4 = MG4()
        self.assertEqual(mg4.get_install_dir(), os.getenv("HPSMC_DIR", None) + '/share/generators/madgraph4/src')


class test_MG5(unittest.TestCase):

    def test_init(self):
        mg5 = MG5()
        self.assertEqual(mg5.name, 'tritrig')
        self.assertEqual(mg5.event_types, ['unweighted', 'weighted'])

    def test_init_BH(self):
        mg5 = MG5('BH')
        self.assertEqual(mg5.name, 'BH')

    def test_init_RAD(self):
        mg5 = MG5('RAD')
        self.assertEqual(mg5.name, 'RAD')

    def test_init_simp(self):
        mg5 = MG5('simp')
        self.assertEqual(mg5.name, 'simp')

    def test_init_exception(self):
        self.assertRaises(Exception, lambda: MG5('some_invalid_name'))

    def test_get_install_dir(self):
        mg5 = MG5()
        self.assertEqual(mg5.get_install_dir(), os.getenv("HPSMC_DIR", None) + '/share/generators/madgraph5/src')
