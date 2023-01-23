import unittest
import configparser
import os

from hpsmc.tools import SLIC


class TestSLIC(unittest.TestCase):

    def test_init(self):
        slic = SLIC()
        self.assertEqual(slic.name, 'slic')

    def test_config(self):
        slic = SLIC()
        parser = configparser.ConfigParser()
        config_file = ['.hpsmc_test_cfg']
        parser.read(config_file)
        slic.config(parser)
        self.assertEqual(slic.detector_dir, 'some/detector/dir')
        self.assertEqual(slic.hps_fieldmaps_dir, 'some/fieldmaps/dir')

    def test_optional_params(self):
        slic = SLIC()
        self.assertEqual(slic.optional_parameters(), ['nevents', 'macros', 'run_number'])

    def test_required_params(self):
        slic = SLIC()
        self.assertEqual(slic.required_parameters(), ['detector'])

    def test_required_configs(self):
        slic = SLIC()
        self.assertEqual(slic.required_config(), ['slic_dir', 'hps_fieldmaps_dir', 'detector_dir'])

    def test_cmd_args_exception_input(self):
        slic = SLIC()
        self.assertRaises(Exception, lambda: slic.cmd_args())

    def test_cmd_args_exception_particle_tbl(self):
        slic = SLIC(inputs=["some/path/to/input.slcio"], outputs=["some/path/to/output.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['.hpsmc_test_cfg_invalid']
        parser.read(config_file)
        slic.config(parser)
        params = {'detector': 'some_detector'}
        slic.set_parameters(params)
        self.assertRaises(Exception, lambda: slic.cmd_args())

    def test_cmd_args(self):
        slic = SLIC(inputs=["some/path/to/input.slcio"], outputs=["some/path/to/output.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['.hpsmc_test_cfg']
        parser.read(config_file)
        slic.config(parser)
        params = {'detector': 'some_detector', 'nevents': 1}
        slic.set_parameters(params)
        self.assertEqual(slic.cmd_args(), ["-g", "some/detector/dir/some_detector/some_detector.lcdd", "-i", "some/path/to/input.slcio", "-o", "some/path/to/output.slcio", "-d1", "-r", str(1), "-P", "slicdir/share/particle.tbl"])

    def test_setup_exception_slic_dir(self):
        slic = SLIC()
        parser = configparser.ConfigParser()
        config_file = ['.hpsmc_test_cfg_invalid']
        parser.read(config_file)
        slic.config(parser)
        self.assertRaises(Exception, lambda: slic.setup())

    def test_setup(self):
        slic = SLIC(inputs=["some/path/to/input.slcio"], outputs=["some/path/to/output.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['.hpsmc_test_cfg']
        parser.read(config_file)
        slic.config(parser)
        slic.setup()
        self.assertEqual(slic.env_script, "slicdir/bin/slic-env.sh")
        os.remove(os.getcwd() + '/fieldmap')


if __name__ == '__main__':
    unittest.main()
