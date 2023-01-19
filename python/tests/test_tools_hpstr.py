import unittest
import configparser

from hpsmc.tools import HPSTR


class TestHPSTR(unittest.TestCase):

    def test_init(self):
        hpstr = HPSTR()
        self.assertEqual(hpstr.name, 'hpstr')
        self.assertEqual(hpstr.command, 'hpstr')
        self.assertEqual(hpstr.is_data, 0)

    def test_required_params(self):
        hpstr = HPSTR()
        self.assertEqual(hpstr.required_parameters(), ['config_files'])

    def test_optional_params(self):
        hpstr = HPSTR()
        self.assertEqual(hpstr.optional_parameters(), ['year', 'is_data', 'nevents'])

    def test_required_configs(self):
        hpstr = HPSTR()
        self.assertEqual(hpstr.required_config(), ['hpstr_install_dir', 'hpstr_base'])

    def test_setup(self):
        hpstr = HPSTR(cfg='config', inputs=['some/path/to/input.root'], outputs=['some/path/to/output.root'])
        parser = configparser.ConfigParser()
        config_file = ['.hpsmc_test_cfg']
        parser.read(config_file)
        hpstr.config(parser)
        params = {'config_files': {'config': 'config.py'}}
        hpstr.set_parameters(params)

        hpstr.setup()
        self.assertEqual(hpstr.env_script, 'hpstrdir/install/bin/hpstr-env.sh')
        self.assertEqual(hpstr.cfg_path, 'hpstrdir/processors/config/config.py')
        self.assertEqual(hpstr.append_tok, 'config')

    def test_output_files_root(self):
        hpstr = HPSTR(cfg='appended_config', inputs=['some/path/to/input.root'], outputs=['some/path/to/output.root'])
        parser = configparser.ConfigParser()
        config_file = ['.hpsmc_test_cfg']
        parser.read(config_file)
        hpstr.config(parser)
        params = {'config_files': {'appended_config': 'config.py'}}
        hpstr.set_parameters(params)
        hpstr.setup()
        self.assertEqual(hpstr.output_files(), ['some/path/to/input_appended_config.root'])

    def test_output_files_slcio(self):
        hpstr = HPSTR(cfg='appended_config', inputs=['some/path/to/input.slcio'], outputs=['some/path/to/output.root'])
        parser = configparser.ConfigParser()
        config_file = ['.hpsmc_test_cfg']
        parser.read(config_file)
        hpstr.config(parser)
        params = {'config_files': {'appended_config': 'config.py'}}
        hpstr.set_parameters(params)
        hpstr.setup()
        self.assertEqual(hpstr.output_files(), ['some/path/to/input.root'])

    def test_cmd_args(self):
        hpstr = HPSTR(cfg='config', inputs=['some/path/to/input.root'], outputs=['some/path/to/output.root'])

        parser = configparser.ConfigParser()
        config_file = ['.hpsmc_test_cfg']
        parser.read(config_file)
        hpstr.config(parser)
        params = {'config_files': {'config': 'config.py'}, 'nevents': 1000, 'year': 10}
        hpstr.set_parameters(params)
        hpstr.setup()

        self.assertEqual(hpstr.cmd_args(), ["hpstrdir/processors/config/config.py", "-t", "0", "-i", "some/path/to/input.root", "-o", "some/path/to/input_config.root", "-n", "1000", "-y", "10"])


if __name__ == '__main__':
    unittest.main()
