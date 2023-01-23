import unittest
import os
import configparser

from hpsmc.component import Component


class TestComponent(unittest.TestCase):

    def test_init(self):
        component = Component('component_name')
        self.assertEqual(component.name, 'component_name')
        self.assertEqual(component.seed, 1)
        self.assertEqual(component.inputs, [])
        self.assertEqual(component.outputs, None)
        self.assertEqual(component.ignore_job_params, [])
        self.assertEqual(component.hpsmc_dir, os.getenv("HPSMC_DIR", None))

    def test_required_params(self):
        component = Component('component_name')
        self.assertEqual(component.required_parameters(), [])

    def test_optional_params(self):
        component = Component('component_name')
        self.assertEqual(component.optional_parameters(), ['nevents', 'seed'])

    def test_required_config(self):
        component = Component('component_name')
        self.assertEqual(component.required_config(), [])

    def test_input_files(self):
        component = Component('component_name', inputs=['input1', 'input2'])
        self.assertEqual(component.input_files(), ['input1', 'input2'])

    def test_inputs_to_outputs(self):
        component = Component('component_name', inputs=['input1.stdhep', 'input2.stdhep'])
        self.assertEqual(component._inputs_to_outputs(), ['input1.stdhep', 'input2.stdhep'])

    def test_inputs_to_outputs_extension(self):
        component = Component('component_name', inputs=['input1.stdhep', 'input2.stdhep'])
        component.append_tok = 'tok'
        component.output_ext = '.ext'
        self.assertEqual(component._inputs_to_outputs(), ['input1_tok.ext', 'input2_tok.ext'])

    def test_output_files(self):
        component = Component('component_name', outputs=['output1', 'output2'])
        self.assertEqual(component.output_files(), ['output1', 'output2'])

    def test_output_files_from_inputs(self):
        component = Component('component_name', inputs=['input1.stdhep', 'input2.stdhep'])
        component.append_tok = 'tok'
        component.output_ext = '.ext'
        self.assertEqual(component.output_files(), ['input1_tok.ext', 'input2_tok.ext'])

    def test_set_parameters(self):
        component = Component('component_name')
        component.set_parameters({'nevents': 10, 'seed': 2})
        self.assertEqual(component.nevents, 10)
        self.assertEqual(component.seed, 2)

    def test_set_parameters_ignore(self):
        component = Component('component_name', ignore_job_params=['nevents'])
        component.set_parameters({'nevents': 10, 'seed': 2})
        self.assertEqual(component.nevents, None)
        self.assertEqual(component.seed, 2)

    def test_config(self):
        component = Component('Component')
        parser = configparser.ConfigParser()
        config_file = ['.hpsmc_test_cfg']
        parser.read(config_file)
        component.config(parser)
        self.assertEqual(component.hps_java_bin_jar, 'some/path/to/hps-java-bin.jar')
        self.assertEqual(component.lcio_bin_jar, 'some/path/to/lcio-bin.jar')

    def test_cmd_args(self):
        component = Component('component_name')
        self.assertEqual(component.cmd_args(), [])

    def test_cmd_line_str(self):
        component = Component('component_name', command='some_command')
        self.assertEqual(component.cmd_line_str(), 'some_command')


if __name__ == '__main__':
    unittest.main()
