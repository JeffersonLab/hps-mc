import unittest
import configparser
import os

from hpsmc.tools import JobManager


class TestJobManager(unittest.TestCase):

    def test_init(self):
        job_manager = JobManager()
        self.assertEqual(job_manager.name, 'job_manager')
        self.assertEqual(job_manager.command, 'java')

    def test_init_with_input(self):
        job_manager = JobManager(steering="steering", inputs=["some/path/to/input.slcio"], outputs=["some/path/to/output.slcio"],  overlay_file="some/path/to/overlay.slcio")
        self.assertEqual(job_manager.name, 'job_manager')
        self.assertEqual(job_manager.command, 'java')
        self.assertEqual(job_manager.inputs, ["some/path/to/input.slcio"])
        self.assertEqual(job_manager.outputs, ["some/path/to/output.slcio"])
        self.assertEqual(job_manager.steering, "steering")
        self.assertEqual(job_manager.append_tok, "steering")
        self.assertEqual(job_manager.overlay_file, "some/path/to/overlay.slcio")

    def test_required_params(self):
        job_manager = JobManager()
        self.assertEqual(job_manager.required_parameters(), ['steering_files'])

    def test_required_configs(self):
        job_manager = JobManager()
        self.assertEqual(job_manager.required_config(), ['hps_java_bin_jar', 'hps_fieldmaps_dir'])

    def test_optional_params(self):
        job_manager = JobManager()
        self.assertEqual(job_manager.optional_parameters(), ['detector', 'run_number', 'defs'])

    def test_config(self):
        job_manager = JobManager()
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        job_manager.config(parser)
        self.assertEqual(job_manager.hps_fieldmaps_dir, 'some/fieldmaps/dir')
        self.assertEqual(job_manager.hps_java_bin_jar, 'some/path/to/hps-java-bin.jar')
        self.assertEqual(job_manager.conditions_url, 'http://some/path/to/conditions')

    def test_config_exception_java_jar(self):
        job_manager = JobManager()
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg_invalid']
        parser.read(config_file)
        self.assertRaises(Exception, lambda: job_manager.config(parser))

    def test_setup(self):
        job_manager = JobManager(steering="steering", inputs=["some/path/to/input.slcio"], outputs=["some/path/to/output.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        job_manager.config(parser)
        params = {'steering_files': {'steering': 'some/path/to/steering.lcsim'}}
        job_manager.set_parameters(params)
        job_manager.setup()
        self.assertEqual(job_manager.steering_file, "some/path/to/steering.lcsim")
        self.assertEqual(job_manager.hps_fieldmaps_dir, "some/fieldmaps/dir")
        os.remove(os.getcwd() + '/fieldmap')

    def test_setup_exception_steering(self):
        job_manager = JobManager(steering="invalid_steering", inputs=["some/path/to/input.slcio"], outputs=["some/path/to/output.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        job_manager.config(parser)
        params = {'steering_files': {'steering': 'some/path/to/steering.lcsim'}}
        job_manager.set_parameters(params)
        self.assertRaises(Exception, lambda: job_manager.setup())

    def test_cmd_args(self):
        job_manager = JobManager(steering="steering", inputs=["some/path/to/input.slcio"], outputs=["some/path/to/output.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        job_manager.config(parser)
        params = {'steering_files': {'steering': 'some/path/to/steering.lcsim'}, 'detector': 'some_detector', 'run_number': 1, 'event_print_interval': 10, 'nevents': 1}
        job_manager.set_parameters(params)
        job_manager.setup()
        print(job_manager.cmd_args())
        self.assertEqual(job_manager.cmd_args(), ["-Dorg.hps.conditions.url=http://some/path/to/conditions", "-jar", "some/path/to/hps-java-bin.jar", "-R", "1", "-d", "some_detector", "-D", "outputFile=some/path/to/output", "-r", "some/path/to/steering.lcsim", "-i", "some/path/to/input.slcio"])


if __name__ == '__main__':
    unittest.main()
