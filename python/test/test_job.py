import unittest
import os
import sys
import logging
import configparser

from os.path import expanduser
from hpsmc.job import JobConfig, Job
from hpsmc.tools import SLIC, HPSTR


class TestJobConfig(unittest.TestCase):

    def test_init(self):
        job_config = JobConfig()
        self.assertEqual(job_config.config_files, [os.path.join(expanduser("~"), ".hpsmc"), os.path.abspath(".hpsmc")])

    def test_init_config(self):
        job_config = JobConfig(config_files=['test_helpers/.hpsmc_test_cfg'], include_default_locations=False)
        self.assertEqual(job_config.config_files, ['test_helpers/.hpsmc_test_cfg'])
        # JobConfig uses configparser to load config. If the default config files don't exist, configparser will not crash but ignore those files instead.

    def test_config(self):
        job_config = JobConfig(config_files=['test_helpers/.hpsmc_test_cfg'], include_default_locations=False)
        component = SLIC()
        job_config.config(component, required_names=['detector_dir'])
        self.assertEqual(component.detector_dir, 'some/detector/dir')

    def test_config_missing_section(self):
        job_config = JobConfig(config_files=['test_helpers/.hpsmc_test_cfg_invalid'], include_default_locations=False)
        component = HPSTR()
        self.assertRaises(Exception, lambda: job_config.config(component, required_names=['hpstr_install_dir']), "Missing required config section HPSTR")
        job_config.config(component, required_names=['hpstr_install_dir'], require_section=False)
        self.assertEqual(component.hpstr_install_dir, None)

    def test_config_missing_config(self):
        job_config = JobConfig(config_files=['test_helpers/.hpsmc_test_cfg'], include_default_locations=False)
        component = SLIC()
        self.assertRaises(Exception, lambda: job_config.config(component, required_names=['not_in_config']), "Missing required config not_in_config")

    def test_config_not_allowed_config(self):
        job_config = JobConfig(config_files=['test_helpers/.hpsmc_test_cfg'], include_default_locations=False)
        component = SLIC()
        self.assertRaises(Exception, lambda: job_config.config(component, allowed_names=['not_in_config']), "Config name detector_dir is not allowed for SLIC")


class TestJob(unittest.TestCase):

    def test_init(self):
        job = Job(args=['-d', 'some/run/dir', '-o', 'output/dir', '-i', '1'])
        self.assertEqual(job.args, ['-d', 'some/run/dir', '-o', 'output/dir', '-i', '1'])
        self.assertEqual(job.description, 'HPS MC Job')
        self.assertEqual(job.job_id, None)
        self.assertEqual(job.param_file, None)
        self.assertEqual(job.components, [])
        self.assertEqual(job.rundir, os.getcwd())
        self.assertEqual(job.params, {})
        self.assertEqual(job.output_dir, os.getcwd())
        self.assertEqual(job.input_files, {})
        self.assertEqual(job.output_files, {})
        self.assertEqual(job.ptags, {})
        self.assertEqual(job.log, sys.stdout)
        self.assertEqual(job.out, sys.stdout)
        self.assertEqual(job.err, sys.stderr)
        self.assertEqual(job.enable_copy_output_files, True)
        self.assertEqual(job.enable_copy_input_files, True)
        self.assertEqual(job.delete_existing, False)
        self.assertEqual(job.delete_rundir, False)
        self.assertEqual(job.dry_run, False)
        self.assertEqual(job.ignore_return_codes, True)
        self.assertEqual(job.check_output_files, True)
        self.assertEqual(job.check_commands, False)
        self.assertEqual(job.enable_file_chaining, True)
        self.assertEqual(job.enable_env_config, False)
        self.assertEqual(job.log_level, logging.INFO)

    def test_add(self):
        job = Job()
        job.add('test1')
        self.assertEqual(job.components, ['test1'])
        job.add(['test2', 'test3'])
        self.assertEqual(job.components, ['test1', 'test2', 'test3'])

    def test_set_params(self):
        job = Job()
        job.set_parameters({'test1': 1, 'test2': 2})
        self.assertEqual(job.params, {'test1': 1, 'test2': 2})
        job.set_parameters({'test3': 3})
        self.assertEqual(job.params, {'test1': 1, 'test2': 2, 'test3': 3})

    def test_parse_args(self):
        job = Job(args=['-c', 'test_helpers/.hpsmc_test_cfg', '-d', 'some/run/dir', '-o', 'test_helpers/job_files/out_file', '-e', 'test_helpers/job_files/err_file', '-l', 'test_helpers/job_files/log_file', '-s', '1', '-i', '1', 'test_helpers/job_files/some_job_script.py', 'test_helpers/job_files/job_params.json'])
        self.assertEqual(job.args, ['-c', 'test_helpers/.hpsmc_test_cfg', '-d', 'some/run/dir', '-o', 'test_helpers/job_files/out_file', '-e', 'test_helpers/job_files/err_file', '-l', 'test_helpers/job_files/log_file', '-s', '1', '-i', '1', 'test_helpers/job_files/some_job_script.py', 'test_helpers/job_files/job_params.json'])
        job.parse_args()
        self.assertEqual(job.log_level, 10)
        self.assertEqual(job.rundir, 'some/run/dir')
        self.assertEqual(job.job_steps, 1)
        self.assertEqual(job.script, 'test_helpers/job_files/some_job_script.py')
        self.assertEqual(job.param_file, os.path.abspath('test_helpers/job_files/job_params.json'))
        self.assertEqual(job.job_id, 1)
        self.assertEqual(job.params, {"input_files": {"input1.stdhep": "path/to/input1.stdhep", "input2.stdhep": "path/to/input2.stdhep"}, "output_files": {"output.slcio": "output_file.slcio"}, "output_dir": "some/output_dir", "job_id": 1})
        self.assertEqual(job.output_dir, os.path.abspath('some/output_dir'))
        self.assertEqual(job.input_files, {"input1.stdhep": "path/to/input1.stdhep", "input2.stdhep": "path/to/input2.stdhep"})
        self.assertEqual(job.output_files, {"output.slcio": "output_file.slcio"})

    def test_parse_args_no_script(self):
        job = Job(args=[])
        self.assertRaises(Exception, lambda: job.parse_args(), 'Missing required script name or location.')
