import unittest
import configparser
import os

from hpsmc.tools import JavaTool, EvioToLcio, FilterBunches, ExtractEventsWithHitAtHodoEcal


class TestJavaTool(unittest.TestCase):

    def test_init(self):
        java_tool = JavaTool(name="java_tool", java_class="java_class")
        self.assertEqual(java_tool.name, "java_tool")
        self.assertEqual(java_tool.java_class, "java_class")
        self.assertEqual(java_tool.command, "java")

    def test_required_config(self):
        java_tool = JavaTool(name="java_tool", java_class="java_class")
        self.assertEqual(java_tool.required_config(), ["hps_java_bin_jar", "hps_fieldmaps_dir"])

    def test_config(self):
        java_tool = JavaTool(name="java_tool", java_class="java_class")
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        java_tool.config(parser)
        self.assertEqual(java_tool.hps_fieldmaps_dir, "some/fieldmaps/dir")
        self.assertEqual(java_tool.hps_java_bin_jar, "some/path/to/hps-java-bin.jar")
        self.assertEqual(java_tool.conditions_url, "http://some/path/to/conditions")

    def test_setup(self):
        java_tool = JavaTool(name="java_tool", java_class="java_class")
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        java_tool.config(parser)
        java_tool.setup()
        self.assertTrue(os.path.islink(os.getcwd() + "/fieldmap"))
        os.remove(os.getcwd() + "/fieldmap")

    def test_cmd_args(self):
        java_tool = JavaTool(name="java_tool", java_class="java_class")
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        java_tool.config(parser)
        self.assertEqual(java_tool.cmd_args(), ["args", "-Dorg.hps.conditions.url=http://some/path/to/conditions", "-cp", "some/path/to/hps-java-bin.jar", "java_class"])


class TestEvioToLcio(unittest.TestCase):

    def test_init(self):
        evio_to_lcio = EvioToLcio(steering="steering_file")
        self.assertEqual(evio_to_lcio.name, "evio_to_lcio")
        self.assertEqual(evio_to_lcio.java_class, "org.hps.evio.EvioToLcio")
        self.assertEqual(evio_to_lcio.steering, "steering_file")
        self.assertEqual(evio_to_lcio.output_ext, ".slcio")

    def test_required_params(self):
        evio_to_lcio = EvioToLcio()
        self.assertEqual(evio_to_lcio.required_parameters(), ["detector", "steering_files"])

    def test_optional_params(self):
        evio_to_lcio = EvioToLcio()
        self.assertEqual(evio_to_lcio.optional_parameters(), ["run_number", "skip_events", "nevents", "event_print_interval"])

    def test_setup(self):
        evio_to_lcio = EvioToLcio(steering="steering")
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        evio_to_lcio.config(parser)
        evio_to_lcio.set_parameters({"steering_files": {"steering": "steering_file"}, "detector": "detector"})
        evio_to_lcio.setup()
        self.assertEqual(evio_to_lcio.steering_file, "steering_file")

    def test_cmd_args(self):
        evio_to_lcio = EvioToLcio(steering="steering", inputs=["input1.evio", "input2.evio"], outputs=["output.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        evio_to_lcio.config(parser)
        evio_to_lcio.set_parameters({"steering_files": {"steering": "steering_file"}, "detector": "detector", "skip_events": 10, "nevents": 100, "event_print_interval": 10, "run_number": 1234})
        evio_to_lcio.setup()
        self.assertEqual(evio_to_lcio.cmd_args(), ["args", "-Dorg.hps.conditions.url=http://some/path/to/conditions", "-cp", "some/path/to/hps-java-bin.jar", "org.hps.evio.EvioToLcio", "-DoutputFile=output", "-d", "detector", "-R", "1234", "-s", "10", "-r", "-x", "steering_file", "-n", "100", "-b", "input1.evio", "input2.evio", "-e", "10"])


class TestFilterBunches(unittest.TestCase):

    def test_init(self):
        filter_bunches = FilterBunches(filter_no_cuts=True, filter_ecal_pairs=True, filter_ecal_hit_ecut=1.0, filter_event_interval=10, filter_nevents_read=1, filter_nevents_write=10)
        self.assertEqual(filter_bunches.name, "filter_bunches")
        self.assertEqual(filter_bunches.java_class, "org.hps.util.FilterMCBunches")
        self.assertEqual(filter_bunches.append_tok, 'filt')
        self.assertEqual(filter_bunches.filter_no_cuts, True)
        self.assertEqual(filter_bunches.filter_ecal_pairs, True)
        self.assertEqual(filter_bunches.filter_ecal_hit_ecut, 1.0)
        self.assertEqual(filter_bunches.filter_event_interval, 10)
        self.assertEqual(filter_bunches.filter_nevents_read, 1)
        self.assertEqual(filter_bunches.filter_nevents_write, 10)

    def test_optional_params(self):
        filter_bunches = FilterBunches()
        self.assertEqual(filter_bunches.optional_parameters(), ["filter_ecal_hit_ecut", "filter_event_interval", "filter_nevents_read", "filter_nevents_write", "filter_no_cuts"])

    def test_required_config(self):
        filter_bunches = FilterBunches()
        self.assertEqual(filter_bunches.required_config(), ["hps_java_bin_jar"])

    def test_config(self):
        filter_bunches = FilterBunches()
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        filter_bunches.config(parser)
        self.assertEqual(filter_bunches.hps_java_bin_jar, "some/path/to/hps-java-bin.jar")

    def test_cmd_args(self):
        filter_bunches = FilterBunches(inputs=["input1.slcio", "input2.slcio"], outputs=["output.slcio"], filter_no_cuts=True, filter_ecal_pairs=True, filter_ecal_hit_ecut=1.0, filter_event_interval=10, filter_nevents_read=100, filter_nevents_write=100)
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        filter_bunches.config(parser)
        self.assertEqual(filter_bunches.cmd_args(), ["args", "-Dorg.hps.conditions.url=http://some/path/to/conditions", "-cp", "some/path/to/hps-java-bin.jar", "org.hps.util.FilterMCBunches", "-e", "10", "input1.slcio", "input2.slcio", "output.slcio", "-d", "-E", "1.0", "-n", "100", "-w", "100", "-a"])


class TestExtractEventsWithHitAtHodoEcal(unittest.TestCase):

    def test_init(self):
        extract_events = ExtractEventsWithHitAtHodoEcal(num_hodo_hits=1, event_interval=10)
        self.assertEqual(extract_events.name, "filter_events")
        self.assertEqual(extract_events.java_class, "org.hps.util.ExtractEventsWithHitAtHodoEcal")
        self.assertEqual(extract_events.append_tok, 'filt')
        self.assertEqual(extract_events.num_hodo_hits, 1)
        self.assertEqual(extract_events.event_interval, 10)

    def test_optional_params(self):
        extract_events = ExtractEventsWithHitAtHodoEcal()
        self.assertEqual(extract_events.optional_parameters(), ["num_hodo_hits", "event_interval"])

    def test_cmd_args(self):
        extract_events = ExtractEventsWithHitAtHodoEcal(inputs=["input1.slcio", "input2.slcio"], outputs=["output.slcio"], num_hodo_hits=1, event_interval=10, nevents=100)
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        extract_events.config(parser)
        self.assertEqual(extract_events.cmd_args(), ["args", "-Dorg.hps.conditions.url=http://some/path/to/conditions", "-cp", "some/path/to/hps-java-bin.jar", "org.hps.util.ExtractEventsWithHitAtHodoEcal", "-e", "10", "input1.slcio", "input2.slcio", "output.slcio", "-M", "1", "-w", "100"])


if __name__ == '__main__':
    unittest.main()
