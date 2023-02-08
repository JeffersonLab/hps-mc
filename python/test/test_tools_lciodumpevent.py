import unittest
import configparser

from hpsmc.tools import LCIODumpEvent


class TestLCIODumpEvent(unittest.TestCase):

    def test_init(self):
        lcio_dump_event = LCIODumpEvent(event_num=10)
        self.assertEqual(lcio_dump_event.name, "lcio_dump_event")
        self.assertEqual(lcio_dump_event.command, "dumpevent")
        self.assertEqual(lcio_dump_event.event_num, 10)

    def test_required_params(self):
        lcio_dump_event = LCIODumpEvent()
        self.assertEqual(lcio_dump_event.required_parameters(), [])

    def test_required_config(self):
        lcio_dump_event = LCIODumpEvent()
        self.assertEqual(lcio_dump_event.required_config(), ["lcio_dir"])

    def test_config(self):
        lcio_dump_event = LCIODumpEvent()
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_dump_event.config(parser)
        self.assertEqual(lcio_dump_event.lcio_dir, "test_helpers/lciodir")

    def test_setup(self):
        lcio_dump_event = LCIODumpEvent()
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_dump_event.config(parser)
        lcio_dump_event.setup()
        self.assertEqual(lcio_dump_event.command, "test_helpers/lciodir/bin/dumpevent")

    def test_cmd_args(self):
        lcio_dump_event = LCIODumpEvent(event_num=10, inputs=["some/path/to/input.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_dump_event.config(parser)
        self.assertEqual(lcio_dump_event.cmd_args(), ["some/path/to/input.slcio", "10"])


if __name__ == '__main__':
    unittest.main()
