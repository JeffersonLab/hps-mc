import unittest
import configparser

from hpsmc.tools import LCIOTool, LCIOConcat, LCIOCount, LCIOMerge


class TestLCIOTool(unittest.TestCase):

    def test_init(self):
        lcio_tool = LCIOTool()
        self.assertEqual(lcio_tool.command, "java")

    def test_required_params(self):
        lcio_tool = LCIOTool()
        self.assertEqual(lcio_tool.required_parameters(), [])

    def test_required_config(self):
        lcio_tool = LCIOTool()
        self.assertEqual(lcio_tool.required_config(), ["lcio_bin_jar"])

    def test_config(self):
        lcio_tool = LCIOTool()
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_tool.config(parser)
        self.assertEqual(lcio_tool.lcio_bin_jar, "some/path/to/lcio-bin.jar")

    def test_cmd_args(self):
        lcio_tool = LCIOTool(name="lcio_tool")
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_tool.config(parser)
        self.assertEqual(lcio_tool.cmd_args(), ["-jar", "some/path/to/lcio-bin.jar", "lcio_tool"])


class TestLCIOConcat(unittest.TestCase):

    def test_init(self):
        lcio_concat = LCIOConcat()
        self.assertEqual(lcio_concat.name, "concat")
        self.assertEqual(lcio_concat.command, "java")

    def test_cmd_args(self):
        lcio_concat = LCIOConcat(inputs=["some/path/to/input1.slcio", "some/path/to/input2.slcio"], outputs=["some/path/to/output.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_concat.config(parser)
        self.assertEqual(lcio_concat.cmd_args(), ["-jar", "some/path/to/lcio-bin.jar", "concat", "-f", "some/path/to/input1.slcio", "-f", "some/path/to/input2.slcio", "-o", "some/path/to/output.slcio"])

    def test_cmd_args_no_input(self):
        lcio_concat = LCIOConcat(outputs=["some/path/to/output.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_concat.config(parser)
        self.assertRaises(Exception, lambda: lcio_concat.cmd_args(), "Missing at least one input file.")

    def test_cmd_args_no_output(self):
        lcio_concat = LCIOConcat(inputs=["some/path/to/input1.slcio", "some/path/to/input2.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_concat.config(parser)
        self.assertRaises(Exception, lambda: lcio_concat.cmd_args(), "Missing an output file.")


class TestLCIOCount(unittest.TestCase):

    def test_init(self):
        lcio_count = LCIOCount()
        self.assertEqual(lcio_count.name, "count")
        self.assertEqual(lcio_count.command, "java")

    def test_required_params(self):
        lcio_count = LCIOCount()
        self.assertEqual(lcio_count.required_parameters(), [])

    def test_optional_params(self):
        lcio_count = LCIOCount()
        self.assertEqual(lcio_count.optional_parameters(), [])

    def test_cmd_args(self):
        lcio_count = LCIOCount(inputs=["some/path/to/input.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_count.config(parser)
        self.assertEqual(lcio_count.cmd_args(), ["-jar", "some/path/to/lcio-bin.jar", "count", "-f", "some/path/to/input.slcio"])

    def test_cmd_args_no_input(self):
        lcio_count = LCIOCount()
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_count.config(parser)
        self.assertRaises(Exception, lambda: lcio_count.cmd_args(), "Missing an input file.")


class TestLCIOMerge(unittest.TestCase):

    def test_init(self):
        lcio_merge = LCIOMerge()
        self.assertEqual(lcio_merge.name, "merge")
        self.assertEqual(lcio_merge.command, "java")

    def test_cmd_args(self):
        lcio_merge = LCIOMerge(inputs=["some/path/to/input1.slcio", "some/path/to/input2.slcio"], outputs=["some/path/to/output.slcio"], nevents=100)
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_merge.config(parser)
        self.assertEqual(lcio_merge.cmd_args(), ["-jar", "some/path/to/lcio-bin.jar", "merge", "-f", "some/path/to/input1.slcio", "-f", "some/path/to/input2.slcio", "-o", "some/path/to/output.slcio", "-n", "100"])

    def test_cmd_args_no_input(self):
        lcio_merge = LCIOMerge(outputs=["some/path/to/output.slcio"], nevents=100)
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_merge.config(parser)
        self.assertRaises(Exception, lambda: lcio_merge.cmd_args(), "Missing at least one input file.")

    def test_cmd_args_no_output(self):
        lcio_merge = LCIOMerge(inputs=["some/path/to/input1.slcio", "some/path/to/input2.slcio"], nevents=100)
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        lcio_merge.config(parser)
        self.assertRaises(Exception, lambda: lcio_merge.cmd_args(), "Missing an output file.")


if __name__ == '__main__':
    unittest.main()
