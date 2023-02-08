import unittest
import configparser
import os

from hpsmc.tools import SimBase, Sim


class TestSimBase(unittest.TestCase):

    def test_required_params(self):
        sim_base = SimBase(name="sim_base")
        self.assertEqual(sim_base.required_parameters(), ["detector"])

    def test_optional_params(self):
        sim_base = SimBase(name="sim_base")
        self.assertEqual(sim_base.optional_parameters(), ["nevents", "macros", "run_number"])

    def test_detector_file(self):
        sim_base = SimBase(name="sim_base")
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        sim_base.config(parser)
        params = {"detector": "some_detector"}
        sim_base.set_parameters(params)
        self.assertEqual(sim_base.detector_file(), "some/detector/dir/some_detector/some_detector.lcdd")


class TestSim(unittest.TestCase):

    def test_init(self):
        sim = Sim()
        self.assertEqual(sim.name, "hps-sim")
        self.assertEqual(sim.command, "hps-sim")
        self.assertEqual(sim.output_ext, ".slcio")

    def test_required_configs(self):
        sim = Sim()
        self.assertEqual(sim.required_config(), ["hps_sim_dir", "hps_fieldmaps_dir", "detector_dir"])

    def test_config(self):
        sim = Sim()
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        sim.config(parser)
        self.assertEqual(sim.detector_dir, "some/detector/dir")
        self.assertEqual(sim.hps_fieldmaps_dir, "some/fieldmaps/dir")
        self.assertEqual(sim.hps_sim_dir, "test_helpers/simdir")

    def test_setup(self):
        sim = Sim(inputs=["some/path/to/input.stdhep"], outputs=["some/path/to/output.slcio"])
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        sim.config(parser)
        params = {"detector": "some_detector"}
        sim.set_parameters(params)
        sim.setup()
        self.assertEqual(sim.env_script, "test_helpers/simdir/bin/hps-sim-env.sh")
        self.assertTrue(os.path.islink(os.getcwd() + os.path.sep + "fieldmap"))
        os.unlink(os.getcwd() + "/fieldmap")

    def test_write_run_macro(self):
        sim = Sim(inputs=["some/path/to/input.stdhep"], outputs=["some/path/to/output.slcio"], seed=2, nevents=10)
        parser = configparser.ConfigParser()
        config_file = ['test_helpers/.hpsmc_test_cfg']
        parser.read(config_file)
        sim.config(parser)
        params = {"detector": "some_detector", "macros": ["macro1.mac", "macro2.mac"]}
        sim.set_parameters(params)
        sim.setup()
        sim.write_run_macro()
        self.assertTrue(os.path.isfile("run.macro"))
        with open("run.macro", "r") as f:
            lines = f.readlines()
            self.assertEqual(lines[0], "/lcdd/url some/detector/dir/some_detector/some_detector.lcdd\n")
            self.assertEqual(lines[1], "/run/initialize\n")
            self.assertEqual(lines[2], "/random/seed 2\n")
            self.assertEqual(lines[3], "/hps/generators/create StdHepGen STDHEP\n")
            self.assertEqual(lines[4], "/hps/generators/StdHepGen/file some/path/to/input.stdhep\n")
            self.assertEqual(lines[5], "/control/execute macro1.mac\n")
            self.assertEqual(lines[6], "/control/execute macro2.mac\n")
            self.assertEqual(lines[7], "/hps/lcio/recreate\n")
            self.assertEqual(lines[8], "/hps/lcio/file some/path/to/output.slcio\n")
            self.assertEqual(lines[9], "/run/beamOn 10")

        os.remove(os.getcwd() + "/run.macro")
        os.unlink(os.getcwd() + "/fieldmap")


if __name__ == "__main__":
    unittest.main()
