import unittest
from hpsmc.run_params import RunParameters


class TestRunParams(unittest.TestCase):

    def test_run_params1pt1(self):
        rp = RunParameters("1pt1")
        self.assertEqual(rp.get("aprime_mass")[0], 15)
        self.assertEqual(rp.get("target_z"), 0.0004062)
        self.assertEqual(rp.get("beam_energy"), 1100.00)
        self.assertEqual(rp.get("num_electrons"), 625)

    def test_run_params1pt92(self):
        rp = RunParameters("1pt92")
        self.assertEqual(rp.get("aprime_mass")[0], 50)
        self.assertEqual(rp.get("target_z"), 0.0008)
        self.assertEqual(rp.get("beam_energy"), 1920.00)
        self.assertEqual(rp.get("num_electrons"), 875)

    def test_run_params1pt05(self):
        rp = RunParameters("1pt05")
        self.assertEqual(rp.get("aprime_mass")[0], 15)
        self.assertEqual(rp.get("target_z"), 0.0004062)
        self.assertEqual(rp.get("beam_energy"), 1056.00)
        self.assertEqual(rp.get("num_electrons"), 625)

    def test_run_params2pt2(self):
        rp = RunParameters("2pt2")
        self.assertEqual(rp.get("aprime_mass")[0], 15)
        self.assertEqual(rp.get("target_z"), 0.0004062)
        self.assertEqual(rp.get("beam_energy"), 2200.00)
        self.assertEqual(rp.get("num_electrons"), 2500)

    def test_run_params2pt3(self):
        rp = RunParameters("2pt3")
        self.assertEqual(rp.get("aprime_mass")[0], 15)
        self.assertEqual(rp.get("target_z"), 0.0004062)
        self.assertEqual(rp.get("beam_energy"), 2300.00)
        self.assertEqual(rp.get("num_electrons"), 2500)

    def test_run_params3pt7(self):
        rp = RunParameters("3pt7")
        self.assertEqual(rp.get("aprime_mass")[0], 15)
        self.assertEqual(rp.get("target_z"), 0.002)
        self.assertEqual(rp.get("beam_energy"), 3700.00)
        self.assertEqual(rp.get("num_electrons"), 1500)

    def test_run_params3pt74(self):
        rp = RunParameters("3pt74")
        self.assertEqual(rp.get("target_z"), 0.000875)
        self.assertEqual(rp.get("beam_energy"), 3740.00)
        self.assertEqual(rp.get("num_electrons"), 625)

    def test_run_params3pt742(self):
        rp = RunParameters("3pt742")
        self.assertEqual(rp.get("aprime_mass")[0], 50)
        self.assertEqual(rp.get("target_z"), 0.002)
        self.assertEqual(rp.get("beam_energy"), 3742.00)
        self.assertEqual(rp.get("num_electrons"), 1500)

    def test_run_params4pt4(self):
        rp = RunParameters("4pt4")
        self.assertEqual(rp.get("aprime_mass")[0], 15)
        self.assertEqual(rp.get("target_z"), 0.0004062)
        self.assertEqual(rp.get("beam_energy"), 4400.00)
        self.assertEqual(rp.get("num_electrons"), 5000)

    def test_run_params4pt55(self):
        rp = RunParameters("4pt55")
        self.assertEqual(rp.get("aprime_mass")[0], 75)
        self.assertEqual(rp.get("target_z"), 0.002)
        self.assertEqual(rp.get("beam_energy"), 4550.00)
        self.assertEqual(rp.get("num_electrons"), 1500)

    def test_run_params6pt6(self):
        rp = RunParameters("6pt6")
        self.assertEqual(rp.get("aprime_mass")[0], 50)
        self.assertEqual(rp.get("target_z"), 0.000875)
        self.assertEqual(rp.get("beam_energy"), 6600.00)
        self.assertEqual(rp.get("num_electrons"), 5625)


if __name__ == '__main__':
    unittest.main()
