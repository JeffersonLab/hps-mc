import unittest
import hpsmc.func
from hpsmc.run_params import RunParameters


class TestLint(unittest.TestCase):

    def test_lint(self):
        run_params = {"target_z": 1, "num_electrons": 1}
        self.assertEqual(hpsmc.func.lint(run_params), 6.306e-14)
