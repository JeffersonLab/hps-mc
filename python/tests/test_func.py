import unittest
import hpsmc.func
from hpsmc.run_params import RunParameters


class TestLint(unittest.TestCase):

    def test_lint_1pt1(self):
        run_params = RunParameters("1pt1")
        res = 6.306e-14 * 0.0004062 * 625
        self.assertEqual(hpsmc.func.lint(run_params), res)

    def test_lint_1pt92(self):
        run_params = RunParameters("1pt92")
        res = 6.306e-14 * 0.0008 * 875
        self.assertEqual(hpsmc.func.lint(run_params), res)

    def test_lint_1pt05(self):
        run_params = RunParameters("1pt05")
        res = 6.306e-14 * 0.0004062 * 625
        self.assertEqual(hpsmc.func.lint(run_params), res)

    def test_lint_2pt2(self):
        run_params = RunParameters("2pt2")
        res = 6.306e-14 * 0.0004062 * 2500
        self.assertEqual(hpsmc.func.lint(run_params), res)

    def test_lint_2pt3(self):
        run_params = RunParameters("2pt3")
        res = 6.306e-14 * 0.0004062 * 2500
        self.assertEqual(hpsmc.func.lint(run_params), res)

    def test_lint_3pt7(self):
        run_params = RunParameters("3pt7")
        res = 6.306e-14 * 0.002 * 1500
        self.assertEqual(hpsmc.func.lint(run_params), res)

    def test_lint_3pt74(self):
        run_params = RunParameters("3pt74")
        res = 6.306e-14 * 0.000875 * 625
        self.assertEqual(hpsmc.func.lint(run_params), res)

    def test_lint_3pt742(self):
        run_params = RunParameters("3pt742")
        res = 6.306e-14 * 0.002 * 1500
        self.assertEqual(hpsmc.func.lint(run_params), res)

    def test_lint_4pt4(self):
        run_params = RunParameters("4pt4")
        res = 6.306e-14 * 0.0004062 * 5000
        self.assertEqual(hpsmc.func.lint(run_params), res)

    def test_lint_4pt55(self):
        run_params = RunParameters("4pt55")
        res = 6.306e-14 * 0.002 * 1500
        self.assertEqual(hpsmc.func.lint(run_params), res)

    def test_lint_6pt6(self):
        run_params = RunParameters("6pt6")
        res = 6.306e-14 * 0.000875 * 5625
        self.assertEqual(hpsmc.func.lint(run_params), res)
