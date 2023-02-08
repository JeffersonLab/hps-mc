import unittest
import hpsmc.func


class TestLint(unittest.TestCase):

    def test_lint_1pt1(self):
        res = 6.306e-14 * 0.0004062 * 625
        self.assertEqual(hpsmc.func.lint(0.0004062, 625), res)

    def test_lint_1pt92(self):
        res = 6.306e-14 * 0.0008 * 875
        self.assertEqual(hpsmc.func.lint(0.0008, 875), res)

    def test_lint_1pt05(self):
        res = 6.306e-14 * 0.0004062 * 625
        self.assertEqual(hpsmc.func.lint(0.0004062, 625), res)

    def test_lint_2pt2(self):
        res = 6.306e-14 * 0.0004062 * 2500
        self.assertEqual(hpsmc.func.lint(0.0004062, 2500), res)

    def test_lint_2pt3(self):
        res = 6.306e-14 * 0.0004062 * 2500
        self.assertEqual(hpsmc.func.lint(0.0004062, 2500), res)

    def test_lint_3pt7(self):
        res = 6.306e-14 * 0.002 * 1500
        self.assertEqual(hpsmc.func.lint(0.002, 1500), res)

    def test_lint_3pt74(self):
        res = 6.306e-14 * 0.000875 * 625
        self.assertEqual(hpsmc.func.lint(0.000875, 625), res)

    def test_lint_3pt742(self):
        res = 6.306e-14 * 0.002 * 1500
        self.assertEqual(hpsmc.func.lint(0.002, 1500), res)

    def test_lint_4pt4(self):
        res = 6.306e-14 * 0.0004062 * 5000
        self.assertEqual(hpsmc.func.lint(0.0004062, 5000), res)

    def test_lint_4pt55(self):
        res = 6.306e-14 * 0.002 * 1500
        self.assertEqual(hpsmc.func.lint(0.002, 1500), res)

    def test_lint_6pt6(self):
        res = 6.306e-14 * 0.000875 * 5625
        self.assertEqual(hpsmc.func.lint(0.000875, 5625), res)


if __name__ == '__main__':
    unittest.main()
