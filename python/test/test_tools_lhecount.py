import unittest

from hpsmc.tools import LHECount


class TestLHECount(unittest.TestCase):

    def test_init(self):
        lhe_count = LHECount(minevents=10)
        self.assertEqual(lhe_count.name, "lhe_count")
        self.assertEqual(lhe_count.minevents, 10)

    def test_setup_no_inputs(self):
        lhe_count = LHECount()
        self.assertRaises(Exception, lambda: lhe_count.setup(), "Missing at least one input file.")


if __name__ == '__main__':
    unittest.main()
