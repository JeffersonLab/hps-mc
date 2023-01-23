import unittest

from hpsmc.tools import StdHepCount


class TestStdHepCount(unittest.TestCase):

    def test_init(self):
        stdhep_count = StdHepCount()
        self.assertEqual(stdhep_count.name, "stdhep_count")
        self.assertEqual(stdhep_count.command, "stdhep_count.sh")

    def test_cmd_args(self):
        stdhep_count = StdHepCount(inputs=["input1.stdhep", "input2.stdhep"], outputs=["some/path/to/output.txt"])
        self.assertEqual(stdhep_count.cmd_args(), ["input1.stdhep"])


if __name__ == '__main__':
    unittest.main()
