import unittest
import os

from hpsmc.alignment._parameter import Parameter
from hpsmc.alignment._pattern import Pattern

class TestParameterPatterns(unittest.TestCase):

    def test_match_id_number(self):
        param = Parameter.from_idn('11106')
        self.assertEqual(Pattern(11106), param)
        self.assertEqual(Pattern('11106'), param)
        self.assertEqual(Pattern('id=11106'), param)

    def test_bad_patterns(self) :
        # ID number not matching pattern
        with self.assertRaises(ValueError) :
            Pattern(52420)
        with self.assertRaises(ValueError) :
            Pattern(1)
        with self.assertRaises(ValueError) :
            Pattern('100123')

        # unary operation not listed
        with self.assertRaises(ValueError) :
            Pattern('dne')
        with self.assertRaises(ValueError) :
            Pattern('!dne')
        with self.assertRaises(ValueError) :
            Pattern('top&dne')

        # binary operation bad format
        with self.assertRaises(ValueError) :
            Pattern('id==11106')
        with self.assertRaises(ValueError) :
            Pattern('dne=123')
        with self.assertRaises(ValueError) :
            Pattern('half=dne')

    def test_match_unary(self) :
        individual = Pattern('individual')
        self.assertEqual(individual, Parameter.from_idn(11106))
        self.assertNotEqual(individual, Parameter.from_idn(12382))

    def test_match_direction(self) :
        direction_u = Pattern('direction=u')
        self.assertEqual(direction_u, Parameter.from_idn(11106))
        self.assertNotEqual(direction_u, Parameter.from_idn(11206))

    def test_match_operation(self) :
        operation_t = Pattern('translation')
        self.assertEqual(operation_t, Parameter.from_idn(11106))
        self.assertNotEqual(operation_t, Parameter.from_idn(12106))

    def test_combine_binary(self) :
        tu = Pattern('direction=u & translation')
        self.assertEqual(tu, Parameter.from_idn(11106))
        self.assertNotEqual(tu, Parameter.from_idn(12106))
        self.assertNotEqual(tu, Parameter.from_idn(11206))

    def test_combine_binary_and_unary(self) :
        stereo_tu = Pattern('stereo & direction=u & translation')
        self.assertNotEqual(stereo_tu, Parameter.from_idn(11106))
        self.assertNotEqual(stereo_tu, Parameter.from_idn(12106))
        self.assertNotEqual(stereo_tu, Parameter.from_idn(11206))
        self.assertEqual(stereo_tu, Parameter.from_idn(11107))

    def test_alias(self) :
        tu = Pattern('tu')
        self.assertEqual(tu, Parameter.from_idn(11106))
        self.assertNotEqual(tu, Parameter.from_idn(12106))
        self.assertNotEqual(tu, Parameter.from_idn(11206))

    def test_combine_alias(self) :
        stereo_tu = Pattern('stereo & tu')
        self.assertNotEqual(stereo_tu, Parameter.from_idn(11106))
        self.assertNotEqual(stereo_tu, Parameter.from_idn(12106))
        self.assertNotEqual(stereo_tu, Parameter.from_idn(11206))
        self.assertEqual(stereo_tu, Parameter.from_idn(11107))


if __name__ == '__main__':
    unittest.main()
