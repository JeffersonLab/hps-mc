import unittest

from hpsmc.tools import StdHepTool, BeamCoords, RandomSample, DisplaceTime, DisplaceUni, AddMother, AddMotherFullTruth, MergePoisson, MergeFiles


class TestStdHepTools(unittest.TestCase):

    def test_init(self):
        stdheptool = StdHepTool(name='tool')
        self.assertEqual(stdheptool.name, 'tool')
        self.assertEqual(stdheptool.command, 'stdhep_tool')

    def test_cmd_args(self):
        stdheptool = StdHepTool(name='beam_coords', seed=1, inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep'])
        self.assertEqual(stdheptool.cmd_args(), ['input1.stdhep', 'input2.stdhep', 'output.stdhep', '-s', '1'])

    def test_cmd_args_no_seed(self):
        stdheptool = StdHepTool(name='tool', inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep'])
        self.assertEqual(stdheptool.cmd_args(), ['input1.stdhep', 'input2.stdhep', 'output.stdhep'])

    def test_cmd_args_no_inputs(self):
        stdheptool = StdHepTool(name='tool', outputs=['output.stdhep'])
        self.assertRaises(Exception, lambda: stdheptool.cmd_args())

    def test_cmd_args_too_many_outputs(self):
        stdheptool = StdHepTool(name='tool', inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output1.stdhep', 'output2.stdhep'])
        self.assertEqual(len(stdheptool.output_files()), 2)
        self.assertRaises(Exception, lambda: stdheptools.cmd_args())


class TestBeamCoords(unittest.TestCase):

    def test_init(self):
        beam_coords = BeamCoords()
        self.assertEqual(beam_coords.name, 'beam_coords')
        self.assertEqual(beam_coords.append_tok, 'rot')

    def test_optional_parameters(self):
        beam_coords = BeamCoords()
        self.assertEqual(beam_coords.optional_parameters(), ['beam_sigma_x', 'beam_sigma_y', 'beam_rot_x', 'beam_rot_y', 'beam_rot_z', 'target_x', 'target_y', 'target_z'])

    def test_cmd_args(self):
        beam_coords = BeamCoords(inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep'])
        beam_coords.set_parameters({'beam_sigma_x': 11, 'beam_sigma_y': 12, 'beam_rot_x': 21, 'beam_rot_y': 22, 'beam_rot_z': 23, 'target_x': 31, 'target_y': 32, 'target_z': 33})
        self.assertEqual(beam_coords.cmd_args(), ['input1.stdhep', 'input2.stdhep', 'output.stdhep', '-s', '1', '-x', '11', '-y', '12', '-u', '21', '-v', '22', '-w', '23', '-X', '31', '-Y', '32', '-Z', '33'])


class TestRandomSample(unittest.TestCase):

    def test_init(self):
        random_sample = RandomSample()
        self.assertEqual(random_sample.name, 'random_sample')
        self.assertEqual(random_sample.append_tok, 'sampled')

    def test_optional_parameters(self):
        random_sample = RandomSample()
        self.assertEqual(random_sample.optional_parameters(), ['nevents', 'mu'])

    def test_cmd_args(self):
        random_sample = RandomSample(inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep'], seed=2)
        random_sample.set_parameters({'nevents': 100, 'mu': 1, 'seed': 2})
        self.assertEqual(random_sample.cmd_args(), ['input1.stdhep', 'input2.stdhep', 'output', '-s', '2', '-N', '1', '-n', '100', '-m', '1'])


class TestDisplaceTime(unittest.TestCase):

    def test_init(self):
        displace_time = DisplaceTime()
        self.assertEqual(displace_time.name, 'lhe_tridents_displacetime')
        self.assertEqual(displace_time.output_ext, '.stdhep')

    def test_optional_parameters(self):
        displace_time = DisplaceTime()
        self.assertEqual(displace_time.optional_parameters(), ['ctau'])

    def test_cmd_args(self):
        displace_time = DisplaceTime(inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep'])
        displace_time.set_parameters({'ctau': 1})
        self.assertEqual(displace_time.cmd_args(), ['input1.stdhep', 'input2.stdhep', 'output.stdhep', '-s', '1', '-l', '1'])


class TestDisplaceUni(unittest.TestCase):

    def test_init(self):
        displace_uni = DisplaceUni()
        self.assertEqual(displace_uni.name, 'lhe_tridents_displaceuni')
        self.assertEqual(displace_uni.output_ext, '.stdhep')

    def test_optional_parameters(self):
        displace_uni = DisplaceUni()
        self.assertEqual(displace_uni.optional_parameters(), ['ctau'])

    def test_cmd_args(self):
        displace_uni = DisplaceUni(inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep'])
        displace_uni.set_parameters({'ctau': 1})
        self.assertEqual(displace_uni.cmd_args(), ['input1.stdhep', 'input2.stdhep', 'output.stdhep', '-s', '1', '-l', '1'])


class TestAddMother(unittest.TestCase):

    def test_init(self):
        add_mother = AddMother()
        self.assertEqual(add_mother.name, 'add_mother')
        self.assertEqual(add_mother.append_tok, 'mom')


class TestMotherFullTruth(unittest.TestCase):

    def test_init(self):
        add_mother_full_truth = AddMotherFullTruth(inputs=['input1.stdhep', 'input2.lhe'], outputs=['output.stdhep'])
        self.assertEqual(add_mother_full_truth.name, 'add_mother_full_truth')
        self.assertEqual(add_mother_full_truth.append_tok, 'mom_full_truth')

    def test_init_with_wrong_inputs(self):
        self.assertRaises(Exception, lambda: AddMotherFullTruth(outputs=['output.stdhep']), msg="Must have 2 input files: a stdhep file and a lhe file in order")
        self.assertRaises(Exception, lambda: AddMotherFullTruth(inputs=['input1.stdhep', 'input2.lhe', 'input3.stdhep'], outputs=['output.stdhep']), msg="Must have 2 input files: a stdhep file and a lhe file in order")
        self.assertRaises(Exception, lambda: AddMotherFullTruth(inputs=['input1.stdhep'], outputs=['output.stdhep']), msg="Must have 2 input files: a stdhep file and a lhe file in order")
        self.assertRaises(Exception, lambda: AddMotherFullTruth(inputs=['input1.root', 'input2.lhe'], outputs=['output.stdhep']), msg="The first input file must be a stdhep file")
        self.assertRaises(Exception, lambda: AddMotherFullTruth(inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep']), msg="The second input file must be a lhe file")

    def test_cmd_args(self):
        add_mother_full_truth = AddMotherFullTruth(inputs=['input1.stdhep', 'input2.lhe'], outputs=['output.stdhep'])
        self.assertEqual(add_mother_full_truth.cmd_args(), ['input1.stdhep', 'input2.lhe', 'output.stdhep'])


class TestMergePoisson(unittest.TestCase):

    def test_init(self):
        merge_poisson = MergePoisson()
        self.assertEqual(merge_poisson.name, 'merge_poisson')
        self.assertEqual(merge_poisson.append_tok, 'sampled')
        self.assertEqual(merge_poisson.xsec, 0)

    def test_required_parameters(self):
        merge_poisson = MergePoisson()
        self.assertEqual(merge_poisson.required_parameters(), ['target_thickness', 'num_electrons'])

    def test_setup_1pt1_xsec1(self):
        merge_poisson = MergePoisson(inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep'], xsec=1)
        merge_poisson.set_parameters({'target_thickness': 0.0004062, 'num_electrons': 625})
        merge_poisson.setup()
        res = 6.306e-14 * 0.0004062 * 625
        self.assertEqual(merge_poisson.mu, res)

    def test_setup_1pt1_xsec20(self):
        merge_poisson = MergePoisson(inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep'], xsec=20)
        merge_poisson.set_parameters({'target_thickness': 0.0004062, 'num_electrons': 625})
        merge_poisson.setup()
        res = 6.306e-14 * 0.0004062 * 625 * 20
        self.assertEqual(merge_poisson.mu, res)

    def test_setup_3pt74_xsec1(self):
        merge_poisson = MergePoisson(inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep'], xsec=1)
        merge_poisson.set_parameters({'target_thickness': 0.000875, 'num_electrons': 625})
        merge_poisson.setup()
        res = 6.306e-14 * 0.000875 * 625
        self.assertEqual(merge_poisson.mu, res)

    def test_cmd_args(self):
        merge_poisson = MergePoisson(inputs=['input1.stdhep', 'input2.stdhep'], outputs=['output.stdhep'], xsec=1)
        merge_poisson.set_parameters({'target_thickness': 0.0004062, 'num_electrons': 625, 'mu': 1, 'seed': 2, 'nevents': 100})
        merge_poisson.setup()
        self.assertEqual(merge_poisson.cmd_args(), ['input1.stdhep', 'input2.stdhep', 'output', '-s', '2', '-m', str(6.306e-14 * 0.0004062 * 625), '-N', '1', '-n', '100'])


class TestMergeFiles(unittest.TestCase):

    def test_init(self):
        merge_files = MergeFiles()
        self.assertEqual(merge_files.name, 'merge_files')

    def test_optional_params(self):
        merge_files = MergeFiles()
        self.assertEqual(merge_files.optional_parameters(), [])

    def test_required_params(self):
        merge_files = MergeFiles()
        self.assertEqual(merge_files.required_parameters(), [])


if __name__ == '__main__':
    unittest.main()
