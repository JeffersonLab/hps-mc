# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

hps-mc is a Python-based Monte Carlo simulation framework for the Heavy Photon Search (HPS) experiment. It provides a component-based job management system for running physics simulations through multiple stages: event generation, detector simulation (SLIC), and readout/reconstruction. Jobs are defined using JSON configuration and can be submitted to various batch systems (local, pool, Slurm, LSF).

## Build and Installation

### Prerequisites
- gcc >= 4.8, CMake >= 3.18, Maven >= 3.0, Python >= 3.6, Java 1.8
- GSL 1.16: `GSL_ROOT_DIR` must point to installation prefix
- Python packages: `pip install psutil jinja2`
- External tools: `slic` and `dst_maker` must be in PATH

### Build Commands
```bash
# Basic build
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=../install -DGSL_ROOT_DIR=$GSL_ROOT_DIR ..
make install

# Full build with all dependencies
cmake -DCMAKE_INSTALL_PREFIX=$(realpath ../install) \
      -DGSL_ROOT_DIR=$GSL_ROOT_DIR \
      -DENABLE_INSTALL_GENERATORS=ON \
      -DENABLE_INSTALL_FIELDMAPS=ON \
      -DENABLE_INSTALL_LCIO=ON \
      -DENABLE_INSTALL_HPSJAVA=ON \
      -DENABLE_INSTALL_CONDITIONS=ON ..
make install
```

### Environment Setup
```bash
# Must be sourced before running any scripts
source install/bin/hps-mc-env.sh  # bash
source install/bin/hps-mc-env.csh # csh
```

## Testing

### Python Unit Tests
```bash
cd python/test

# Run all tests
python3 -m unittest -v

# Run specific test module
python3 -m unittest -v test_component.py

# Run specific test class or method
python3 -m unittest -v test_component.TestClass.test_method
```

### Code Formatting
The repository enforces PEP 8 style via GitHub Actions with specific ignores.

Check formatting before committing:
```bash
pycodestyle --ignore=E266,E501,E121,E123,E126,E133,E226,E241,E242,E704,W503,W504,W505 \
            --max-line-length=160 python/
```

Auto-fix formatting:
```bash
autopep8 --aggressive --aggressive \
         --ignore=E265,E266,E501,E121,E123,E126,E133,E226,E241,E242,E704,W503,W504,W505 \
         --in-place --max-line-length=160 --recursive python/
# Or use: ./scripts/format_python_code.sh
```

## Architecture

### Component-Based Job Framework

The framework uses a **Job** + **Component** pattern:
- **Job** (`python/hpsmc/job.py`): Container that manages execution of components, file I/O, configuration
- **Component** (`python/hpsmc/component.py`): Base class for all processing units (generators, tools, etc.)

Each component:
- Has `execute()` method that runs its command
- Takes inputs/outputs, handles file chaining automatically
- Configured via system-level (`~/.hpsmc`) and job-level (`.hpsmc`) config files
- Can access job parameters (seed, nevents, etc.) and global config

### Component Types

**Event Generators** (`python/hpsmc/generators.py`):
- `MG4`, `MG5`: MadGraph event generators (v4 and v5)
- `EGS5`: Electron-gamma shower generator
- `StdHepConverter`: Converts EGS5 output to StdHep format

**Tools** (`python/hpsmc/tools.py`):
- `SLIC`: Geant4-based detector simulation
- `JobManager`: Java-based HPS analysis framework
- `HPSTR`: ROOT-based analysis tool
- `LCIOTool`, `JavaTool`: Base classes for LCIO/Java operations
- `MergeROOT`: Merges ROOT files using hadd
- `StdHepTool`: StdHep file manipulation

**Other Components** (`python/hpsmc/_hadd.py`):
- `hadd`: Direct ROOT histogram/TTree merger

### Configuration Hierarchy

Configs are loaded from multiple locations (later overrides earlier):
1. System-level: `~/.hpsmc` (see `config/bravo_sdf.cfg` for template)
2. Job-level: `.hpsmc` in job directory

Config sections match component class names. Example:
```ini
[SLIC]
slic_dir = /path/to/slic/install
detector_dir = /path/to/detectors

[JobManager]
hps_java_bin_jar = /path/to/hps-distribution.jar
java_args = -Xmx1g -XX:+UseSerialGC
```

### Job Definition Workflow

1. **Define job script** (`python/jobs/*_job.py`):
   - Import components, configure, add to job
   - Example: `tritrig_gen_job.py` imports MG5, adds to job

2. **Create job template** (`job.json.tmpl`):
   - Uses Jinja2 templating for parameters
   - Variables wrapped in `{{ variable }}`
   - Special variable `job_id` always available

3. **Define variables** (`vars.json`):
   - Lists of values to iterate over
   - Example: `{"nevents": [1000], "run_params": ["4pt55"]}`

4. **Generate jobs.json**:
   ```bash
   hps-mc-job-template -j <num_jobs> -r <run_number> -a vars.json job.json.tmpl jobs.json
   ```

5. **Execute jobs**:
   ```bash
   # Single job locally
   hps-mc-job run -d /scratch/output job_script jobs.json

   # Multiple jobs in pool (2 cores)
   hps-mc-batch pool -p 2 job_script jobs.json

   # Submit to Slurm
   hps-mc-batch slurm -l logs/ -d jobs/ -S scripts/ -q shared -W 2 -m 2000 job_script jobs.json
   ```

### Production Pipeline Stages

The typical simulation follows a three-stage pipeline:

1. **Generation (gen)**: Event generation
   - Tools: MG4/MG5 for signal, EGS5 for beam backgrounds
   - Output: StdHep files

2. **Simulation (slic)**: Detector response
   - Tool: SLIC (Geant4)
   - Preprocessing: coordinate transformations, event sampling
   - Output: SLCIO files

3. **Readout/Reconstruction (recon)**: Digitization and reconstruction
   - Tool: JobManager (HPS Java)
   - Post-processing: ROOT conversion via HPSTR
   - Output: LCIO → ROOT files

See `prod/prod.md` for production documentation.

### Batch System Integration

`python/hpsmc/batch.py` provides unified interface to:
- **local**: Single job execution
- **pool**: Multiprocessing pool (n cores)
- **slurm**: SLURM batch system
- **lsf**: LSF batch system
- **auger**: Auger system

All use same command structure; batch system handles job preparation/submission differently.

### File Merging

Recent addition: ROOT file merging workflow (`python/hpsmc/prepare_merge_jobs.py`)
- Scans directories for ROOT files (pattern: `hps_*/*.root`)
- Batches files into groups (default: 20 files/job)
- Generates input file lists for `hps-mc-job-template`
- Jobs use `MergeROOT` component with `hadd` under the hood

Commands:
```bash
# Prepare merge job configurations
hps-mc-prepare-merge-jobs -p /path/to/parent_dir -o merge_jobs

# Generate jobs.json from prepared configs
hps-mc-job-template -j <num_jobs> -a merge_vars.json merge_job.json.tmpl jobs.json

# Run merge jobs
hps-mc-batch pool -p 4 root_merge jobs.json
```

## Common Patterns

### Reading Job Parameters in Job Scripts
```python
# Access parameters from JSON
nevents = job.params.get('nevents', 1000)
seed = job.params.get('seed', 1)
run_number = job.params['run_number']  # Required param
```

### Component Input/Output Chaining
Jobs automatically chain outputs → inputs when `enable_file_chaining=True`:
```python
# gen.outputs[0] automatically becomes slic.inputs[0]
gen = MG5(name='tritrig', outputs=['events.stdhep'])
slic = SLIC(inputs=['events.stdhep'], outputs=['sim.slcio'])
job.add([gen, slic])
```

### Run Parameters by Beam Energy
`python/hpsmc/run_params.py` contains physics parameters indexed by beam energy string (e.g., "4pt55" = 4.55 GeV):
- `aprime_mass`: A' mass points in MeV
- `target_thickness`: Target thickness in cm
- `beam_energy`: Beam energy in MeV

Used extensively in job templates.

### Adding New Components
1. Subclass `Component` (or `EventGenerator`, `JavaTool`, `LCIOTool`)
2. Implement `__init__()`, `cmd_args()`, optionally `execute()`
3. Set `command`, `inputs`, `outputs`
4. Import in appropriate module (`generators.py` or `tools.py`)
5. Use in job scripts

## Important Notes

- **HPSMC_DIR must be set**: All components require `$HPSMC_DIR` environment variable (set by env script)
- **Working directory**: Run jobs from scratch directories; components create many temporary files
- **Job ID**: Always available in templates as `{{ job_id }}`; commonly used for unique seeds
- **File paths**: Use absolute paths for detector files, macros, external resources
- **Dry run**: Set `dry_run=True` in Job config section to test without execution
- **Development workflow**: Test jobs locally or in pool mode before submitting to batch systems. Start with small job counts to validate configurations.

## Getting Help

- HPS Slack: [#montecarlo channel](https://heavyphotonsearch.slack.com/messages/C47LLBP5F)
- HPS software mailing list
- Documentation: https://jeffersonlab.github.io/hps-mc/
