#!/usr/bin/env python

from hpsmc.batch import LSF

if __name__ == "__main__":
    lsf = LSF()
    lsf.parse_args()
    lsf.update()
