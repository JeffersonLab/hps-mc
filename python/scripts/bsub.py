#!/usr/bin/env python

from hpsmc.batch import LSF

if __name__ == "__main__":
    b = LSF()
    b.parse_args()
    b.submit()
