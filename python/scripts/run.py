#!/usr/bin/env python

from hpsmc.batch import Local

if __name__ == "__main__":
    b = Local()
    b.parse_args()
    b.submit_all()
