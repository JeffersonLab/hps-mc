#!/usr/bin/env python

from hpsmc.workflow import Workflow

if __name__ == "__main__":
    workflow = Workflow()
    workflow.parse_args()
    workflow.build()
