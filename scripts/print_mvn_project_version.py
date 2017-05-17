#!/usr/bin/env python

import sys
import xml.etree.ElementTree as ET

tree = ET.parse(sys.argv[1])
root = tree.getroot()
for child in root:
    if "version" in child.tag[child.tag.rfind('}')+1:]:
        print child.text
