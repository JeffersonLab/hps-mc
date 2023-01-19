#!/bin/bash
autopep8 --aggressive --aggressive --ignore=E265,E266,E501,E121,E123,E126,E133,E226,E241,E242,E704,W503,W504,W505 --in-place --max-line-length=160 --recursive python/
