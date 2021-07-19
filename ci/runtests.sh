#!/bin/sh
emacs -Q -batch -l pekobj-tests.el -l ring-tests.el -f ert-run-tests-batch-and-exit
