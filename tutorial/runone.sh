#!/bin/bash
cd sbt; sbt "project tutorial" "run $1 --compile --test"
