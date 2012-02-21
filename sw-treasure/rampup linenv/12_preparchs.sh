#!/bin/bash
echo "unzipping prepared-eclipse in silent mode (please wait) ..."
tar xzf archs2unzip/prepared-eclipse.tgz -C "$HOME/progs"
echo "unzipping Maven 3 in silent mode (please wait) ..."
unzip -q archs2unzip/apache-maven-3.0.2-bin.zip -d "$HOME/progs"
