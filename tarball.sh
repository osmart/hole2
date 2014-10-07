#!/bin/bash
# command used to packup distribution tar ball
tar cvzf hole2.2.x.x.tar.gz --exclude README.md * --exclude src --exclude 'hole*.tar.gz' --exclude index.asciidoc --exclude .ignore --exclude tarball.sh
