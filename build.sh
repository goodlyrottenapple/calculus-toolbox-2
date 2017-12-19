#!/bin/sh

mkdir -p gui/resources
stack install
gzexe gui/resources/calculus-toolbox
rm gui/resources/calculus-toolbox~
cd gui
resources/calculus-toolbox generateservantapi 'src/ServantApi.js'
npm run build
npm run dist