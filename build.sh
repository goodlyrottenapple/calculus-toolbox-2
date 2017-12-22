#!/bin/sh

mkdir -p gui/resources
stack install
upx gui/resources/calculus-toolbox
cd gui
resources/calculus-toolbox generateservantapi 'src/ServantApi.js'
npm install
npm run build
npm run dist
