import { join } from 'path';
import { format } from 'url';

function urlPath(p) {
  console.log("__dirname: " + window.__dirname)
  console.log("path: " +  join(window.__dirname, '/../build/index.html'))
  console.log("ELECTRON_START_URL: " + window.process.env.ELECTRON_START_URL)
  var editUrl;
  if (window.process.env.ELECTRON_START_URL)
    if (window.process.platform === "win32") editUrl = window.process.env.ELECTRON_START_URL + "#" + p
    else editUrl = editUrl = join(window.process.env.ELECTRON_START_URL , "#", p)
  else
    editUrl = format({
      pathname: join(window.__dirname, '/../build/index.html'),
      hash: p,
      protocol: 'file:',
      slashes: true
    });
  return editUrl;
}

function getPort() {
  return window.require('electron').remote.getCurrentWindow().port
}

export { urlPath, getPort }