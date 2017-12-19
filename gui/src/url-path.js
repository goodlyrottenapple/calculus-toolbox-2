import { join } from 'path';
import { format } from 'url';

export default (p) => {
  var editUrl;
  if (window.process.env.ELECTRON_START_URL) 
    editUrl = join(window.process.env.ELECTRON_START_URL , "#", p)
  else
    editUrl = format({
      pathname: join(window.__dirname, '/../build/index.html'),
      hash: p,
      protocol: 'file:',
      slashes: true
    });
  return editUrl;
};