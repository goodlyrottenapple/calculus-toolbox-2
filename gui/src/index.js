import React from 'react';
import ReactDOM from 'react-dom';
import { HashRouter } from 'react-router-dom'
import './index.css';
import 'semantic-ui-css/semantic.min.css';
import '../node_modules/katex/dist/katex.min.css'

import App from './App';
// import registerServiceWorker from './registerServiceWorker';

ReactDOM.render(<HashRouter><App /></HashRouter>, document.getElementById('root'));
// registerServiceWorker();
