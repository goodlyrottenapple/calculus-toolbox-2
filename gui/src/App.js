
import React, { Component } from 'react';
import CalcDescription from './CalcDescription.js'
import Preferences from './Preferences.js'
import MainView from './MainView.js'
// import { getPort } from './utils.js'

import { Route, Switch } from 'react-router-dom'

export default class App extends Component {
  render() {
  	const Settings = ({ match }) => <Preferences opts={match.params.opts}/>

    window.require('electron').webFrame.setVisualZoomLevelLimits(1, 1);

    return (
      <Switch>
        <Route path={`/edit`} component={CalcDescription}/>
        <Route path={`/preferences/:opts`} component={Settings}/>
        <Route path={`/`} component={MainView}/>
      </Switch>
    );
  }
}