
import React, { Component } from 'react';
import CalcDescription from './CalcDescription.js'
import Preferences from './Preferences.js'
import MainView from './MainView.js'
import { getPort } from './utils.js'

import { Route, Switch } from 'react-router-dom'

export default class App extends Component {
  render() {
  	const Settings = ({ match }) => <Preferences opts={match.params.opts}/>
  	const CalcDesc = ({ match }) => <CalcDescription port={match.params.port}/>

    return (
      <Switch>
        <Route path={`/edit/:port`} component={CalcDesc}/>
        <Route path={`/preferences/:opts`} component={Settings}/>
        <Route path={`/`} component={MainView}/>
      </Switch>
    );
  }
}