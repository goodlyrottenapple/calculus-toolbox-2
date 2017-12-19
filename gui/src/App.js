
import React, { Component } from 'react';
import CalcDescription from './CalcDescription.js'
import MainView from './MainView.js'

import { Route, Switch } from 'react-router-dom'

export default class App extends Component {
  render() {
    return (
      <Switch>
        <Route path="/edit" component={CalcDescription}/>
        <Route path="/" component={MainView}/>
      </Switch>
    );
  }
}