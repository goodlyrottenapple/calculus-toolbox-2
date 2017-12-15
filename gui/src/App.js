
import React, { Component } from 'react';
import './App.css';
import CalcDescription from './CalcDescription.js'
import ProofTree from './ProofTree.js'
import ParserBar from './ParserBar.js'
import DocName from './DocName.js'
import MainView from './MainView.js'

import { getMacros } from './ServantApi.js'

import { Dropdown, Segment, Header, Image, Menu, Icon, Sidebar, Button } from 'semantic-ui-react'
import { Route, Switch } from 'react-router-dom'

class App extends Component {


  constructor() {
    super()
  }

  render() {
    return (
      <Switch>
        <Route path="/edit" component={CalcDescription}/>
        <Route path="/" component={MainView}/>
      </Switch>
    );
  }
}

export default App;
