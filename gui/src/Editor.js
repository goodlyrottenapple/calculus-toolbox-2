
import React, { Component } from 'react';
import './App.css';
import CalcDescription from './CalcDescription.js'

import { getMacros } from './ServantApi.js'

import { Dropdown, Segment, Header, Image, Menu, Icon, Sidebar, Button } from 'semantic-ui-react'

class Editor extends Component {


  constructor() {
    super()
    this.state = {
      addingCalcDesc: false,
      macros: {},
      name: '',
      ptSequent: {
        latex : '',
        term: {}
      },
      sidebarVisible: false
    }
    // this.toggleCalcDesc = this.toggleCalcDesc.bind(this)  
    this.reloadMacros = this.reloadMacros.bind(this)  
    this.mkPT = this.mkPT.bind(this)
    this.updateName = this.updateName.bind(this)

  }

  componentDidMount() {
    this.reloadMacros()
  }

  reloadMacros() {
    const success = (data) => this.setState({ macros: data })
    const failure = (data) => console.log(data)
    getMacros('private', success, failure)
  }

  handleErrors(response) {
    if (response.ok) {
      return response.json();
    }
    var error = new Error()
    error.data = response.json();
    throw error;
  }

  toggle(x) {
    this.setState({[x] : !this.state[x]})
  }

  mkPT(s) {
    this.setState({ptSequent: s})
    console.log(s)
  }

  updateName(n) {
    this.setState({name:n})
  }

  newWindow() {
    const electron = window.require('electron');
      // const fs = electron.remote.require('fs');
      // const ipcRenderer = electron.ipcRenderer;

      const remote = electron.remote;
      const BrowserWindow = remote.BrowserWindow;
    var win = new BrowserWindow({ width: 800, height: 600 });
  }

  render() {
    return (

      <div className="App">
        <CalcDescription 
          open={true} 
          onClose={() => this.toggle('addingCalcDesc')}
          onSave={this.reloadMacros} />
        
      </div>
    );
  }
}

export default App;
