
import React, { Component } from 'react';
import './App.css';
import CalcDescription from './CalcDescription.js'
import ProofTree from './ProofTree.js'
import ParserBar from './ParserBar.js'
import DocName from './DocName.js'

import { getMacros } from './ServantApi.js'

import { Dropdown, Segment, Header, Image, Menu, Icon, Sidebar, Button } from 'semantic-ui-react'
import { Route, Switch } from 'react-router-dom'

class App extends Component {


  constructor() {
    super()

    this.state = {
      macros: {},
      name: '',
      ptSequent: {
        latex : '',
        term: {}
      },
      sidebarVisible: false,
      //editWindow: win
    }

    // this.toggleCalcDesc = this.toggleCalcDesc.bind(this)  
    this.reloadMacros = this.reloadMacros.bind(this)  
    this.mkPT = this.mkPT.bind(this)
    this.updateName = this.updateName.bind(this)
    this.openEdit = this.openEdit.bind(this)

  }

  componentDidMount() {
    this.reloadMacros()
    const ipcRenderer = window.require('electron').ipcRenderer;
    ipcRenderer.on('updateMacros2', e => {
        console.log("reloading Macros!"); // logs out "Hello second window!"
        this.reloadMacros()
    })
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

  openEdit() {

    const electron = window.require('electron');
  
    const remote = electron.remote;
    const BrowserWindow = remote.BrowserWindow;
    var win = new BrowserWindow({ width: 800, height: 600 });
    win.loadURL('http://localhost:3000/edit');

    win.on('closed', function () {
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        win = null
    })


    //   // const fs = electron.remote.require('fs');
    
    //   const {dialog} = window.require('electron').remote
    //   console.log(dialog.showOpenDialog({properties: ['openFile', 'openDirectory', 'multiSelections']}))

      // console.log(electron.dialog.showOpenDialog({properties: ['openFile', 'openDirectory', 'multiSelections']}))

      
  }

  render() {
    const MainMenu = (
      <Dropdown id="mainMenu" text='Menu' floating>
        <Dropdown.Menu>
          <Dropdown.Item onClick={() => this.newWindow()}>Load Calculus</Dropdown.Item>
          <Dropdown.Item onClick={this.openEdit}>Modify Calculus</Dropdown.Item>
        </Dropdown.Menu>
      </Dropdown>
    )

    const sidebarArea = (<Sidebar.Pushable as={Segment} style={{marginBottom: '0px', border:'0px'}}>
          <Sidebar
            as={Menu}
            style={{borderTopWidth: '0px', borderBottomWidth: '0px', borderRightWidth: '0px'}}
            animation='overlay'
            width='wide'
            direction='right'
            visible={this.state.sidebarVisible}
            icon='labeled'
            vertical
          >
            <Button style={{float:'right', margin:'10px'}} basic circular icon='close' onClick={() => this.toggle('sidebarVisible')} />
            <div style={{margin:'10px'}}>
              <Header style={{marginTop:'56px'}} textAlign='left' size='tiny'>Assumptions</Header>

              <Segment.Group >
                <Segment>Nested Top</Segment>
                <Segment>Nested Middle</Segment>
                <Segment>Nested Bottom</Segment>
              </Segment.Group>
            </div>
          </Sidebar>
          <Sidebar.Pusher style={{minHeight: '100vh'}}>
            <div style={{paddingBottom: '30px'}}>
              {MainMenu}
              <Button style={{float:'right', margin:'10px'}} basic circular icon='setting' onClick={() => this.toggle('sidebarVisible')} />
              <DocName style={{float:'right', margin:'10px'}} onEdit={this.updateName}/>
            </div>
            <div id="ProofTree">
              <ProofTree macros={this.state.macros} 
                         sequent={this.state.ptSequent} 
                         saveSequent={(s,r) => this.setState({ptSequent: s, rule:r})} 
                         rule=""/>
            </div>
          </Sidebar.Pusher>
        </Sidebar.Pushable>)

    const main = () => (
      <div className="App">
        {sidebarArea}
        <ParserBar macros={this.state.macros} callback={this.mkPT}/>
      </div>)

    const edit = () => <CalcDescription 
      open={true} 
      onClose={() => this.toggle('addingCalcDesc')}
      onSave={this.reloadMacros} />

    return (
      <Switch>
        <Route path="/edit" render={edit}/>
        <Route path="/" render={main}/>
      </Switch>
    );
  }
}

export default App;
