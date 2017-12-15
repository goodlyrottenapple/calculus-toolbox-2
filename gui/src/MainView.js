
import React, { Component } from 'react';
import './App.css';
import CalcDescription from './CalcDescription.js'
import ProofTree from './ProofTree.js'
import ParserBar from './ParserBar.js'
import DocName from './DocName.js'

import { getMacros } from './ServantApi.js'

import { Dropdown, Segment, Header, Image, Menu, Icon, Sidebar, Button } from 'semantic-ui-react'
import { Route, Switch } from 'react-router-dom'

export default class MainView extends Component {


  constructor() {
    super()
    this.setMenu();

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


  setMenu() {
    const electron = window.require('electron');
    const remote = electron.remote;
    const Menu = remote.Menu;

    var template = require('./menu-template.js')


    if (window.process.platform === 'darwin') {
      console.log(electron.app)
      const name = 'aaa' //app.getName()
      template.unshift({
        label: name,
        submenu: [
          {
            role: 'about'
          },
          {
            type: 'separator'
          },
          {
            role: 'services',
            submenu: []
          },
          {
            type: 'separator'
          },
          {
            role: 'hide'
          },
          {
            role: 'hideothers'
          },
          {
            role: 'unhide'
          },
          {
            type: 'separator'
          },
          {
            role: 'quit'
          }
        ]
      })
      // Edit menu.
      template[2].submenu.push(
        {
          type: 'separator'
        },
        {
          label: 'Speech',
          submenu: [
            {
              role: 'startspeaking'
            },
            {
              role: 'stopspeaking'
            }
          ]
        }
      )
      // Window menu.
      template[4].submenu = [
        {
          label: 'Close',
          accelerator: 'CmdOrCtrl+W',
          role: 'close'
        },
        {
          label: 'Minimize',
          accelerator: 'CmdOrCtrl+M',
          role: 'minimize'
        },
        {
          label: 'Zoom',
          role: 'zoom'
        },
        {
          type: 'separator'
        },
        {
          label: 'Bring All to Front',
          role: 'front'
        }
      ]
    }

    const menu = Menu.buildFromTemplate(template);
    Menu.setApplicationMenu(menu);

    const ipcRenderer = window.require('electron').ipcRenderer;
    ipcRenderer.on('menu:open', e => {
      console.log("open")
      console.log(this.state)
    })
    ipcRenderer.on('menu:save', function () {
      console.log("save")
    })
    ipcRenderer.on('menu:edit', e => {
      this.openEdit();
    })
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
    win.setMenu(null);

    const current = remote.getCurrentWindow();
    win.on('close', function () {
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        // win.removeAllListeners();
        win = null
        
        // this is really hacky, but it's the only way menus work after closing the edit window
        //but keeping the main window open...
        // const electron = window.require('electron');
        // const remote = electron.remote;
        // const Menu = remote.Menu;

        // var template = require('./menu-template.js')

        // const menu = Menu.buildFromTemplate(template);
        // Menu.setApplicationMenu(menu);

    })


    //   // const fs = electron.remote.require('fs');
    
    //   const {dialog} = window.require('electron').remote
    //   console.log(dialog.showOpenDialog({properties: ['openFile', 'openDirectory', 'multiSelections']}))

      // console.log(electron.dialog.showOpenDialog({properties: ['openFile', 'openDirectory', 'multiSelections']}))

      
  }


  // implement navigation using arrow keys? will be a pain ...

  // _handleKeyDown (event) {
  //   console.log(event.keyCode)
  //   // const SPACE = 32, LEFT_ARR = 37, UP_ARR = 38, RIGHT_ARR, DOWN_ARR = 40, A = 65
  //   // switch( event.keyCode ) {
  //   //     case ESCAPE_KEY:
  //   //         // this.state.activePopover.hide();

  //   //         break;
  //   //     default: 
  //   //         break;
  //   // }
  // }

  // componentWillMount() {
  //   document.addEventListener("keydown", this._handleKeyDown.bind(this));
  // }

  // componentWillUnmount() {
  //   document.removeEventListener("keydown", this._handleKeyDown.bind(this));
  // }


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

    return <div className="App">
        {sidebarArea}
        <ParserBar macros={this.state.macros} callback={this.mkPT}/>
      </div>;
  }
}