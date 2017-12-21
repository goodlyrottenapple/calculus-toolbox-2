
import React, { Component } from 'react';
import './App.css';
import ProofTree from './ProofTree.js'
import ParserBar from './ParserBar.js'
import DocName from './DocName.js'

import { getMacros } from './ServantApi.js'
import { urlPath, getPort } from './utils.js'
import menuTemplate from './menu-template.js'

import { Segment, Header, Menu, Sidebar, Button } from 'semantic-ui-react'

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
      sidebarVisible: false
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

    const menu = Menu.buildFromTemplate(menuTemplate());
    Menu.setApplicationMenu(menu);

    const ipcRenderer = window.require('electron').ipcRenderer;
    ipcRenderer.on('menu:open', e => {
      console.log("open")
      console.log(this.state)
    })

    const saveAs = () => {
      const {dialog} = window.require('electron').remote;
      dialog.showSaveDialog(
        window.require('electron').remote.getCurrentWindow(),
        {
          filters: [
            { name: 'Calculus Session', extensions: ['session'] }
          ]
        },
        (fileName) => {
          if (fileName === undefined){
              console.log("You didn't save the file");
              return;
          }
          this.setState({saveFile: fileName});
          this.saveSession(fileName);
        }
      );
    }


    ipcRenderer.on('menu:save', e => {
      if(this.state.saveFile === undefined) saveAs();
      else this.saveSession(this.state.saveFile);
    })

    ipcRenderer.on('menu:saveAs', e => saveAs())

    ipcRenderer.on('menu:edit', e => {
      this.openEdit();
    })

    ipcRenderer.on('menu:prefs', e => {
      this.openPrefs();
    })
  }

  saveSession(file) {
    const fs = window.require('fs')
    const content = JSON.stringify( this.pt.toJSON() );

    fs.writeFile(file, content, (err) => {
      if(err){
          alert("An error ocurred creating the file "+ err.message)
      }       
      // alert("The session has been succesfully saved");
    });
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
    getMacros(getPort(), 'private', success, failure)
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
    var win = new BrowserWindow({ width: 800, height: 700, title:"Modify calculus" });


    win.loadURL(urlPath('/edit/'+getPort()));
    win.setMenu(null);

    // const current = remote.getCurrentWindow();
    win.on('close', function () {
        // Dereference the window object, usually you would store windows
        // in an array if your app supports multi windows, this is the time
        // when you should delete the corresponding element.
        // win.removeAllListeners();
      win = null
    })


    //   // const fs = electron.remote.require('fs');
    
    //   const {dialog} = window.require('electron').remote
    //   console.log(dialog.showOpenDialog({properties: ['openFile', 'openDirectory', 'multiSelections']}))

      // console.log(electron.dialog.showOpenDialog({properties: ['openFile', 'openDirectory', 'multiSelections']}))

      
  }

  openPrefs() {
    const electron = window.require('electron');
  
    const remote = electron.remote;

    const BrowserWindow = remote.BrowserWindow;
    var win = new BrowserWindow({ width: 800, height: 600, title:"Modify calculus" });

    win.loadURL(urlPath('/preferences/initial'));
    win.setMenu(null);

    win.on('close', function () {
      win = null
    })      
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
    // const MainMenu = (
    //   <Dropdown id="mainMenu" text='Menu' floating>
    //     <Dropdown.Menu>
    //       <Dropdown.Item onClick={() => this.newWindow()}>Load Calculus</Dropdown.Item>
    //       <Dropdown.Item onClick={this.openEdit}>Modify Calculus</Dropdown.Item>
    //     </Dropdown.Menu>
    //   </Dropdown>
    // )

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
          <Header style={{marginTop:'26px'}} textAlign='left' size='tiny'>Assumptions</Header>

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
                     rule=""
                     ref={(node) => {this.pt = node}} />
        </div>
      </Sidebar.Pusher>
    </Sidebar.Pushable>)

    return <div className="App">
        {sidebarArea}
        <ParserBar macros={this.state.macros} callback={this.mkPT}/>
      </div>;
  }
}