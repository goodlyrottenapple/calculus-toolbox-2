
import React, { Component } from 'react';

import './App.css';
import ProofTree from './ProofTree.js'
import ParserBar from './ParserBar.js'
// import DocName from './DocName.js'
import SwitchCalc from './SwitchCalc.js'
import AddAssm from './AddAssm.js'

import { getMacros } from './ServantApi.js'
import { urlPath, getPort } from './utils.js'
import menuTemplate from './menu-template.js'
import KaTeXRenderer from './KaTeXRenderer.js'


import { Segment, Header, Menu, Sidebar, Button, Icon } from 'semantic-ui-react'

export default class MainView extends Component {
  constructor() {
    super()
    this.setMenu();

    this.state = {
      macros: {},
      assms: [],
      name: '',
      ptSequent: {
        latex : '',
        term: {}
      },
      sidebarVisible: false,
      switchCalcVisible: false,
      addAssmVisible: false
    }
    // this.toggleCalcDesc = this.toggleCalcDesc.bind(this)  
    this.reloadMacros = this.reloadMacros.bind(this)  
    this.mkPTstub = this.mkPTstub.bind(this)
    this.updateName = this.updateName.bind(this)
    // this.openEdit = this.openEdit.bind(this)
    this.mkPT = this.mkPT.bind(this)
  }

  setMenu() {
    const electron = window.require('electron');
    const remote = electron.remote;
    const Menu = remote.Menu;

    const menu = Menu.buildFromTemplate(menuTemplate());
    Menu.setApplicationMenu(menu);

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


    const open = () => {
      const {dialog} = window.require('electron').remote;
      dialog.showOpenDialog(
        window.require('electron').remote.getCurrentWindow(),
        {
          filters: [
            { name: 'Calculus Session', extensions: ['session'] }
          ]
        },
        (fileName) => {
          if (fileName === undefined){
              console.log("You didn't open a file");
              return;
          }
          console.log(fileName)
          this.openSession(fileName);
        }
      );
    }

    const ipcRenderer = window.require('electron').ipcRenderer;
    ipcRenderer.on('menu:open', e => {
      open()
    })

    ipcRenderer.on('menu:save', e => {
      if(this.state.saveFile === undefined) saveAs();
      else this.saveSession(this.state.saveFile);
    })

    ipcRenderer.on('menu:saveAs', e => saveAs())

    // ipcRenderer.on('menu:edit', e => {
    //   this.openEdit();
    //   ipcRenderer.send('menu:edit2');
    // })

    ipcRenderer.on('menu:switch', e => {
      // this.switchCalc();
      this.toggle('switchCalcVisible')
    })

    ipcRenderer.on('menu:prefs', e => {
      this.openPrefs();
    })
  }


  mkPT(pt) {
    const r = Object.keys(pt)[0];
    const concl = pt[r].conclusion;
    this.setState({ptSequent:concl})
    const cs = pt[r].premises.reverse().map((c) => this.pt.fromJSONFileInput(c))
    this.pt.setState({rule:r, children:cs})
  }


  openSession(file) {
    const fs = window.require('fs')
    fs.readFile(file[0], (err, data) => {
      if (err) {
        return console.error(err);
      }
      const session = JSON.parse(data.toString())
      // console.log(session);

      this.mkPT(session.pt)
    });
  }

  saveSession(file) {
    const fs = window.require('fs')
    const content = JSON.stringify({ version:1, pt: this.pt.toJSON() });

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
    getMacros(getPort(), success, failure)
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

  mkPTstub(s) {
    this.setState({ptSequent: s})
    console.log(s)
  }

  updateName(n) {
    this.setState({name:n})
  }

  // openEdit() {
  //   const electron = window.require('electron');
  
  //   const remote = electron.remote;

  //   const BrowserWindow = remote.BrowserWindow;
  //   var win = new BrowserWindow({ width: 800, height: 700, title:"Modify calculus" });


  //   win.loadURL(urlPath('/edit/'+getPort()));
  //   win.setMenu(null);

  //   // const current = remote.getCurrentWindow();
  //   win.on('close', function () {
  //       // Dereference the window object, usually you would store windows
  //       // in an array if your app supports multi windows, this is the time
  //       // when you should delete the corresponding element.
  //       // win.removeAllListeners();
  //     win = null
  //   })
  // }

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

    const addAssm = (data) => {
      var array = this.state.assms;
      array.push(data);
      this.setState({ assms: array });
    }

    const removeAssm = (index) => {
      var array = this.state.assms;
      array.splice(index, 1);
      this.setState({ assms: array });
    }

    const assms = this.state.assms.map((a, index) => 
      <Segment key={a.latex+index}>
        <KaTeXRenderer math={a.latex} macros={this.state.macros}/>
        <Button basic icon="close" 
          style={{boxShadow:'none', fontSize: '0.8em', float:'right', marginTop: '-24px'}}
          onClick={() => removeAssm(index)}/>
      </Segment>)

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

          {this.state.assms.length > 0 && <Segment.Group>
            {assms}
          </Segment.Group>}
          <Button basic onClick={() => {console.log(this.state.assms); this.toggle('addAssmVisible')}}><Icon name='add' /> Add Assumption</Button>

          <AddAssm visible={this.state.addAssmVisible}
                   macros={this.state.macros} 
                   onClose={() => this.toggle('addAssmVisible')}
                   onAdd={(data) => {addAssm(data); this.toggle('addAssmVisible')}}/>
        </div>
      </Sidebar>
      <Sidebar.Pusher style={{minHeight: '100vh'}}>
        <div style={{paddingBottom: '30px'}}>
          <Button style={{float:'right', margin:'10px'}} basic circular icon='setting' onClick={() => this.toggle('sidebarVisible')} />
          
        </div>
        <div id="ProofTree">
          <ProofTree macros={this.state.macros}
                     assms={this.state.assms}
                     sequent={this.state.ptSequent}
                     saveSequent={(s,r) => this.setState({ptSequent: s, rule:r})} 
                     rule=""
                     ref={(node) => {this.pt = node}} />
        </div>
      </Sidebar.Pusher>
    </Sidebar.Pushable>)

    const switchFn = () => {
      this.toggle('switchCalcVisible')
      this.reloadMacros();
      this.setState({
        ptSequent: {
          latex : '',
          term: {}
        }
      })
    }

    //<DocName style={{float:'right', margin:'10px'}} onEdit={this.updateName}/>

    return (<div className="App">
        {sidebarArea}
        <SwitchCalc visible={this.state.switchCalcVisible} 
                    onClose={() => this.toggle('switchCalcVisible')}
                    onSwitch={switchFn}/>
        <ParserBar macros={this.state.macros} callback={this.mkPTstub}/>
      </div>)
  }
}