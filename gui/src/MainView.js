
import React, { Component } from 'react';

import './App.css';
import ProofTree from './ProofTree.js'
import ParserBar from './ParserBar.js'
// import DocName from './DocName.js'
import SwitchCalc from './SwitchCalc.js'
import ParseTermModal from './ParseTermModal.js'

import { getMacros, postParseDSeq, postParseFormula } from './ServantApi.js'
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
      axioms: [],
      name: '',
      ptSequent: {
        latex : '',
        term: {}
      },
      abbrevs: {"abbrevsFormula":[],"abbrevsStructure":[]},
      sidebarVisible: false,
      switchCalcVisible: false,
      addAxiomVisible: false,
      addAbbrevsFVisible: false,
      currentZoom: 1
    }
    // this.toggleCalcDesc = this.toggleCalcDesc.bind(this)  
    this.reloadMacros = this.reloadMacros.bind(this)  
    this.mkPTstub = this.mkPTstub.bind(this)
    this.updateName = this.updateName.bind(this)
    // this.openEdit = this.openEdit.bind(this)
    this.mkPT = this.mkPT.bind(this)
    this.getAbbrevs = this.getAbbrevs.bind(this)
    this.openSession = this.openSession.bind(this)
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

    ipcRenderer.on('menu:zoomIn', e => {
      console.log("zoom In")
      this.setState({
        currentZoom: this.state.currentZoom + 0.05 < 2 ? 
          this.state.currentZoom + 0.05 : this.state.currentZoom
      })
    })

    ipcRenderer.on('menu:zoomOut', e => {
      this.setState({
        currentZoom: this.state.currentZoom - 0.05 > 0.2 ? 
          this.state.currentZoom - 0.05 : this.state.currentZoom
      })
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

      if(session.version > 1) {
        console.log(session);
        this.setState({axioms: session.axioms, abbrevs: session.abbrevs})
      }
      this.mkPT(session.pt)
    });
  }

  saveSession(file) {
    const fs = window.require('fs')
    const content = JSON.stringify({ version:2, pt: this.pt.toJSON(), axioms: this.state.axioms, abbrevs: this.state.abbrevs });

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
    const success = (data) => {
      console.log(data)
      this.setState({ macros: data })
    }
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

  getAbbrevs() {
    const abbrevsF = this.state.abbrevs.abbrevsFormula.map((e) => [e[0], e[1].term])
    const abbrevsS = this.state.abbrevs.abbrevsStructure.map((e) => [e[0], e[1].term])
    return {abbrevsFormula: abbrevsF, abbrevsStructure: abbrevsS}
  }


  render() {
    // const MainMenu = (
    //   <Dropdown id="mainMenu" text='Menu' floating>
    //     <Dropdown.Menu>
    //       <Dropdown.Item onClick={() => this.newWindow()}>Load Calculus</Dropdown.Item>
    //       <Dropdown.Item onClick={this.openEdit}>Modify Calculus</Dropdown.Item>
    //     </Dropdown.Menu>
    //   </Dropdown>
    // )

    const addAxiom = (data) => {
      var array = this.state.axioms;
      array.push(data);
      this.setState({ axioms: array });
    }

    const removeAxiom = (index) => {
      var array = this.state.axioms;
      array.splice(index, 1);
      this.setState({ axioms: array });
    }

    const axioms = this.state.axioms.map((a, index) => 
      <Segment key={a.latex+index}>
        <KaTeXRenderer math={a.latex} macros={this.state.macros}/>
        <Button basic icon="close" 
          style={{boxShadow:'none', fontSize: '0.8em', float:'right', marginTop: '-24px'}}
          onClick={() => removeAxiom(index)}/>
      </Segment>)


    const abbrevsF = this.state.abbrevs.abbrevsFormula.map((a, index) => 
      <Segment key={a[1].latex+index}>
        <div style={{overflowY:'hidden', marginRight:'30px'}}>
          <KaTeXRenderer math={`${a[0][0]} = ${a[1].latex}`} macros={this.state.macros}/>
        </div>
        <Button basic icon="close" 
          style={{boxShadow:'none', fontSize: '0.8em', float:'right', marginTop: '-24px'}}
          onClick={() => {
            console.log("being called")
            var array = this.state.abbrevs;
            array.abbrevsFormula.splice(index, 1);
            this.setState({ abbrevs: array });
          }}
          />
      </Segment>)

    const sidebarArea = (<Sidebar.Pushable as={Segment} style={{marginBottom: '0px', border:'0px'}}>
      <Sidebar
        as={Menu}
        style={{borderTopWidth: '0px', borderBottomWidth: '0px', borderRightWidth: '0px', paddingBottom:'70px'}}
        animation='overlay'
        width='wide'
        direction='right'
        visible={this.state.sidebarVisible}
        icon='labeled'
        vertical
      >
        <Button style={{float:'right', margin:'10px'}} basic circular icon='close' onClick={() => this.toggle('sidebarVisible')} />
        <div style={{margin:'10px'}}>
          <Header style={{marginTop:'26px'}} textAlign='left' size='tiny'>Axioms</Header>

          {this.state.axioms.length > 0 && <Segment.Group>
            {axioms}
          </Segment.Group>}
          <Button basic onClick={() => {console.log(this.state.axioms); this.toggle('addAxiomVisible')}}><Icon name='add' /> Add an axiom</Button>

          <ParseTermModal 
            visible={this.state.addAxiomVisible}
            macros={this.state.macros} 
            header="Add a new axiom"
            confirmButton="Add axiom"
            parser={(trm, success, error) => postParseDSeq(getPort(), {text: trm, opts: this.getAbbrevs()}, success, error)}
            onClose={() => this.toggle('addAxiomVisible')}
            onAdd={(data) => {addAxiom(data); this.toggle('addAxiomVisible')}}/>


          <Header style={{marginTop:'26px'}} textAlign='left' size='tiny'>Abbreviations</Header>

          {this.state.abbrevs.abbrevsFormula.length > 0 && <Segment.Group>
            {abbrevsF}
          </Segment.Group>}
          <Button basic onClick={() => {console.log(this.state.axioms); this.toggle('addAbbrevsFVisible')}}><Icon name='add' /> Add a formula abbreviation</Button>

          <ParseTermModal 
            visible={this.state.addAbbrevsFVisible}
            macros={this.state.macros} 
            header="Add a formula abbreviation"
            confirmButton="Add abbreviation"
            addingAbbrev={true}
            parser={(trm, typ, success, error) => postParseFormula(getPort(), {text: trm, opts: [typ, this.getAbbrevs()]}, success, error)}
            onClose={() => this.toggle('addAbbrevsFVisible')}
            onAdd={(varName, type, term) => {
              // console.log(varName, type, term);
              var array = this.state.abbrevs;
              array.abbrevsFormula.push([[varName, type], term]);
              this.setState({ abbrevs: array });
              this.toggle('addAbbrevsFVisible')}}/>
        </div>
      </Sidebar>
      <Sidebar.Pusher style={{minHeight: '100vh'}}>
        <div style={{paddingBottom: '30px'}}>
          <Button style={{float:'right', margin:'10px'}} basic circular icon='setting' onClick={() => this.toggle('sidebarVisible')} />
          
        </div>
        <div className="scroll">
          <div id="ProofTree" style={{zoom: this.state.currentZoom}}>
            <ProofTree macros={this.state.macros}
                       axioms={this.state.axioms}
                       sequent={this.state.ptSequent}
                       saveSequent={(s,r) => this.setState({ptSequent: s, rule:r})} 
                       rule=' '
                       abbrevs={this.getAbbrevs()}
                       ref={(node) => {this.pt = node}} />
          </div>
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
        <ParserBar macros={this.state.macros} callback={this.mkPTstub} abbrevs={this.getAbbrevs()}/>
      </div>)
  }
}