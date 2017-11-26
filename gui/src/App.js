import React, { Component } from 'react';
import './App.css';
import CalcDescription from './CalcDescription.js'
import ProofTree from './ProofTree.js'
import ParserBar from './ParserBar.js'

import { Button, Dropdown, Tab, Menu } from 'semantic-ui-react'

class App extends Component {
  constructor() {
    super()
    this.state = {
      addingCalcDesc: false,
      macros: {},
      ptSequent: {
        latex : '',
        term: {}
      }
    }
    this.toggleCalcDesc = this.toggleCalcDesc.bind(this)  
    this.mkPT = this.mkPT.bind(this)
  }

  componentDidMount() {
    this.getMacros()
  }

  handleErrors(response) {
    if (response.ok) {
      return response.json();
    }
    var error = new Error()
    error.data = response.json();
    throw error;
  }

  getMacros() {
    fetch("http://localhost:8081/getMacros")
    .then(this.handleErrors)
    .then(data => {
      console.log(data)
      this.setState({macros: data})
    })
    .catch(error => error.data.then(data => console.log(data)) );
  }

  toggleCalcDesc() {
    this.setState({addingCalcDesc: !this.state.addingCalcDesc })
  }

  mkPT(s) {
    this.setState({ptSequent: s})
    console.log(s)
  }


  render() {
    // const html = Katex.renderToString(this.state.sequent, {macros:this.state.kmacros});
    // let calcDescription;
    // if(this.state.addingCalcDesc) calcDescription =

    const mItem = <Menu.Item>a |- a /\ b<span style={{'padding-left':'50px'}}/>
      <Button basic icon="close" style={{'box-shadow':'none', 'font-size': '0.8em' }}/></Menu.Item>
    const panes = [
  { menuItem: mItem, render: () => <Tab.Pane>Tab 1 Content</Tab.Pane> },
  { menuItem: mItem, render: () => <Tab.Pane>Tab 2 Content</Tab.Pane> },
  { menuItem: mItem, render: () => <Tab.Pane>Tab 3 Content</Tab.Pane> },
]

const TabExampleBasic = () => (
  <Tab panes={panes} />
)

    const MainMenu = (
      <Dropdown id="mainMenu" text='Menu' floating>
        <Dropdown.Menu>
          <Dropdown.Item>Load Calculus</Dropdown.Item>
          <Dropdown.Item onClick={this.toggleCalcDesc}>Modify Calculus</Dropdown.Item>
        </Dropdown.Menu>
      </Dropdown>
    )

    return (
      <div className="App">
        <div>
        {MainMenu}
        {TabExampleBasic()}
        </div>
        <CalcDescription 
          open={this.state.addingCalcDesc} 
          onClose={this.toggleCalcDesc} 
          callback={(data) => console.log(data)}/>
     

        <div id="ProofTree">
          <ProofTree macros={this.state.macros} sequent={this.state.ptSequent} rule=""/>
        </div>
        <ParserBar macros={this.state.macros} callback={this.mkPT}/>
      </div>
    );
  }
}

export default App;
