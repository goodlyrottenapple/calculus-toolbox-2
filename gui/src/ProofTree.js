import React, {Component} from 'react'
import KaTeXRenderer from './KaTeXRenderer.js'
import './ProofTree.css'

import { Dropdown, Modal, Header, Button, List } from 'semantic-ui-react'

export default class ProofTree extends Component {
  static defaultProps = {
    sequent: {
      term : {},
      latex : ''
    },
    rule: '',
    macros: {}
  }

  constructor(props) {
    super(props)
    this.state = {
      //loaded: false,
      sequent: props.sequent,
      rule: props.rule,
      // macros: props.macros,
      addingRules: false,
      possibleRules: [],
      children: []
    }
    this.deleteAbove = this.deleteAbove.bind(this)  
    this.getPossibleRules = this.getPossibleRules.bind(this)  
    this.toggleAddingRules = this.toggleAddingRules.bind(this)  
  }


  componentWillReceiveProps(nextProps) {
    this.setState({
      sequent: nextProps.sequent, 
      rule: nextProps.rule,
      possibleRules: [],
      children: []
    })
  }


  toggleAddingRules() {
    this.setState({addingRules: !this.state.addingRules })
  }

  handleErrors(response) {
    if (response.ok) {
      return response.json();
    }
    var error = new Error()
    error.data = response.json();
    throw error;
  }

  addAbove(r) {
    // console.log(r)
    const cs = r.sequents.map((r) => 
      <ProofTree macros={this.props.macros} sequent={r} rule=""/>
    )
    this.setState({rule:r.rule, children:cs})
    this.toggleAddingRules()
  }

  deleteAbove() {
    this.setState({rule:'', children:[]})
  }

  getPossibleRules() {
    console.log(JSON.stringify(this.props.sequent.term))
    fetch("http://localhost:8081/getApplicableRules", {
      method: 'post',
      body: JSON.stringify(this.props.sequent.term),
      headers: {"Content-Type": "application/json"}
    })
      .then(this.handleErrors)
      .then(data =>{
          console.log(data)
          this.setState({possibleRules: data })
          this.toggleAddingRules()
      })
      
      .catch(error => 
        error.data.then(data => console.log(data))
      );
  }



  render() {
    if(this.props.sequent.latex === '') return null;
    // console.log(this.props.children)
    var Pts;
    const span = this.state.children.length;

    const chMap = () => {
      var ret = []
      for (var i = 0; i < this.state.children.length; i++){
        if(i<this.state.children.length -1) ret.push(<td className="hasRight" valign="bottom">{this.state.children[i]}</td>)
        else ret.push(<td valign="bottom">{this.state.children[i]}</td>)
      }
      return ret;
    }

    if(span > 0) {
      Pts = chMap() //this.state.children.map((c) => {return <td valign="bottom">{c}</td>})  
    } else {
      Pts = <td></td>
    }


    const Concl = (
      <KaTeXRenderer math={this.props.sequent.latex} macros={this.props.macros}/>
    )

    const Menu = (
      <Dropdown 
        trigger={Concl}
        pointing='top left' icon={null}>
        <Dropdown.Menu>
          <Dropdown.Item onClick={this.getPossibleRules}
            text='Add above' description='a'/>
          <Dropdown.Item onClick={this.deleteAbove}>
            Delete above
          </Dropdown.Item>
          <Dropdown.Item>
            Proof Search
          </Dropdown.Item>
        </Dropdown.Menu>
      </Dropdown>
    )

    const rules = this.state.possibleRules.map(([name,rules]) => 
      <Button basic color="blue" onClick={() => this.addAbove({rule:name,sequents:rules})}>
        <List horizontal>
          <List.Item>
            <List.Content>{name}</List.Content>
          </List.Item>
          {rules.map((rule) => 
            <List.Item>
              <List.Content>
                <KaTeXRenderer math={rule.latex} macros={this.props.macros}/>
              </List.Content>
            </List.Item>)}
        </List>
      </Button>)

    const addAboveModal =(
      <Modal dimmer={'blurring'} open={this.state.addingRules}>
        <Modal.Header>Select a Rule To Apply</Modal.Header>
        <Modal.Content image>
          <Modal.Description>
            <Button.Group vertical style={{width:"100%"}}>
              {rules}
            </Button.Group>
          </Modal.Description>
        </Modal.Content>
        <Modal.Actions>
          <Button negative onClick={this.toggleAddingRules}>Cancel</Button>
        </Modal.Actions>
      </Modal>
    )
    

   

    return (
      <table>
        <tbody>
          <tr>
            {Pts}
            
          </tr>
          <tr>
            <td colSpan={span} align="center">

                <div className="ruleBar">
                {Menu}
                <span className="ruleTag">{this.state.rule}</span>
                </div>

              {addAboveModal}
            </td>
            
          </tr>
        </tbody>
      </table>
    )
  }
}



 // <td rowSpan={2} valign="bottom">
 //              <span className="ruleTag">
 //                {this.state.rule}
 //              </span>
 //            </td>