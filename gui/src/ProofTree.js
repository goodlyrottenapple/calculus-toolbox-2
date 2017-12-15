import React, {Component} from 'react'
import KaTeXRenderer from './KaTeXRenderer.js'
import './ProofTree.css'
import { postApplicableRules, postLaunchPS, postCancelPS, postQueryPSResult } from './ServantApi.js'

import { Dropdown, Modal, Button, List, Icon, Loader } from 'semantic-ui-react'

export default class ProofTree extends Component {
  static defaultProps = {
    sequent: {
      term : {},
      latex : ''
    },
    rule: '',
    macros: {},
    children : []
  }

  constructor(props) {
    super(props)
    this.state = {
      //loaded: false,
      sequent: props.sequent,
      rule: props.rule,
      // macros: props.macros,
      addingRules: false,
      psId: 0,
      doingPS: false,
      possibleRules: [],
      children: props.children
    }
    this.deleteAbove = this.deleteAbove.bind(this)  
    this.getPossibleRules = this.getPossibleRules.bind(this)  
    this.runPS = this.runPS.bind(this)  
    this.cancelPS = this.cancelPS.bind(this)  
    this.pollPS = this.pollPS.bind(this)  

    this.toggle = this.toggle.bind(this)
    this.toJSON = this.toJSON.bind(this)  
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.sequent !== this.props.sequent)
      this.setState({
        sequent: nextProps.sequent, 
        rule: nextProps.rule,
        possibleRules: [],
        children: nextProps.children
      })
  }


  toggle(x) {
    this.setState({[x] : !this.state[x]})
  }

  addAbove(r) {
    // console.log(r)
    const cs = r.sequents.map((r) => 
      <ProofTree macros={this.props.macros} sequent={r} rule=""/>
    )
    this.setState({rule:r.rule, children:cs})
    this.toggle('addingRules')
  }

  deleteAbove() {
    this.setState({rule:'', children:[]})
  }

  getPossibleRules() {
    const success = (data) => {
      this.setState({possibleRules: data })
      this.toggle('addingRules')
    }
    const error = (data) => console.log(data)
    postApplicableRules(this.props.sequent.term, 'private', success, error)
  }

  runPS() {
    this.toggle('doingPS')
    const success = (data) => {
      console.log(data)
      this.setState({psId: data})
      this.pollPS()
    }
    const error = (data) => console.log(data)
    // 15 is the proof depth parameter
    postLaunchPS([15,[],this.props.sequent.term], 'private', success, error)
  }

  pollPS() {
    const success = (data) => {
      if(data.length === 0) {
        // console.log("got nothing")
        if(this.state.doingPS) setTimeout(this.pollPS, 500);
      }
      else if(data.length === 1){
        // console.log(data);
        this.toggle('doingPS');
        this.deleteAbove();
        this.appendPT(data[0]);
      } else {
        console.log("error!"+ data)
      }
    }
    const error = (data) => console.log(data)
    postQueryPSResult(this.state.psId, 'private', success, error)
  }

  appendPT(pt) {
    const r = Object.keys(pt)[0];
    const cs = pt[r].premises.map((p) => this.fromJSON(p))
    this.setState({rule: r, children: cs})
  }

  fromJSON(pt) {
    const r = Object.keys(pt)[0];
    const concl = pt[r].conclusion;
    const cs = pt[r].premises.map((c) => this.fromJSON(c))
    return <ProofTree macros={this.props.macros} sequent={concl} rule={r} children={cs}/>
  }

  cancelPS() {
    this.toggle('doingPS');
    const success = (data) => {
      console.log(data)
    }
    const error = (data) => console.log(data)
    postCancelPS(this.state.psId, 'private', success, error)
  }

  toJSON() {
    const r = this.state.rule
    const concl = this.state.sequent
    const keys = Object.keys(this.refs)
    var ps = []
    for (var i = keys.length - 1; i >= 0; i--) {
      const p = this.refs[keys[i]].toJSON()
      ps.push(p)
    }
    return { [r] : { conclusion: concl, premises: ps} }
  }

  render() {
    if(this.props.sequent.latex === '') return null;
    // console.log(this.props.children)
    var Pts;
    const span = this.state.children.length;

    const chMap = () => {
      var ret = []

      for (var i = 0; i < this.state.children.length; i++){
        const child = React.cloneElement(this.state.children[i], {
            ref: 'child-' + i
        });
        if(i<this.state.children.length -1) ret.push(<td className="hasRight" valign="bottom">{child}</td>)
        else ret.push(<td valign="bottom">{child}</td>)
      }
      return ret;
    }

    if(span > 0) {
      Pts = chMap() //this.state.children.map((c) => {return <td valign="bottom">{c}</td>})  
    } else {
      Pts = <td></td>
    }


    const Concl = (
      <KaTeXRenderer ref={(node) => {this.concl = node}} math={this.props.sequent.latex} macros={this.props.macros}/>
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
          <Dropdown.Item onClick={this.runPS}>
            Proof Search
          </Dropdown.Item>
          <Dropdown.Item onClick={() => console.log(this.toJSON())}>
            Save
          </Dropdown.Item>
        </Dropdown.Menu>
      </Dropdown>
    )

    const rules = this.state.possibleRules.map(([name,rules]) => 
      <Button basic color="blue" style={{marginBottom: '-1px'}} onClick={() => this.addAbove({rule:name,sequents:rules})}>
        <List horizontal>
          <List.Item>
            <List.Content>{name}</List.Content>
          </List.Item>
          {rules.map((rule) => 
            <List.Item key={rule.latex}>
              <List.Content>
                <KaTeXRenderer math={rule.latex} macros={this.props.macros}/>
              </List.Content>
            </List.Item>)}
        </List>
      </Button>
    )

    const addAboveModal =(
      <Modal dimmer={'blurring'} open={this.state.addingRules}>
        <Modal.Header>Select a Rule To Apply</Modal.Header>
        <Modal.Content>
          <Modal.Description>
            <Button.Group vertical style={{width:"100%"}}>
              {rules}
            </Button.Group>
          </Modal.Description>
        </Modal.Content>
        <Modal.Actions>
          <Button negative onClick={() => this.toggle('addingRules')}>Cancel</Button>
        </Modal.Actions>
      </Modal>
    )

    const psModal = (
      <Modal dimmer={'blurring'} open={this.state.doingPS} basic size='small'>
        <Modal.Content>
          <Loader size='huge'>Searching for a proof</Loader>
        </Modal.Content>
        <div style={{margin: 'auto'}}>
          <Button basic color='red' inverted onClick={this.cancelPS}>
            <Icon name='remove' /> Cancel Search
          </Button>
        </div>
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
              {psModal}
            </td>
          </tr>
        </tbody>
      </table>
    )
  }
}