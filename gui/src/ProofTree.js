import React, {Component} from 'react'
import KaTeXRenderer from './KaTeXRenderer.js'
import './ProofTree.css'
import { postApplicableRules, postApplyCut, postLaunchPS, postCancelPS, postQueryPSResult, postParseFormula } from './ServantApi.js'
import { getPort } from './utils.js'
import ParseTermModal from './ParseTermModal.js'

import { Dropdown, Modal, Button, List, Icon, Loader, Label } from 'semantic-ui-react'

export default class ProofTree extends Component {
  static defaultProps = {
    sequent: {
      term : {},
      latex : ''
    },
    rule: ' ',
    multiplePremises: false,
    macros: {},
    axioms : [],
    children : []
  }

  constructor(props) {
    super(props)
    this.state = {
      //loaded: false,
      sequent: props.sequent,
      rule: props.rule,
      addingRules: false,
      psId: 0,
      doingPS: false,
      cutModalVisible: false,
      possibleRules: [],
      children: props.children
    }
    this.deleteAbove = this.deleteAbove.bind(this)  
    this.getPossibleRules = this.getPossibleRules.bind(this)  
    this.runPS = this.runPS.bind(this)  
    this.cancelPS = this.cancelPS.bind(this)  
    this.pollPS = this.pollPS.bind(this)  
    this.applyCut = this.applyCut.bind(this)  

    this.toggle = this.toggle.bind(this)
    this.toJSON = this.toJSON.bind(this)
    this.termLen = this.termLen.bind(this)

  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.sequent !== this.state.sequent)
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
      <ProofTree macros={this.props.macros} axioms={this.props.axioms} sequent={r} rule=' ' abbrevs={this.props.abbrevs}/>
    )
    this.setState({rule:r.rule, children:cs})
    if(this.state.addingRules) this.toggle('addingRules')
  }

  deleteAbove() {
    this.setState({rule:' ', children:[]})
  }

  getPossibleRules() {
    const success = (data) => {
      this.setState({possibleRules: data })
      // console.log(data)
      this.toggle('addingRules')
    }
    const error = (data) => console.log(data)
    postApplicableRules(getPort(), this.state.sequent.term, success, error)
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
    console.log("port: " + getPort())
    console.log("axioms: " + this.props.axioms)
    postLaunchPS(getPort(), [15,this.props.axioms.map((a) => a.term),this.state.sequent.term], success, error)
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
    postQueryPSResult(getPort(), this.state.psId, success, error)
  }

  applyCut(formula) {
    console.log(this.state.sequent)
    const success = (data) => {
      // this.setState({possibleRules: data })
      console.log(data)
      this.addAbove({rule:data[0][0],sequents:data[0][1]})
      // this.toggle('addingRules')
    }
    const error = (data) => console.log(data)
    postApplyCut(getPort(), [this.state.sequent.term , formula], success, error)
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
    return <ProofTree macros={this.props.macros} axioms={this.props.axioms} abbrevs={this.props.abbrevs}
                      sequent={concl} rule={r} children={cs}/>
  }

  fromJSONFileInput(pt) {
    const r = Object.keys(pt)[0];
    const concl = pt[r].conclusion;
    const cs = pt[r].premises.reverse().map((c) => this.fromJSONFileInput(c))
    return <ProofTree macros={this.props.macros} axioms={this.props.axioms} abbrevs={this.props.abbrevs}
                      sequent={concl} rule={r} children={cs}/>
  }

  cancelPS() {
    this.toggle('doingPS');
    const success = (data) => {
      console.log(data)
    }
    const error = (data) => console.log(data)
    postCancelPS(getPort(), this.state.psId, success, error)
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

  termLen(s) {

    if(s.DSeq) return this.termLen(s.DSeq.left) + this.termLen(s.DSeq.right)
    if(s.Con)  return 1 + s.Con.terms.map(this.termLen).reduce((a, b) => a + b, 0)
    if(s.Lift) return this.termLen(s.Lift)
    if(s.Base) return s.Base.length
    if(s.Abbrev) return s.Abbrev.name.length

    return -100000000000;
  }

  render() {
    if(this.state.sequent.latex === '') return null;

    const Concl = (
      <span className="concl-sequent">
        <KaTeXRenderer ref={(node) => {this.concl = node}} math={this.state.sequent.latex} macros={this.props.macros}/>
      </span>
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
          <Dropdown.Item onClick={() => this.toggle('cutModalVisible')}>
            Apply Cut
          </Dropdown.Item>
          <Dropdown.Item onClick={this.runPS}>
            Proof Search
          </Dropdown.Item>
        </Dropdown.Menu>
      </Dropdown>
    )

    const rules = this.state.possibleRules.map(([name,rules]) => 
      <List.Item>
        <List.Content>
          <Button className="ruleButton" as='div' labelPosition='right' onClick={() => this.addAbove({rule:name,sequents:rules})}>
            <Button color='blue' style={{minWidth: '100px'}}>
              <KaTeXRenderer math={name} macros={this.props.macros}/>
            </Button>
              {rules.map((rule, index) => 
              <Label as='a' basic color='blue' 
                style={{
                  width: '100%', 
                  borderTopRightRadius: rules.length === index+1 ? "4px" : 0,
                  borderBottomRightRadius: rules.length === index+1 ? "4px" : 0,
                  overflow: 'auto'
                }}>
                <KaTeXRenderer math={rule.latex} macros={this.props.macros}/>
              </Label>)}
            </Button>
          </List.Content>
      </List.Item>
    )

    const addAboveModal =(
      <Modal dimmer={'blurring'} open={this.state.addingRules}>
        <Modal.Header>Select a Rule To Apply to: 
          <span style={{fontSize: '14px'}}>
            <KaTeXRenderer math={this.state.sequent.latex} macros={this.props.macros}/>
          </span>
        </Modal.Header>
        <Modal.Content>
          <Modal.Description>
            <List style={{width:"100%"}}>
              {rules}
            </List>
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

    var dispCurrRuleDueToChildLength = false
    const mkChildren = () => {
      var ret = []

      for (var i = 0; i < this.state.children.length; i++){
        const parentL = this.termLen(this.state.sequent.term)
        const childL = this.termLen(this.state.children[i].props.sequent.term)
        // console.log(this.state.sequent)
        // console.log(this.termLen(this.state.sequent.term))
        console.log("rule: "+this.state.rule)
        console.log(this.termLen(this.state.sequent.term))
        // console.log("parent: "+this.state.sequent.latex.length)
        // console.log("child: "+this.state.children[i].props.sequent.latex.length)
        // console.log("--------------")
        if ((this.state.children.length === 1 && childL >= parentL)
         || (this.state.children.length > 1 && i === this.state.children.length-1))
          ret.push(React.cloneElement(this.state.children[i], {
            ref: 'child-' + i,
            prevRule: this.state.rule
          }));

        else if (this.state.children.length > 1 && i < this.state.children.length-1) {
          ret.push(React.cloneElement(this.state.children[i], {
            ref: 'child-' + i,
            multiplePremises: true
          }));
          ret.push(<div className="inter-proof"></div>)
        }
        else {
          ret.push(React.cloneElement(this.state.children[i], {
            ref: 'child-' + i
          }));
          dispCurrRuleDueToChildLength = true
        }


      }
      return ret;
    }

    const PrevRule = () => {
      if(this.props.prevRule) return (
        <span className="rule-tag">
          <KaTeXRenderer math={this.props.prevRule} macros={this.props.macros}/>
        </span>
      )
      else return (<span></span>)
    }
   
    // this code has been copied and adapted from: https://stackoverflow.com/questions/16212364/how-do-i-display-a-proof-tree-with-html-css-and-or-javascript
    return (
      <div className="proof">
        {this.state.children.length > 0 && 
        <div className="prems">
         {mkChildren()}
        </div>}
        <div className="concl">
          <div className="concl-left"></div>
          <div className="concl-center">
            {Menu}
            <div className={`${this.state.children.length === 0 && this.props.multiplePremises ? "rule-tags-last" : "rule-tags"} ${this.state.children.length === 0 && this.props.multiplePremises && this.state.rule === ' ' ? "fix-rule-tags" : ""}`}>
              {PrevRule()}
              {(this.state.children.length === 0 || (this.state.children.length === 1 && dispCurrRuleDueToChildLength)) && 
              [<span className="rule-tag" key="0">
                <KaTeXRenderer math={this.state.rule} macros={this.props.macros}/>
              </span>,
              <span className="hidden-tag" key="1">
                <KaTeXRenderer math={this.state.rule} macros={this.props.macros}/>
              </span>]}
              {this.props.prevRule && 
              <span className="hidden-tag">
                <KaTeXRenderer math={this.props.prevRule} macros={this.props.macros}/>
              </span>}
            </div>

            {addAboveModal}
            {psModal}
            <ParseTermModal 
              visible={this.state.cutModalVisible}
              macros={this.props.macros}
              confirmButton="Apply Cut"
              header={`Please supply a cut formula of type '${this.state.sequent.term.DSeq.type}'`}
              parser={(trm, success, error) => postParseFormula(getPort(), {text: trm, opts:[this.state.sequent.term.DSeq.type, this.props.abbrevs]}, success, error)}
              onClose={() => this.toggle('cutModalVisible')}
              onAdd={(data) => {this.applyCut(data.term); this.toggle('cutModalVisible')}}/>

          </div>
          <div className="concl-right"></div>
        </div>
      </div>
    )
  }
}