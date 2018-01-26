import React, {Component} from 'react'
import './ParseTermModal.css'
import { getGetTypes } from './ServantApi.js'

import { prettyErrorMsg, getPort } from './utils.js'
import { Modal, Button, Input, Transition, Dropdown } from 'semantic-ui-react'
import KaTeXRenderer from './KaTeXRenderer.js'

export default class ParseTermModal extends Component {
  
  constructor(props) {
    super(props)
    this.state = {  
      sequent : {
        latex : ' ',
        term : {}
      },
      type : "",
      types : [],
      abbrevName : "",
      addEnabled: false,
      parseErrorData : {tag: "DefaultError"},
      caretStart: 0,
      caretEnd: 0
    }
    this.parseSequent = this.parseSequent.bind(this)  
    this.caretPos = this.caretPos.bind(this)
    // this.loadTypes = this.loadTypes.bind(this)
    this.loadTypes()
  }


  loadTypes() {
    const success = (data) => {
      // console.log(data)
      this.setState({ types: data })
    }
    const failure = (data) => console.log(data)
    getGetTypes(getPort(), success, failure)
  }

  handleErrors(response) {
    if (response.ok) {
      return response.json();
    }
    var error = new Error()
    error.data = response.json();
    throw error;
  }

  caretPos(e) {
    this.setState({caretStart: e.target.selectionStart, caretEnd: e.target.selectionEnd})
  }

  parseSequent(e) {
    this.setState({caretStart: e.target.selectionStart, caretEnd: e.target.selectionEnd})

    const didPressEnter = (e.key === 'Enter') ? true : false
    const success = (data) => {
      // console.log(data)
      if (didPressEnter) {
        if(this.props.addingAbbrev) this.props.onAdd(this.state.abbrevName, this.state.type, data)
        else this.props.onAdd(data)

        this.setState({ sequent: {latex: ' ', term : {}}, addEnabled: false, parseError: '', type: '', abbrevName: '' })
      }
      else this.setState({ sequent: data , addEnabled: true, parseError: '' })
    }
    const error = (data) => {
      console.log(data)
      this.setState({ addEnabled: false, parseError: 'error', parseErrorData: data })
    }

    if(e.target.value){
      if(this.props.addingAbbrev) this.props.parser(e.target.value, this.state.type, success, error)
      else this.props.parser(e.target.value, success, error)
    }
    else
      this.setState({ sequent: {latex: ' ', term : {}}, addEnabled: false, parseError: '' })
  }
  render() {
    const addDisabled = this.state.addEnabled ? '' : 'disabled'
    const inputDisabled = this.props.addingAbbrev && (this.state.type === "" || this.state.abbrevName === "")  ? 'disabled' : '' 

    const typeOptions = this.state.types.map((t) => {return {key: t, value: t, text: t}})
    return (
      <Modal dimmer={'blurring'} open={this.props.visible}>
        <Modal.Header>{this.props.header}</Modal.Header>
        <Modal.Content>
          <Modal.Description>
            {this.props.addingAbbrev &&
            <div id="AddAssmInput" style={{paddingBottom : '20px'}}>
              <Dropdown style={{marginBottom : '20px'}} placeholder='Select the type of the term to be parsed' fluid search selection options={typeOptions} 
                onChange={(e, { value }) => this.setState({type: value})} />
              <Input placeholder="Enter the name for the abbreviation" onChange={(e, { value }) => this.setState({abbrevName: value})}/>
            </div>}

            <div id="AddAssmInput">
              <Transition.Group animation='fade up' duration={200}>
                {this.state.parseError === '' && <KaTeXRenderer id="AssmRendered" math={this.state.sequent.latex} macros={this.props.macros}/>}
                {this.state.parseError === 'error' && <div id="assmParseError">{prettyErrorMsg(this.state.parseErrorData)}</div>}
              </Transition.Group>
              <Input className={`parseBar ${this.state.parseError} ${inputDisabled}`} onKeyUp={this.parseSequent} onClick={this.caretPos} onSelect={this.caretPos}/>
              <div className='CaretPos'>({this.state.caretStart},{this.state.caretEnd})</div>
            </div>
          </Modal.Description>
        </Modal.Content>
        <Modal.Actions>
          <Button className={addDisabled} positive 
            onClick={() => {
              if(this.props.addingAbbrev) this.props.onAdd(this.state.abbrevName, this.state.type, this.state.sequent)
              else this.props.onAdd(this.state.sequent)
              this.setState({ sequent: {latex: ' ', term : {}}, addEnabled: false, parseError: '' })
            }}>{this.props.confirmButton}</Button>
          <Button negative 
            onClick={() => {
              this.props.onClose();
              this.setState({ sequent: {latex: ' ', term : {}}, addEnabled: false, parseError: '', type: '', abbrevName: '' })
            }}>Cancel</Button>
        </Modal.Actions>
      </Modal>
    )
  }
}