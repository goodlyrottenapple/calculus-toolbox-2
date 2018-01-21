import React, {Component} from 'react'
import './AddAssm.css'

import { getPort, prettyErrorMsg } from './utils.js'
import { getParseDSeq } from './ServantApi.js'

import { Modal, Button, Input, Transition } from 'semantic-ui-react'
import KaTeXRenderer from './KaTeXRenderer.js'

export default class AddAssm extends Component {
  
  constructor(props) {
    super(props)
    this.state = {  
      sequent : {
        latex : ' ',
        term : {}
      },
      addEnabled: false,
      parseErrorData : {tag: "DefaultError"},
      caretStart: 0,
      caretEnd: 0
    }
    this.parseSequent = this.parseSequent.bind(this)  
    this.caretPos = this.caretPos.bind(this)
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
        this.props.onAdd(data);
        this.setState({ sequent: {latex: ' ', term : {}}, addEnabled: false, parseError: '' })
      }
      else this.setState({ sequent: data , addEnabled: true, parseError: '' })
    }
    const error = (data) => {
      console.log(data)
      this.setState({ addEnabled: false, parseError: 'error', parseErrorData: data })
    }

    if(e.target.value)
      getParseDSeq(getPort(), e.target.value, success, error)
    else
      this.setState({ sequent: {latex: ' ', term : {}}, addEnabled: false, parseError: '' })
  }
  render() {
    const isDisabled = this.state.addEnabled ? '' : 'disabled'
    return (
      <Modal dimmer={'blurring'} open={this.props.visible}>
        <Modal.Header>Add a new assumption</Modal.Header>
        <Modal.Content>
          <Modal.Description>
            <div id="AddAssmInput">
              <Transition.Group animation='fade up' duration={200}>
                {this.state.parseError === '' && <KaTeXRenderer id="AssmRendered" math={this.state.sequent.latex} macros={this.props.macros}/>}
                {this.state.parseError === 'error' && <div id="assmParseError">{prettyErrorMsg(this.state.parseErrorData)}</div>}
              </Transition.Group>
              <Input className={this.state.parseError} onKeyUp={this.parseSequent} onClick={this.caretPos} onSelect={this.caretPos}/>
              <div className='CaretPos'>({this.state.caretStart},{this.state.caretEnd})</div>
            </div>
          </Modal.Description>
        </Modal.Content>
        <Modal.Actions>
          <Button className={isDisabled} positive 
            onClick={() => {
              this.props.onAdd(this.state.sequent);
              this.setState({ sequent: {latex: ' ', term : {}}, addEnabled: false, parseError: '' })
            }}>Add</Button>
          <Button negative 
            onClick={() => {
              this.props.onClose();
              this.setState({ sequent: {latex: ' ', term : {}}, addEnabled: false, parseError: '' })
            }}>Cancel</Button>
        </Modal.Actions>
      </Modal>
    )
  }
}