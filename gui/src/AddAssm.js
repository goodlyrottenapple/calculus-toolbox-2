import React, {Component} from 'react'
import './AddAssm.css'

import { getPort } from './utils.js'
import { getParseDSeq } from './ServantApi.js'

import { Modal, Button, Input } from 'semantic-ui-react'
import KaTeXRenderer from './KaTeXRenderer.js'

export default class AddAssm extends Component {
  
  constructor(props) {
    super(props)
    this.state = {  
      sequent : {
        latex : ' ',
        term : {}
      },
      addEnabled: false
    }
    this.parseSequent = this.parseSequent.bind(this)  
  }

  handleErrors(response) {
    if (response.ok) {
      return response.json();
    }
    var error = new Error()
    error.data = response.json();
    throw error;
  }

  parseSequent(e) {
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
      this.setState({ sequent: {latex: ' ', term : {}}, addEnabled: false, parseError: 'error' })
    }
    getParseDSeq(getPort(), e.target.value, success, error)
  }
  render() {
    const isDisabled = this.state.addEnabled ? '' : 'disabled'
    return (
      <Modal dimmer={'blurring'} open={this.props.visible}>
        <Modal.Header>Add a new assumption</Modal.Header>
        <Modal.Content>
          <Modal.Description>
            <div id="AddAssmInput">
              <KaTeXRenderer id="AssmRendered" math={this.state.sequent.latex} macros={this.props.macros}/>
              <Input className={this.state.parseError} onKeyUp={this.parseSequent}/>
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