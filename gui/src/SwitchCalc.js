import React, {Component} from 'react'
import { getListCalculi , postLoadCalc, getCalcDesc } from './ServantApi.js'
import { getPort } from './utils.js'

import { Modal, Button } from 'semantic-ui-react'

export default class SwitchCalc extends Component {
  
  constructor(props) {
    super(props)
    this.state = {
      currentName:'',
      calculi: []
    }
  }

  componentWillReceiveProps(nextProps) {
    if(nextProps.visible)
      this.loadCalcs()
      this.getCalcName()
  }

  loadCalcs() {
    // console.log("getting called")
    const success = (data) => this.setState({calculi: data})
    const failure = (data) => console.log(data)
    getListCalculi(getPort(), success, failure)
  }

  getCalcName() {
    const success = (data) => {
      // console.log(data)
      this.setState({ currentName: data.name })
    }
    const error = (data) => console.log(data)
    getCalcDesc(getPort(), success, error)
  }

  switchCalc(name) {
    console.log(name)
    const success = (data) => {
      const Store = window.require('electron-store');
      const store = new Store();
      store.set('currentCalc', name)
      this.props.onSwitch()
      alert(`'${name}' was successfully loaded`)
    }
    const failure = (data) => console.log(data)
    postLoadCalc(getPort(), name, success, failure)
  }


  render() {
    const rules = this.state.calculi.map((name) => 
      <Button basic color="blue" key={name} style={{marginBottom: '-1px'}} onClick={() => this.switchCalc(name)}>
        {name}
      </Button>
    )


    return (
      <Modal dimmer={'blurring'} open={this.props.visible}>
        <Modal.Header>Currently loaded: {this.state.currentName}<br/>Select a calculus</Modal.Header>
        <Modal.Content>
          <Modal.Description>
            <Button.Group vertical style={{width:"100%"}}>
              {rules}
            </Button.Group>
          </Modal.Description>
        </Modal.Content>
        <Modal.Actions>
          <Button negative onClick={() => this.props.onClose()}>Cancel</Button>
        </Modal.Actions>
      </Modal>
    )
  }
}