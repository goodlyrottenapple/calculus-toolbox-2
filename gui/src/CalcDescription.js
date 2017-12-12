import React, {Component} from 'react'
import './CalcDescription.css'
import { getCalcDesc , postCalcDesc } from './ServantApi.js'

import CodeMirror from 'react-codemirror'
import '../node_modules/codemirror/lib/codemirror.css'
import { Form, Modal, Button } from 'semantic-ui-react'

export default class CalcDescription extends Component {

  constructor() {
    super()
    this.state = {
      name : '',
      calc : '',
      rules : ''
    }
    this.updateCalc = this.updateCalc.bind(this)
    this.updateRules = this.updateRules.bind(this)
    this.handleChange = this.handleChange.bind(this)
    this.saveCalc = this.saveCalc.bind(this)
    this.getCalc()
  }

  componentWillReceiveProps(nextProps) {
    //if (nextProps.open) {
      
    //}
    this.getCalc()
  }

  getCalc() {
    const success = (data) => this.setState({ name: data.name, calc: data.rawCalc, rules: data.rawRules })
    const error = (data) => console.log(data)
    getCalcDesc('no-cache', success, error)
  }

  handleChange = (e, { name, value }) => this.setState({ [name]: value })

  updateCalc (newCode) {
    this.setState({ calc: newCode });
  }

  updateRules (newCode) {
    this.setState({ rules: newCode });
  }

  saveCalc () {
    const success = (data) => {
      //this.props.onSave()
      //this.props.onClose()
      const electron = window.require('electron');
      const ipcRenderer = electron.ipcRenderer;

      ipcRenderer.send('calcUpdate', `This message goes back to the main window.`);
      console.log("this is called")
    }
    const error = (data) => console.log(data)
    const body = { name: this.state.name, rawCalc: this.state.calc, rawRules: this.state.rules }

    postCalcDesc(body, 'private', success, error)
  }
  render() {
    const options = {
      lineNumbers: true,
      lineWrapping: true
    };
    return (
      <div style={{padding:'20px'}}>
      <Form>
        <Form.Field>
          <label>Calculus Name</label>
          <Form.Input name="name" value={this.state.name} onChange={this.handleChange} />
        </Form.Field>

        <Form.Field>
          <label>Calculus Definition</label>
          <CodeMirror className="codeArea" value={this.state.calc} onChange={this.updateCalc} options={options}/>
        </Form.Field>

        <Form.Field>
          <label>Rules</label>
          <CodeMirror className="codeArea" value={this.state.rules} onChange={this.updateRules} options={options}/>
        </Form.Field>

        <Button positive onClick={this.saveCalc} labelPosition='right' icon='save' content='Save changes' />
      </Form>
      </div>
    )
  }


  // </Modal.Description>
  //       </Modal.Content>
  //       <Modal.Actions>
  //         <Button negative onClick={this.props.onClose}>Cancel</Button>
  //         <Button positive onClick={this.saveCalc} labelPosition='right' icon='save' content='Save' />
  //       </Modal.Actions>
  //     </Modal>
}