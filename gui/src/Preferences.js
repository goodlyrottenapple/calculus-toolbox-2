import React, {Component} from 'react'
import { Form } from 'semantic-ui-react'


export default class Preferences extends Component {

  constructor(props) {
    super(props)
    if(props.opts === 'initial'){
      this.state = {
        workDir: ''
      }
    }
    console.log(this.props.initialSetup)
    this.selectWorkspaceDir = this.selectWorkspaceDir.bind(this)
    this.handleChange = this.handleChange.bind(this)
    this.save = this.save.bind(this)
  }

  selectWorkspaceDir() {
    const {dialog} = window.require('electron').remote;
    dialog.showOpenDialog(
      window.require('electron').remote.getCurrentWindow(),
      {
        properties: ['openDirectory','createDirectory']
      },
      (folderName) => {
        if (folderName === undefined){
            console.log("You didn't select a folder");
            return;
        }
        console.log(folderName)
        this.setState({workDir:folderName})
      }
    );
  }

  save(){
    const Store = window.require('electron-store');
    const store = new Store();
    if(this.state.workDir === '') alert("Please selecet a non-empty working directory path")
    else {
      store.set('workDir', this.state.workDir);
      // var window = remote.getCurrentWindow();
      window.close();
    }
  }

  handleChange = (e, { name, value }) => this.setState({ [name]: value })

  render() {
    const initial = () => 
      <div style={{padding:'20px'}}>
      <h2>Inital setup</h2>
      <Form>
        <Form.Field>
          <label>Select a workspace folder</label>
          <Form.Input value={this.state.workDir} action={{ content: 'Select', onClick: this.selectWorkspaceDir }} name="workspace_dir"/>
        </Form.Field>

        
        <Form.Button positive onClick={this.save} labelPosition='right' icon='save' content='Save' />
      </Form>
      </div>

    return initial()
  }
}