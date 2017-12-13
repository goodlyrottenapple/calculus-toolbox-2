import React, {Component} from 'react'
import './DocName.css';

import { Input } from 'semantic-ui-react'

export default class DocName extends Component {
  static defaultProps = {
  	name: 'Untitled',
  	onEdit : () => { return; }
  }

  constructor(props) {
  	super()
  	this.state = {
  		name: props.name,
  		editing:false
  	}
  	this.handleChange = this.handleChange.bind(this)
  	this.toggle = this.toggle.bind(this)
  }

  toggle() { this.setState({editing: !this.state.editing}) }

  handleChange(e) {
    this.setState({name: e.target.value})
    this.props.onEdit(e.target.value)
  }

  render() {
  	var component;
  	if(this.state.editing) component = 
      <div className='docInput ui input' style={this.props.style} >
  		  <input 
               value={this.state.name} 
  			       onChange={this.handleChange}
  			       onKeyUp={(e) => {if (e.key === 'Enter') this.toggle()}}
  			       ref="inputDocName"
  			       onBlur={this.toggle}/>
      </div>
  	else component = <h4 className='docInput' onClick={this.toggle} style={this.props.style}>{this.state.name}</h4>
  	return component
  }
}