import React, {Component} from 'react'
import './ParserBar.css'

import KaTeXRenderer from './KaTeXRenderer.js'
import { Input } from 'semantic-ui-react'

export default class ParserBar extends Component {
  static defaultProps = {
    macros: {},
    callback : (s) => { return null }
  }

  constructor(props) {
    super(props)
    this.state = {  
      sequent : {
        latex : '',
        term : {}
      },
      parseError : ''
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
    // console.log("sending: "+e.target.value)
    fetch("http://localhost:8081/parseDSeq?val="+e.target.value, {
      headers: {
        'Cache-Control': 'no-cache'
      }
    })
      // .then(handleErrors)
      .then(this.handleErrors)
      .then(data =>{
          // console.log(data)
          this.setState({ sequent: data })
          this.setState({ parseError: "" })
          if (didPressEnter) this.props.callback(data);
      })
      
      .catch(error => {
        error.data.then(data => console.log(data))
        this.setState({ parseError: "error" })
      });
  }

  render() {
    return (
      <div id="BottomBar">
        <KaTeXRenderer id="Rendered" math={this.state.sequent.latex} macros={this.props.macros}/>
        <Input className={this.state.parseError} onKeyUp={this.parseSequent}></Input>
      </div>
    )
  }
}