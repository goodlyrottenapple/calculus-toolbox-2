import React, {Component} from 'react'
import './ParserBar.css'
import { getPort , prettyErrorMsg } from './utils.js'
import { postParseDSeq } from './ServantApi.js'

import KaTeXRenderer from './KaTeXRenderer.js'
import { Input, Transition } from 'semantic-ui-react'

export default class ParserBar extends Component {
  static defaultProps = {
    macros: {},
    callback : (s) => { return null }
  }

  constructor(props) {
    super(props)
    this.state = {  
      sequent : {
        latex : ' ',
        term : {}
      },
      parseError : '',
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
      console.log(data)
      this.setState({ sequent: data , parseError: '' })
      if (didPressEnter) this.props.callback(data);
    }
    const error = (data) => {
      console.log(data)

      this.setState({ parseError: "error", parseErrorData: data })
    }
    if(e.target.value)
      postParseDSeq(getPort(), { text: e.target.value, opts: this.props.abbrevs }, success, error)
    else
      this.setState({ sequent: {latex: ' ', term : {}}, parseError: '' })
  }

  render() {
    return (
      <div id="BottomBar">
        <Transition.Group animation='fade up' duration={300}>
          {this.state.parseError === '' && <KaTeXRenderer id="Rendered" math={this.state.sequent.latex} macros={this.props.macros}/>}
          {this.state.parseError === 'error' && <div id="parseError">{prettyErrorMsg(this.state.parseErrorData)}</div>}
        </Transition.Group>
        <Input className={this.state.parseError} 
               onKeyUp={this.parseSequent} 
               onClick={this.caretPos} 
               onSelect={this.caretPos}
               placeholder='Type in a sequent and press Enter to create a new proof tree...'></Input>
        <div className='CaretPos'>({this.state.caretStart},{this.state.caretEnd})</div>
      </div>
    )
  }
}