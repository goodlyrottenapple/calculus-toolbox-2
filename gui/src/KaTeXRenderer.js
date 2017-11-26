import React, {Component} from 'react'
import KaTeX from 'katex'


export default class KaTeXRenderer extends Component {

  static defaultProps = {
    math: '',
    macros : {}
  }

  constructor(props) {
    super(props)
    this.state = {
      //loaded: false,
      oldMath: props.math,
      macros: props.macros
    }
  }


  renderToString() {
    const oldHTML = this.preview.innerHTML
    try {
      this.preview.innerHTML = KaTeX.renderToString(this.props.math, {
        macros:this.state.macros
      });
    }
    catch (e) {
      this.preview.innerHTML = `<span style="color:red">${oldHTML}</span>`
    }
  }

  componentDidMount() {
    this.renderToString()
  }

  shouldComponentUpdate(nextProps, nextState) {
    if (!nextProps.math) return false
    return nextProps.math !== this.state.oldMath
  }

  componentDidUpdate(prevProps, prevState) {
    this.renderToString()
  }

  componentWillReceiveProps(nextProps) {
    this.setState({oldMath: nextProps.math, macros: nextProps.macros})
  }

  render() {
    return (
      <div id={this.props.id} ref={(node) => {this.preview = node}}></div>
    )
  }
}