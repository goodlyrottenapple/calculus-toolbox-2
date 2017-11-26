import React, {Component} from 'react'
// import PropTypes from 'prop-types'
import loadScript from 'load-script'

const SCRIPT = 'https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_CHTML'

export default class MathJaxRenderer extends Component {
  // static propTypes = {
  //   className: PropTypes.string,
  //   style: PropTypes.string,
  //   math: PropTypes.string,
  // }

  static defaultProps = {
    math: '',
    macros : {}
  }

  constructor(props) {
    super(props)
    this.state = {
      loaded: false,
      oldMath: props.math,
      macros: props.macros
    }
  }

  onLoad = (err) => {
    this.setState({
        loaded: true
    })
    if (err)
      console.log(err)
    else {
      window.MathJax.Hub.Config({
        showMathMenu: false,
        tex2jax: { inlineMath: [['$','$'],['\\(','\\)']] },
        TeX: {
          Macros: this.state.macros 
        }
      })
      window.MathJax.Hub.Queue(['Typeset', window.MathJax.Hub, this.preview])
    }
  }

  componentDidMount() {
    // this.preview.style.visibility = "hidden";
    this.preview.innerHTML = '$$' + this.props.math + '$$';
    this.state.loaded? window.MathJax.Hub.Queue(['Typeset', window.MathJax.Hub, this.preview]): loadScript(SCRIPT, this.onLoad)
    // this.preview.style.visibility = "";
  }

  shouldComponentUpdate(nextProps, nextState) {
    if (!nextProps.math) return false
    return nextProps.math !== this.state.oldMath
  }

  componentDidUpdate(prevProps, prevState) {
        // this.preview.style.visibility = "hidden";
    this.preview.innerHTML = '$$' + this.props.math + '$$';
    window.MathJax.Hub.Queue(['Typeset', window.MathJax.Hub, this.preview]);

  }

  componentWillReceiveProps(nextProps) {
    this.setState({oldMath: nextProps.math})
  }

  render() {
    return (
      <div
        className={this.props.className}
        id='react-mathjax-preview'
        style={this.props.style}
      >
        <div
          id='react-mathjax-preview-result'
          ref={(node) => {this.preview = node}}
        >
        </div>
      </div>
    )
  }
}