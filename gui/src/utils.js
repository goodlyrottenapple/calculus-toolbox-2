import { join } from 'path';
import { format } from 'url';

function urlPath(p) {
  var editUrl;
  if (window.process.env.ELECTRON_START_URL)
    if (window.process.platform === "win32") editUrl = window.process.env.ELECTRON_START_URL + "#" + p
    else editUrl = editUrl = join(window.process.env.ELECTRON_START_URL , "#", p)
  else
    if (window.process.platform === "win32")
      editUrl = format({
        pathname: join(window.__dirname, 'index.html'),
        hash: p,
        protocol: 'file:',
        slashes: true
      });
    else
      editUrl = format({
        pathname: join(window.__dirname, '/../build/index.html'),
        hash: p,
        protocol: 'file:',
        slashes: true
      });
  return editUrl;
}

function getPort() {
  return window.require('electron').remote.getCurrentWindow().port
}

function prettyErrorMsg(e) {
  switch(e.tag){
    case 'TermParserError': 
      return "Expected one of the following: " + e.report.expected.filter((item, pos) => e.report.expected.indexOf(item) === pos).map((s) => "'" + s + "'").join(', ') + " or a variable after position (" + e.position[0] + "," + (e.position[1]+1) + ")."
    case 'AmbiguousTermParse': 
      return "Ambiguous parse. Expected one of the following: " + e.contents.expected.map((s) => "'" + s + "'").join(', ') + " or a variable"
    case 'TypeMismatch': 
      return "There is a type mismatch. Expected '" + e.contents.term + 
        "' to have type '" + e.contents.expectedType + "', but found type '" + e.contents.foundType + "' instead."
    case 'NoDefaultTypeDeclared':
      return "You have declared multiple types, without providing a default one. Please add the keyword 'default' before one of the type definitions"
    case 'MultipleDefaultTypes':
      return "You have declared multiple default types, but there can only be one default type."
    case 'NoTypesDeclared':
      return "Please define at least one type. For example: 'default type atprop'"
    case 'TypesNotDeclared':
      return "The following types, found in the definition of '" + 
        e.connective + "' were not declared: " + e.missingTypes.map((s) => "'" + s + "'").join(', ') + "."
    case 'IncorrectNoOfArgs':
      return "The definition of '" + e.connective + "' is incorrect. You have declared it to have " + 
        e.expected + (e.expected === 1 ? " argument" : " arguments") + " but the parser syntax contains " +
        e.noOfHoles + (e.noOfHoles === 1 ? " hole" : " holes") + " (_)."
    case 'SameNameConn':
      return "There are multiple definitions of '" + e.contents + "'."
    case 'SameParserSyntax':
      return "Both '" + e.contents[0] + "' and '" + e.contents[1] + "' have the same parser syntax. Please use unique parser syntax for each connective definition to avoid ambiguous parsing."
    default: 
      console.log(e)
      return e.tag + " error message pretty printing has not yet been implemented. For further details, please see the console output (via View > Toggle Developer Options)"
  }
}
export { urlPath, getPort, prettyErrorMsg }