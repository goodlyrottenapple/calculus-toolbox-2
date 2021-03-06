// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

import CodeMirror from 'codemirror'

CodeMirror.defineMode("calcRules", function(_config, modeConfig) {

  function switchState(source, setState, f) {
    setState(f);
    return f(source, setState);
  }

  // These should all be Unicode extended, as per the Haskell 2010 report
  var smallRE = /[a-z_]/;
  var largeRE = /[A-Z]/;
  var digitRE = /\d/;
  var hexitRE = /[0-9A-Fa-f]/;
  var octitRE = /[0-7]/;
  var idRE = /[a-z_A-Z0-9'\xa1-\uffff]/;
  var symbolRE = /[-!#$%&*+.\/<=>?@\\^|~:]/;
  var specialRE = /[(),;[\]`{}]/;
  var whiteCharRE = /[ \t\v\f]/; // newlines are handled in tokenizer

  function normal(source, setState) {
    if (source.eatWhile(whiteCharRE)) {
      return null;
    }

    var ch = source.next();
    if (specialRE.test(ch)) {
      if (ch === '{' && source.eat('-')) {
        var t = "comment";
        // if (source.eat('#')) {
        //   t = "meta";
        // }
        return switchState(source, setState, ncomment(t, 1));
      }
      return null;
    }

    if (ch === '\'') {
      if (source.eat('\\')) {
        source.next();  // should handle other escapes here
      }
      else {
        source.next();
      }
      if (source.eat('\'')) {
        return "string";
      }
      return "string error";
    }

    if (ch === '"') {
      return switchState(source, setState, stringLiteral);
    }

    if (largeRE.test(ch)) {
      source.eatWhile(idRE);
      if (source.eat('.')) {
        return "qualifier";
      }
      return "variable";
    }

    if (smallRE.test(ch)) {
      source.eatWhile(idRE);
      return "variable";
    }

    if (digitRE.test(ch)) {
      if (ch === '0') {
        if (source.eat(/[xX]/)) {
          source.eatWhile(hexitRE); // should require at least 1
          return "variable";
        }
        if (source.eat(/[oO]/)) {
          source.eatWhile(octitRE); // should require at least 1
          return "variable";
        }
      }
      source.eatWhile(digitRE);
      var t = "variable";
      if (source.match(/^\.\d+/)) {
        t = "variable";
      }
      if (source.eat(/[eE]/)) {
        t = "variable";
        source.eat(/[-+]/);
        source.eatWhile(digitRE); // should require at least 1
      }
      return t;
    }

    if (ch === "." && source.eat("."))
      return "keyword";

    if (symbolRE.test(ch)) {
      if (ch === '-' && source.eat(/-/)) {
        source.eatWhile(/-/);
        if (!source.eat(symbolRE)) {
          // source.skipToEnd();
          return "variable-2";
        }
      }
      if (ch === '=' && source.eat(/=/)) {
        source.eatWhile(/=/);
        if (!source.eat(symbolRE)) {
          return "variable-2";
        }
      }
      var t = "variable";
      if (ch === ':') {
        t = "variable-2";
      }
      source.eatWhile(symbolRE);
      return t;
    }

    return "error";
  }

  function ncomment(type, nest) {
    if (nest === 0) {
      return normal;
    }
    return function(source, setState) {
      var currNest = nest;
      while (!source.eol()) {
        var ch = source.next();
        if (ch === '{' && source.eat('-')) {
          ++currNest;
        }
        else if (ch === '-' && source.eat('}')) {
          --currNest;
          if (currNest === 0) {
            setState(normal);
            return type;
          }
        }
      }
      setState(ncomment(type, currNest));
      return type;
    };
  }

  function stringLiteral(source, setState) {
    while (!source.eol()) {
      var ch = source.next();
      if (ch === '"') {
        setState(normal);
        return "string";
      }
      if (ch === '\\') {
        if (source.eol() || source.eat(whiteCharRE)) {
          setState(stringGap);
          return "string";
        }
        if (source.eat('&')) {
        }
        else {
          source.next(); // should handle other escapes here
        }
      }
    }
    setState(normal);
    return "string error";
  }

  function stringGap(source, setState) {
    if (source.eat('\\')) {
      return switchState(source, setState, stringLiteral);
    }
    source.next();
    setState(normal);
    return "error";
  }


  var wellKnownWords = (function() {
    var wkw = {};
    function setType(t) {
      return function () {
        for (var i = 0; i < arguments.length; i++)
          wkw[arguments[i]] = t;
      };
    }

    // setType("keyword")(
    //   "default", "type", "_", "MACRO", "LeftAssoc", "RightAssoc", "NonAssoc");

    // setType("keyword")(
    //   "-", "=");

    setType("keyword")(
      "{" , "}" , "|-");

    // setType("builtin")(
    //   "Bool", "Bounded", "Char", "Double", "EQ", "Either", "Enum", "Eq",
    //   "False", "FilePath", "Float", "Floating", "Fractional", "Functor", "GT",
    //   "IO", "IOError", "Int", "Integer", "Integral", "Just", "LT", "Left",
    //   "Maybe", "Monad", "Nothing", "Num", "Ord", "Ordering", "Rational", "Read",
    //   "ReadS", "Real", "RealFloat", "RealFrac", "Right", "Show", "ShowS",
    //   "String", "True");

    // setType("builtin")();

    var override = modeConfig.overrideKeywords;
    if (override) for (var word in override) if (override.hasOwnProperty(word))
      wkw[word] = override[word];

    return wkw;
  })();



  return {
    startState: function ()  { return { f: normal }; },
    copyState:  function (s) { return { f: s.f }; },

    token: function(stream, state) {
      var t = state.f(stream, function(s) { state.f = s; });
      var w = stream.current();
      return wellKnownWords.hasOwnProperty(w) ? wellKnownWords[w] : t;
    },

    blockCommentStart: "{-",
    blockCommentEnd: "-}"
  };

});

CodeMirror.defineMIME("text/x-haskell", "haskell");

// });
