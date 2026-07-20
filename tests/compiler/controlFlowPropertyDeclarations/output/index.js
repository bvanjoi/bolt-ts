
var HTMLDOMPropertyConfig = require('react/lib/HTMLDOMPropertyConfig');
for ( var propname in HTMLDOMPropertyConfig.Properties) {
  if (!HTMLDOMPropertyConfig.Properties.hasOwnProperty(propname)) {
    continue;
  }
  
  var mapFrom = HTMLDOMPropertyConfig.DOMAttributeNames[propname] || propname.toLowerCase();
}
function repeatString(string, times) {
  if (times === 1) {
    return string;
  }
  
  if (times < 0) {
    throw new Error()
  }
  
  var repeated = '';
  while (times) {
    if (times & 1) {
      repeated += string;
    }
    
    if (times >>= 1) {
      string += string;
    }
    
  }
  return repeated;
}
function endsWith(haystack, needle) {
  return haystack.slice(-needle.length) === needle;
}
function trimEnd(haystack, needle) {
  return endsWith(haystack, needle) ? haystack.slice(0, -needle.length) : haystack;
}
function hyphenToCamelCase(string) {
  return string.replace(/-(.)/g, function (match, chr) {
    return chr.toUpperCase();
  });
}
function isEmpty(string) {
  return !/[^\s]/.test(string);
}
function isConvertiblePixelValue(value) {
  return /^\d+px$/.test(value);
}
export class HTMLtoJSX {
  output;
  level;
  _inPreTag;
  _visitText = (node) => {
    var parentTag = node.parentNode && node.parentNode.tagName.toLowerCase();
    if (parentTag === 'textarea' || parentTag === 'style') {
      return ;
    }
    
    var text = '';
    if (this._inPreTag) {
      text = text.replace(/\r/g, '').replace(/( {2,}|\n|\t|\{|\})/g, function (whitespace) {
        return '{' + JSON.stringify(whitespace) + '}';
      });
    } else {
      if (text.indexOf('
') > -1) {}
      
    }
    
    this.output += text;
  };
}
;
export class StyleParser {
  styles = {};
  toJSXString = () => {
    for ( var key in this.styles) {
      if (!this.styles.hasOwnProperty(key)) {}
      
    }
  };
}