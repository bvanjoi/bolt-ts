
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowPropertyDeclarations.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail
var HTMLDOMPropertyConfig = require('react/lib/HTMLDOMPropertyConfig');
for ( var propname in HTMLDOMPropertyConfig// Populate property map with ReactJS's attribute and property mappings
// TODO handle/use .Properties value eg: MUST_USE_PROPERTY is not HTML attr
.Properties) {
  if (!HTMLDOMPropertyConfig.Properties.hasOwnProperty(propname)) {
    continue;
  }
  
  var mapFrom = HTMLDOMPropertyConfig.DOMAttributeNames[propname] || propname.toLowerCase();
}
function repeatString(string, times/**
 * Repeats a string a certain number of times.
 * Also: the future is bright and consists of native string repetition:
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/repeat
 *
 * @param {string} string  String to repeat
 * @param {number} times   Number of times to repeat string. Integer.
 * @see http://jsperf.com/string-repeater/2
 */
) {
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
function endsWith(haystack, needle) /**
 * Determine if the string ends with the specified substring.
 *
 * @param {string} haystack String to search in
 * @param {string} needle   String to search for
 * @return {boolean}
 */
{
  return haystack.slice(-needle.length) === needle;
}
function trimEnd(haystack, needle) /**
 * Trim the specified substring off the string. If the string does not end
 * with the specified substring, this is a no-op.
 *
 * @param {string} haystack String to search in
 * @param {string} needle   String to search for
 * @return {string}
 */
{
  return endsWith(haystack, needle) ? haystack.slice(0, -needle.length) : haystack;
}
function hyphenToCamelCase(string) {
  return /**
 * Convert a hyphenated string to camelCase.
 */
  string.replace(/-(.)/g, function (match, chr) {
    return chr.toUpperCase();
  });
}
function isEmpty(string) {
  return /**
 * Determines if the specified string consists entirely of whitespace.
 */
  !/[^\s]/.test(string);
}
function isConvertiblePixelValue(value) {
  return /**
 * Determines if the CSS value can be converted from a
 * 'px' suffixed string to a numeric value
 *
 * @param {string} value CSS property value
 * @return {boolean}
 */
  /^\d+px$/.test(value);
}
export class HTMLtoJSX {
  output;
  level;
  _inPreTag;
  /**
   * Handles processing of the specified text node
   *
   * @param {TextNode} node
   */
  _visitText = (node) => {
    var parentTag = node.parentNode && node.parentNode.tagName.toLowerCase();
    if (parentTag === 'textarea' || parentTag === 'style') {
      // Ignore text content of textareas and styles, as it will have already been moved
      // to a "defaultValue" attribute and "dangerouslySetInnerHTML" attribute respectively.
      return ;
    }
    
    var text = '';
    if (this._inPreTag) // If this text is contained within a <pre>, we need to ensure the JSX
    // whitespace coalescing rules don't eat the whitespace. This means
    // wrapping newlines and sequences of two or more spaces in variables.
    {
      text = text.replace(/\r/g, '').replace(/( {2,}|\n|\t|\{|\})/g, function (whitespace) {
        return '{' + JSON.stringify(whitespace) + '}';
      });
    } else // If there's a newline in the text, adjust the indent level
    {
      if (text.indexOf('
') > -1) {}
      
    }
    
    this.output += text;
  };
/**
 * Handles parsing of inline styles
 */
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