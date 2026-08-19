function f1(x) {
  if (typeof x !== 'string') {
    switch (x.kind) {
      case 'foo':
        x.name;
      
    }
  }
  
}
function f2(x) {
  if (typeof x === 'object') {
    switch (x.kind) {
      case 'foo':
        x.name;
      
    }
  }
  
}
function f3(x) {
  if (x && typeof x !== 'string') {
    switch (x.kind) {
      case 'foo':
        x.name;
      
    }
  }
  
}
function f4(x) {
  if (x && typeof x === 'object') {
    switch (x.kind) {
      case 'foo':
        x.name;
      
    }
  }
  
}
var EnumTypeNode = {};
(function (EnumTypeNode) {

  EnumTypeNode[EnumTypeNode['Pattern'] = 'Pattern'] = 'Pattern'
  EnumTypeNode[EnumTypeNode['Disjunction'] = 'Disjunction'] = 'Disjunction'
})(EnumTypeNode);
var n;
if (n.type === 'Disjunction') {
  n.alternatives.slice();
} else {
  n.elements.slice();
}
