function f1(a, b) {
  var x1 = cast(a, isC);
  var x2 = cast(b, isC);
}
function f2(b, c) {
  consume(b, c, useA);
  consume(c, b, useA);
  consume(b, b, useA);
  consume(c, c, useA);
}
function f3(arr) {
  if (every(arr, isC)) {
    arr;
  } else {
    arr;
  }
  
}
var SyntaxKind = {};
(function (SyntaxKind) {

  SyntaxKind[SyntaxKind['Block'] = 0] = 'Block'
  SyntaxKind[SyntaxKind['Identifier'] = 0] = 'Identifier'
  SyntaxKind[SyntaxKind['CaseClause'] = 0] = 'CaseClause'
  SyntaxKind[SyntaxKind['FunctionExpression'] = 0] = 'FunctionExpression'
  SyntaxKind[SyntaxKind['FunctionDeclaration'] = 0] = 'FunctionDeclaration'
})(SyntaxKind);
function foo(node) {
  assertNode(node, canHaveLocals);
  node;
}
function bar(node) {
  var a = tryCast(node, isExpression);
}
var SyntaxKind1 = {};
(function (SyntaxKind1) {

  SyntaxKind1[SyntaxKind1['ClassExpression'] = 0] = 'ClassExpression'
  SyntaxKind1[SyntaxKind1['ClassStatement'] = 0] = 'ClassStatement'
})(SyntaxKind1);

var maybeClassStatement = tryCast(statement, isClassLike);

var x = tryCast(types, isNodeArray);