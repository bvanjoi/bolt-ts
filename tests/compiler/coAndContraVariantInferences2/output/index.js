// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/coAndContraVariantInferences2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail
function f1(a, b) {
  var x1 = cast(a, isC);
  // cast<A, C>
  var x2 = cast(b, isC);
// cast<A, C>
}
function f2(b, c) {
  consume(b, c, useA);
  // consume<A, C>
  consume(c, b, useA);
  // consume<A, B>
  consume(b, b, useA);
  // consume<B, B>
  consume(c, c, useA);
// consume<C, C>
}
function f3(arr) {
  if (every(arr, isC)) {
    arr;
  // readonly C[]
  } else {
    arr;
  // readonly B[]
  }
  
}
// Repro from #52111
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
  // assertNode<Node, HasLocals>
  node;
// FunctionDeclaration
}
function bar(node) {
  var a = tryCast(node, isExpression);
// tryCast<Expression, Node>
}
// Repro from #49924
var SyntaxKind1 = {};
(function (SyntaxKind1) {

  SyntaxKind1[SyntaxKind1['ClassExpression'] = 0] = 'ClassExpression'
  SyntaxKind1[SyntaxKind1['ClassStatement'] = 0] = 'ClassStatement'
})(SyntaxKind1);

var maybeClassStatement = tryCast(statement, isClassLike);// ClassLike1
// Repro from #49924


var x = tryCast(types, isNodeArray);