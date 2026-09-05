var SyntaxKind = {};
(function (SyntaxKind) {

  SyntaxKind[SyntaxKind['ImportDeclaration'] = 0] = 'ImportDeclaration'
  SyntaxKind[SyntaxKind['Modifier'] = 0] = 'Modifier'
  SyntaxKind[SyntaxKind['ImportClause'] = 0] = 'ImportClause'
  SyntaxKind[SyntaxKind['AssertClause'] = 0] = 'AssertClause'
  SyntaxKind[SyntaxKind['Decorator'] = 0] = 'Decorator'
})(SyntaxKind);
;


buildOverload('updateImportDeclaration').overload({
  0(node, modifiers, importClause, moduleSpecifier, assertClause) {
    return updateImportDeclaration(node, modifiers, importClause, moduleSpecifier, assertClause);
  },
  1(node, _decorators, modifiers, importClause, moduleSpecifier, assertClause) {
    return updateImportDeclaration(node, modifiers, importClause, moduleSpecifier, assertClause);
  }  
}).bind({
  0: ([, modifiers, importClause, moduleSpecifier, assertClause, other]) => ((other === undefined) && (modifiers === undefined || every(modifiers, isModifier)) && (importClause === undefined || !isArray(importClause)) && (moduleSpecifier === undefined || isExpression(moduleSpecifier)) && (assertClause === undefined || isAssertClause(assertClause))),
  1: ([, decorators, modifiers, importClause, moduleSpecifier, assertClause]) => ((decorators === undefined || every(decorators, isDecorator)) && (modifiers === undefined || isArray(modifiers)) && (importClause === undefined || isImportClause(importClause)) && (moduleSpecifier !== undefined && isExpression(moduleSpecifier)) && (assertClause === undefined || isAssertClause(assertClause)))  
}).deprecate({
  1: DISALLOW_DECORATORS  
}).finish();

function foo() {
  every(modifiers, isModifier);
  every(modifiers, isDecorator);
}