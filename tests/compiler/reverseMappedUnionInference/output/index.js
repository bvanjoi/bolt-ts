// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reverseMappedUnionInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
var identifierExtractor = createExtractor({
  matcher: isIdentifier,
  extract: (node) => ({
      node,
    kind: 'identifier',
    value: node.name    
  })  
});
var stringExtractor = createExtractor({
  matcher: isStringLiteral,
  extract: (node) => ({
      node,
    kind: 'string',
    value: node.value    
  })  
});
var myUnion = unionType([identifierExtractor, stringExtractor]);