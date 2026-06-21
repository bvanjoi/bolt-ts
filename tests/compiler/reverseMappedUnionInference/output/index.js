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