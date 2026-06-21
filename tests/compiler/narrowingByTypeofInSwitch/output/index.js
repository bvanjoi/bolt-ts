function assertNever(x) {
  return x
}
function assertNumber(x) {
  return x
}
function assertBoolean(x) {
  return x
}
function assertString(x) {
  return x
}
function assertSymbol(x) {
  return x
}
function assertFunction(x) {
  return x
}
function assertObject(x) {
  return x
}
function assertObjectOrNull(x) {
  return x
}
function assertUndefined(x) {
  return x
}
function assertAll(x) {
  return x
}
function assertStringOrNumber(x) {
  return x
}
function assertBooleanOrObject(x) {
  return x
}
function testUnion(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
      return 
    
    case 'boolean':
      assertBoolean(x);
      return 
    
    case 'function':
      assertFunction(x);
      return 
    
    case 'symbol':
      assertSymbol(x);
      return 
    
    case 'object':
      assertObject(x);
      return 
    
    case 'string':
      assertString(x);
      return 
    
    case 'undefined':
      assertUndefined(x);
      return 
    
  }
  assertNever(x);
}
function testExtendsUnion(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
      return 
    
    case 'boolean':
      assertBoolean(x);
      return 
    
    case 'function':
      assertAll(x);
      return 
    
    case 'symbol':
      assertSymbol(x);
      return 
    
    case 'object':
      assertAll(x);
      return 
    
    case 'string':
      assertString(x);
      return 
    
    case 'undefined':
      assertUndefined(x);
      return 
    
  }
  assertAll(x);
}
function testAny(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
      return 
    
    case 'boolean':
      assertBoolean(x);
      return 
    
    case 'function':
      assertFunction(x);
      return 
    
    case 'symbol':
      assertSymbol(x);
      return 
    
    case 'object':
      assertObject(x);
      return 
    
    case 'string':
      assertString(x);
      return 
    
    case 'undefined':
      assertUndefined(x);
      return 
    
  }
  assertAll(x);
}
function a1(x) {
  return x
}
function testUnionExplicitDefault(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
      return 
    
    case 'boolean':
      assertBoolean(x);
      return 
    
    case 'function':
      assertFunction(x);
      return 
    
    case 'symbol':
      assertSymbol(x);
      return 
    
    default:
      a1(x);
      return 
    
  }
}
function testUnionImplicitDefault(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
      return 
    
    case 'boolean':
      assertBoolean(x);
      return 
    
    case 'function':
      assertFunction(x);
      return 
    
    case 'symbol':
      assertSymbol(x);
      return 
    
  }
  return a1(x)
}
function testExtendsExplicitDefault(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
      return 
    
    case 'boolean':
      assertBoolean(x);
      return 
    
    case 'function':
      assertAll(x);
      return 
    
    case 'symbol':
      assertSymbol(x);
      return 
    
    default:
      assertAll(x);
      return 
    
  }
}
function testExtendsImplicitDefault(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
      return 
    
    case 'boolean':
      assertBoolean(x);
      return 
    
    case 'function':
      assertAll(x);
      return 
    
    case 'symbol':
      assertSymbol(x);
      return 
    
  }
  return assertAll(x)
}
function exhaustiveChecks(x) {
  switch (typeof x) {
    case 'number':
      return x.toString(2)
    
    case 'string':
      return x
    
    case 'function':
      return x(42)
    
    case 'object':
      return x.x
    
  }
}
function exhaustiveChecksGenerics(x) {
  switch (typeof x) {
    case 'number':
      return x.toString(2)
    
    case 'string':
      return x
    
    case 'function':
      return (x)(42)
    
    case 'object':
      return (x).x
    
  }
}
function multipleGeneric(xy) {
  switch (typeof xy) {
    case 'function':
      return [xy, xy(42)]
    
    case 'object':
      return [xy, xy.y]
    
    default:
      return assertNever(xy)
    
  }
}
function multipleGenericFuse(xy) {
  switch (typeof xy) {
    case 'function':
      return [xy, 1]
    
    case 'object':
      return [xy, 'two']
    
    case 'number':
      return [xy]
    
  }
}
function multipleGenericExhaustive(xy) {
  switch (typeof xy) {
    case 'object':
      return [xy, xy.y]
    
    case 'function':
      return [xy, xy(42)]
    
  }
}
function switchOrdering(x) {
  switch (typeof x) {
    case 'string':
      return assertString(x)
    
    case 'number':
      return assertNumber(x)
    
    case 'boolean':
      return assertBoolean(x)
    
    case 'number':
      return assertNever(x)
    
  }
}
function switchOrderingWithDefault(x) {
  function local(y) {
    return x
  }
  switch (typeof x) {
    case 'string':
    case 'number':
    default:
      return local(x)
    
    case 'string':
      return assertNever(x)
    
    case 'number':
      return assertNever(x)
    
  }
}
function fallThroughTest(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
    
    case 'string':
      assertStringOrNumber(x);
      break;
    
    default:
      assertObject(x);
    
    case 'number':
    case 'boolean':
      assertBooleanOrObject(x);
      break;
    
  }
}
function unknownNarrowing(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
      return 
    
    case 'boolean':
      assertBoolean(x);
      return 
    
    case 'function':
      assertFunction(x);
      return 
    
    case 'symbol':
      assertSymbol(x);
      return 
    
    case 'object':
      assertObjectOrNull(x);
      return 
    
    case 'string':
      assertString(x);
      return 
    
    case 'undefined':
      assertUndefined(x);
      return 
    
  }
}
function keyofNarrowing(k) {
  function assertKeyofS(k1) {}
  switch (typeof k) {
    case 'number':
      assertNumber(k);
      assertKeyofS(k);
      return 
    
    case 'symbol':
      assertSymbol(k);
      assertKeyofS(k);
      return 
    
    case 'string':
      assertString(k);
      assertKeyofS(k);
      return 
    
  }
}
function narrowingNarrows(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
      return 
    
    case 'boolean':
      assertBoolean(x);
      return 
    
    case 'function':
      assertFunction(x);
      return 
    
    case 'symbol':
      assertSymbol(x);
      return 
    
    case 'object':
      var _ = x;
      return 
    
    case 'string':
      assertString(x);
      return 
    
    case 'undefined':
      assertUndefined(x);
      return 
    
    case 'number':
      assertNever(x);
      return 
    
    default:
      var _y = x;
      return 
    
  }
}
function narrowingNarrows2(x) {
  switch (typeof x) {
    case 'number':
      assertNumber(x);
      return 
    
    case 'boolean':
      assertBoolean(x);
      return 
    
    case 'function':
      assertNever(x);
      return 
    
    case 'symbol':
      assertNever(x);
      return 
    
    case 'object':
      var _ = assertNever(x);
      return 
    
    case 'string':
      assertString(x);
      return 
    
    case 'undefined':
      assertUndefined(x);
      return 
    
    case 'number':
      assertNever(x);
      return 
    
    default:
      var _y = assertNever(x);
      return 
    
  }
}
function testUnionWithTempalte(x) {
  switch (typeof x) {
    case `number`:
      assertNumber(x);
      return 
    
    case `boolean`:
      assertBoolean(x);
      return 
    
    case `function`:
      assertFunction(x);
      return 
    
    case `symbol`:
      assertSymbol(x);
      return 
    
    case `object`:
      assertObject(x);
      return 
    
    case `string`:
      assertString(x);
      return 
    
    case `undefined`:
      assertUndefined(x);
      return 
    
  }
  assertNever(x);
}
function fallThroughTestWithTempalte(x) {
  switch (typeof x) {
    case `number`:
      assertNumber(x);
    
    case `string`:
      assertStringOrNumber(x);
      break;
    
    default:
      assertObject(x);
    
    case `number`:
    case `boolean`:
      assertBooleanOrObject(x);
      break;
    
  }
}
function keyofNarrowingWithTemplate(k) {
  function assertKeyofS(k1) {}
  switch (typeof k) {
    case `number`:
      assertNumber(k);
      assertKeyofS(k);
      return 
    
    case `symbol`:
      assertSymbol(k);
      assertKeyofS(k);
      return 
    
    case `string`:
      assertString(k);
      assertKeyofS(k);
      return 
    
  }
}
function multipleGenericFuseWithBoth(xy) {
  switch (typeof xy) {
    case `function`:
      return [xy, 1]
    
    case 'object':
      return [xy, 'two']
    
    case `number`:
      return [xy]
    
  }
}