function needsToNarrowTheType(thing) {
  if (hasAFoo(thing)) {
    console.log(thing.foo);
  } else {
    console.log(thing.bar);
  }
  
  function hasAFoo(value) {
    return 'foo' in value
  }
}