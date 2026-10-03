new Map();
var potentiallyUndefinedIterable = [['1', 1], ['2', 2]];
new Map(potentiallyUndefinedIterable);
var potentiallyNullIterable = [['1', 1], ['2', 2]];
new Map(potentiallyNullIterable);