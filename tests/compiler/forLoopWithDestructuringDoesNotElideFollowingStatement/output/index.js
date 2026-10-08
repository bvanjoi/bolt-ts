// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/forLoopWithDestructuringDoesNotElideFollowingStatement.ts`, Apache-2.0 License
var array = [{
  a: 0,
  b: 1  
}];
for ( var {a, ...rest} of array) void a;