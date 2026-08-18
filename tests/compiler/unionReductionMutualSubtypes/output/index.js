// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unionReductionMutualSubtypes.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var k = {
  something() {}  
};

function run(options) {
  var something = options.something ?? val.something;
  something('');
}