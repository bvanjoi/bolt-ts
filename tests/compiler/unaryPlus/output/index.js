// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unaryPlus.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// allowed per spec
var a = +1;
var b = +('');
var E = {};
(function (E) {

  E[E['some'// also allowed, used to be errors
  ] = 0] = 'some'
  E//should be valid
  [E['thing'] = 0// should be valid
  ] = 'thing'
})(E);
;
var c = +E.some;
var x = +'3';
var y = -'3';
var z = ~'3';