// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionCodeGenEnumWithEnumMemberConflict.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var Color = {};
(function (Color) {

  Color[Color['Color'] = 0] = 'Color'
  Color[Color['Thing'] = Color] = 'Thing'
})(Color);