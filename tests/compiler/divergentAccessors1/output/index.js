// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/divergentAccessors1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
{
  var ihgs = null;
  ihgs.foo = '32';
  var r_ihgs_foo = ihgs.foo;
}
{
  var t_hgs = null;
  t_hgs.foo = '32';
  var r_t_hgs_foo = t_hgs.foo;
}