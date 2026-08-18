// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicateTopLevelTypeParameter.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function getPermissions(user) {
  if (user === 'Jack') return 'admin';
  
  return undefined;
}
var admins = ['Mike', 'Joe'].map((e) => (getPermissions(e)));
function isDefined(a) {
  return a !== undefined;
}
var foundAdmins = admins.filter(isDefined);