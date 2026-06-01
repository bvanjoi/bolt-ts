function getPermissions(user) {
  if (user === 'Jack') return 'admin'
  
  return undefined
}
var admins = ['Mike', 'Joe'].map((e) => (getPermissions(e)));
function isDefined(a) {
  return a !== undefined
}
var foundAdmins = admins.filter(isDefined);