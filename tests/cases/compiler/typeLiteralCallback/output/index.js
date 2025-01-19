
var foo
foo.reject("")

var test
test.fail((arg) => foo.reject(arg))
test.fail2((arg) => foo.reject(arg))