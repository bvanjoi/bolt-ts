var foo = {};
(function (foo) {

foo[foo['b'] = 1] = 'b'
foo[foo['c'] = 2] = 'c'
foo[foo['d'] = 3] = 'd'
})(foo)