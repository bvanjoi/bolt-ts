function baz([]) {}
function baz1([] = [1, 2, 3]) {}
function baz2([[]] = [[1, 2, 3]]) {}
function baz3({}) {}
function baz4({} = {
  x: 10  
}) {}