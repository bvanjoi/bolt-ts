var Test = {};
(function (Test) {

  Test[Test['A'] = 0] = 'A'
  Test[Test['B'] = 0] = 'B'
  Test[Test['C'] = Math.floor(Math.random() * 1000)] = 'C'
  Test[Test['D'] = 10] = 'D'
  Test[Test['E'] = 0] = 'E'
})(Test);