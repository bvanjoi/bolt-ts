// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/enumNumbering1.ts`, Apache-2.0 License
var Test = {};
(function (Test) {

  Test[Test['A'] = 0] = 'A'
  Test[Test['B'] = 0] = 'B'
  Test[Test['C'] = Math.floor(Math.random() * 1000)] = 'C'
  Test[Test['D'] = 10] = 'D'
  Test[Test['E'] = 0] = 'E'
})(Test);