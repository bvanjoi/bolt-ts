var E = {};
(function (E) {

  E[E['S1'] = 'foo'] = 'S1'
  E[E['S2'] = 'bar'] = 'S2'
  E[E['N1'] = 1000] = 'N1'
  E[E['N2'] = 25] = 'N2'
})(E);

if (someNumber > E.N2) {
  someNumber = E.N2;
}


if (someNumber > unionOfEnum) {
  someNumber = E.N2;
}


if (someString > E.S1) {
  someString = E.S2;
}

var E2 = {};
(function (E2) {

  E2[E2['S1'] = 'foo'] = 'S1'
  E2[E2['N1'] = 1000] = 'N1'
  E2[E2['C1'] = someValue()] = 'C1'
})(E2);
someString > E2.S1;
someNumber > E2.N1;
someNumber > E2.C1;