var k = Symbol();
var Props = {};
(function (Props) {

  Props[Props['k'] = 'k'] = 'k'
})(Props);

foo.k = ['foo'];
foo['k'] = ['foo'];
foo[Props.k] = ['foo'];
foo[k] = ['foo'];