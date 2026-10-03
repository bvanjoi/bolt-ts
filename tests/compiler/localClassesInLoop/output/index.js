'use strict';
var data = [];
for ( var x = 0; x < 2; ++x) {
  class C {}
  data.push(() => (C));
}
use(data[0]() === data[1]());