import * as asserts from './asserts'
function test(obj) {
  asserts.isNonNullable(obj);
  obj.trim();
}