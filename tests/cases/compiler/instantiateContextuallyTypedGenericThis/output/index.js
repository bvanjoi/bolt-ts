
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/instantiateContextuallyTypedGenericThis.ts`, Apache-2.0 License
//@ run-fail
var $;
var lines;
$.each(lines, function (dit) {
  return dit.charAt(0) + this.charAt(1)
});