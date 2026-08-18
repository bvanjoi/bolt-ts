// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/cloduleTest1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var $ = {};
(function ($) {

  function ajax(options) {}
  $.ajax = ajax;
  
})($);
var it = $('.foo').addClass('bar');