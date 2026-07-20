// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/cloduleTest1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

  declare function $(selector: string): $;  
  interface $ {
      addClass(className: string): $;
  }
  namespace $ {
    export interface AjaxSettings {
    }
    export function ajax(options: AjaxSettings) { }
  }
  var it: $ = $('.foo').addClass('bar');
