// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privacyGloImport.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration
var m1 = {};
(function (m1) {

  var m1_M1_public = {};
  (function (m1_M1_public) {
  
    class c1 {}
    m1_M1_public.c1 = c1;
    
    function f1() {
      return new c1();
    }
    m1_M1_public.f1 = f1;
    
    var v1 = c1;
    m1_M1_public.v1 = v1
    
    var v2;
    m1_M1_public.v2 = v2
    
  })(m1_M1_public);
  m1//export declare module "m1_M3_public" {
  //    export function f1();
  //    export class c1 {
  //    }
  //    export var v1: { new (): c1; };
  //    export var v2: c1;
  //}
  //declare module "m1_M4_private" {
  //    export function f1();
  //    export class c1 {
  //    }
  //    export var v1: { new (): c1; };
  //    export var v2: c1;
  //}
  .m1_M1_public = m1_M1_public;
  
  var m1_M2_private = {};
  (function (m1_M2_private) {
  
    class c1 {}
    m1_M2_private.c1 = c1;
    
    function f1() {
      return new c1();
    }
    m1_M2_private.f1 = f1;
    
    var v1 = c1;
    m1_M2_private.v1 = v1
    
    var v2;
    m1_M2_private.v2 = v2
    
  })(m1_M2_private);
  
  var m1_im1_private = m1_M1_public
  
  var m1_im1_private_v1_public = m1_im1_private.c1;
  m1.m1_im1_private_v1_public = m1_im1_private_v1_public
  
  var m1_im1_private_v2_public = new m1_im1_private.c1();
  m1.m1_im1_private_v2_public = m1_im1_private_v2_public
  
  var m1_im1_private_v3_public = m1_im1_private.f1;
  m1.m1_im1_private_v3_public = m1_im1_private_v3_public
  
  var m1_im1_private_v4_public = m1_im1_private.f1();
  m1.m1_im1_private_v4_public = m1_im1_private_v4_public
  
  var m1_im1_private_v1_private = m1_im1_private.c1;
  
  var m1_im1_private_v2_private = new m1_im1_private.c1();
  
  var m1_im1_private_v3_private = m1_im1_private.f1;
  
  var m1_im1_private_v4_private = m1_im1_private.f1();
  
  var m1_im2_private //import m1_im3_private = require("m1_M3_public");
  //export var m1_im3_private_v1_public = m1_im3_private.c1;
  //export var m1_im3_private_v2_public = new m1_im3_private.c1();
  //export var m1_im3_private_v3_public = m1_im3_private.f1;
  //export var m1_im3_private_v4_public = m1_im3_private.f1();
  //var m1_im3_private_v1_private = m1_im3_private.c1;
  //var m1_im3_private_v2_private = new m1_im3_private.c1();
  //var m1_im3_private_v3_private = m1_im3_private.f1;
  //var m1_im3_private_v4_private = m1_im3_private.f1();
  //import m1_im4_private = require("m1_M4_private");
  //export var m1_im4_private_v1_public = m1_im4_private.c1;
  //export var m1_im4_private_v2_public = new m1_im4_private.c1();
  //export var m1_im4_private_v3_public = m1_im4_private.f1;
  //export var m1_im4_private_v4_public = m1_im4_private.f1();
  //var m1_im4_private_v1_private = m1_im4_private.c1;
  //var m1_im4_private_v2_private = new m1_im4_private.c1();
  //var m1_im4_private_v3_private = m1_im4_private.f1;
  //var m1_im4_private_v4_private = m1_im4_private.f1();
  = m1_M2_private
  
  var m1_im2_private_v1_public = m1_im2_private.c1;
  m1.m1_im2_private_v1_public = //export import m1_im3_public = require("m1_M3_public");
  //export import m1_im4_public = require("m1_M4_private");
  m1_im2_private_v1_public
  
  var m1_im2_private_v2_public = new m1_im2_private.c1();
  m1.m1_im2_private_v2_public = m1_im2_private_v2_public
  
  var m1_im2_private_v3_public = m1_im2_private.f1;
  m1.m1_im2_private_v3_public = m1_im2_private_v3_public
  
  var m1_im2_private_v4_public = m1_im2_private.f1();
  m1.m1_im2_private_v4_public = m1_im2_private_v4_public
  
  var m1_im2_private_v1_private = m1_im2_private.c1;
  
  var m1_im2_private_v2_private = new m1_im2_private.c1();
  
  var m1_im2_private_v3_private = m1_im2_private.f1;
  
  var m1_im2_private_v4_private = m1_im2_private.f1();
  
  var m1_im1_public = m1_M1_public
  
  var m1_im2_public = m1_M2_private
  
})(m1);
var glo_M1_public = {};
(function (glo_M1_public) {

  class c1 {}
  glo_M1_public.c1 = c1;
  
  function f1() {
    return new c1();
  }
  glo_M1_public.f1 = f1;
  
  var v1 = c1;
  glo_M1_public.v1 = v1
  
  var v2;
  glo_M1_public.v2 = v2
  
})(glo_M1_public);



var m2 = {};
(function (m2) {

  var m4 = {};
  (function (m4) {
  
    var a = 10;
    
  })(m4);
  
})(m2);