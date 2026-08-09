declare namespace m1 {
  namespace m1_M1_public {
    class c1 {}
    function f1(): m1.m1_M1_public.c1;
    var v1: typeof c1;
    var v2: c1;
  }
  namespace m1_M2_private {
    class c1 {}
    function f1(): m1_M2_private.c1;
    var v1: typeof c1;
    var v2: c1;
  }
  import m1_im1_private = m1_M1_public;
  var m1_im1_private_v1_public: typeof c1;
  var m1_im1_private_v2_public: m1.m1_M1_public.c1;
  var m1_im1_private_v3_public: () => m1.m1_M1_public.c1;
  var m1_im1_private_v4_public: m1.m1_M1_public.c1;
  var m1_im1_private_v1_private: typeof c1;
  var m1_im1_private_v2_private: m1.m1_M1_public.c1;
  var m1_im1_private_v3_private: () => m1.m1_M1_public.c1;
  var m1_im1_private_v4_private: m1.m1_M1_public.c1;
  import m1_im2_private = m1_M2_private;
  var m1_im2_private_v1_public: typeof c1;
  var m1_im2_private_v2_public: m1_M2_private.c1;
  var m1_im2_private_v3_public: () => m1_M2_private.c1;
  var m1_im2_private_v4_public: m1_M2_private.c1;
  var m1_im2_private_v1_private: typeof c1;
  var m1_im2_private_v2_private: m1_M2_private.c1;
  var m1_im2_private_v3_private: () => m1_M2_private.c1;
  var m1_im2_private_v4_private: m1_M2_private.c1;
  export import m1_im1_public = m1_M1_public;
  export import m1_im2_public = m1_M2_private;
}
declare namespace glo_M1_public {
  class c1 {}
  function f1(): glo_M1_public.c1;
  var v1: typeof c1;
  var v2: c1;
}
declare namespace "glo_M2_public" {
  function f1(): any;
  class c1 {}
  var v1: {
    new (): c1;
  };
  var v2: c1;
}
declare namespace "use_glo_M1_public" {
  import use_glo_M1_public = glo_M1_public;
  var use_glo_M1_public_v1_public: {
    new (): use_glo_M1_public.c1;
  };
  var use_glo_M1_public_v2_public: typeof use_glo_M1_public;
  var use_glo_M1_public_v3_public: () => use_glo_M1_public.c1;
  var use_glo_M1_public_v1_private: {
    new (): use_glo_M1_public.c1;
  };
  var use_glo_M1_public_v2_private: typeof use_glo_M1_public;
  var use_glo_M1_public_v3_private: () => use_glo_M1_public.c1;
  import use_glo_M2_public = "glo_M2_public";
  var use_glo_M2_public_v1_public: {
    new (): use_glo_M2_public.c1;
  };
  var use_glo_M2_public_v2_public: typeof use_glo_M2_public;
  var use_glo_M2_public_v3_public: () => use_glo_M2_public.c1;
  var use_glo_M2_public_v1_private: {
    new (): use_glo_M2_public.c1;
  };
  var use_glo_M2_public_v2_private: typeof use_glo_M2_public;
  var use_glo_M2_public_v3_private: () => use_glo_M2_public.c1;
  namespace m2 {
    import nonerrorImport = glo_M1_public;
    namespace m5 {
      import m5_nonerrorImport = glo_M1_public;
    }
  }
}
declare namespace "anotherParseError" {
  namespace m2 {
    
  }
  namespace m2 {
    
  }
}
declare namespace m2 {
  namespace m4 {
    var a: number;
  }
}
