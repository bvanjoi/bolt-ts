declare namespace M {
  namespace N {
    class C {}
  }
  import R = N;
  export import X = R;
}
export import A = M;
