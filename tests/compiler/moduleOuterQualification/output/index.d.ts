declare namespace outer {
  interface Beta {}
  namespace inner {
    interface Beta extends outer.Beta {}
  }
}
