interface Array1<T> {
  /**
   * Returns an object whose properties have the value 'true'
   * when they will be absent when used in a 'with' statement.
   */
  [Symbol.unscopables](): {
      copyWithin: boolean;
      entries: boolean;
      fill: boolean;
      find: boolean;
      findIndex: boolean;
      keys: boolean;
      values: boolean;
  };
}
