interface Number {
  /**
   * Returns a string representation of an object.
   * @param radix Specifies a radix for converting numeric values to strings. This value is only used for numbers.
   */
  toString(radix?: number): string;

  /**
   * Returns a string representing a number in fixed-point notation.
   * @param fractionDigits Number of digits after the decimal point. Must be in the range 0 - 20, inclusive.
   */
  toFixed(fractionDigits?: number): string;

  /**
   * Returns a string containing a number represented in exponential notation.
   * @param fractionDigits Number of digits after the decimal point. Must be in the range 0 - 20, inclusive.
   */
  toExponential(fractionDigits?: number): string;

  /**
   * Returns a string containing a number represented either in exponential or fixed-point notation with a specified number of digits.
   * @param precision Number of significant digits. Must be in the range 1 - 21, inclusive.
   */
  toPrecision(precision?: number): string;

  /** Returns the primitive value of the specified object. */
  valueOf(): number;
}
