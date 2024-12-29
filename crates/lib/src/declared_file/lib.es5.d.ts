/////////////////////////////
/// ECMAScript APIs
/////////////////////////////

declare var NaN: number;
declare var Infinity: number;

/**
 * Evaluates JavaScript code and executes it.
 * @param x A String value that contains valid JavaScript code.
 */
declare function eval(x: string): any;

/**
 * Converts a string to an integer.
 * @param string A string to convert into a number.
 * @param radix A value between 2 and 36 that specifies the base of the number in `string`.
 * If this argument is not supplied, strings with a prefix of '0x' are considered hexadecimal.
 * All other strings are considered decimal.
 */
declare function parseInt(string: string, radix?: number): number;

interface String {
  /** Returns a string representation of a string. */
  toString(): string;

  /**
   * Returns the character at the specified index.
   * @param pos The zero-based index of the desired character.
   */
  charAt(pos: number): string;

  /**
   * Returns the Unicode value of the character at the specified location.
   * @param index The zero-based index of the desired character. If there is no character at the specified index, NaN is returned.
   */
  charCodeAt(index: number): number;

  /**
   * Returns a string that contains the concatenation of two or more strings.
   * @param strings The strings to append to the end of the string.
   */
  concat(...strings: string[]): string;

  /**
   * Returns the position of the first occurrence of a substring.
   * @param searchString The substring to search for in the string
   * @param position The index at which to begin searching the String object. If omitted, search starts at the beginning of the string.
   */
  indexOf(searchString: string, position?: number): number;

  /**
   * Returns the last occurrence of a substring in the string.
   * @param searchString The substring to search for.
   * @param position The index at which to begin searching. If omitted, the search begins at the end of the string.
   */
  lastIndexOf(searchString: string, position?: number): number;

  /**
   * Determines whether two strings are equivalent in the current locale.
   * @param that String to compare to target string
   */
  localeCompare(that: string): number;

  // /**
  //  * Matches a string with a regular expression, and returns an array containing the results of that search.
  //  * @param regexp A variable name or string literal containing the regular expression pattern and flags.
  //  */
  // match(regexp: string | RegExp): RegExpMatchArray | null;

  // /**
  //  * Replaces text in a string, using a regular expression or search string.
  //  * @param searchValue A string or regular expression to search for.
  //  * @param replaceValue A string containing the text to replace. When the {@linkcode searchValue} is a `RegExp`, all matches are replaced if the `g` flag is set (or only those matches at the beginning, if the `y` flag is also present). Otherwise, only the first match of {@linkcode searchValue} is replaced.
  //  */
  // replace(searchValue: string | RegExp, replaceValue: string): string;

  // /**
  //  * Replaces text in a string, using a regular expression or search string.
  //  * @param searchValue A string to search for.
  //  * @param replacer A function that returns the replacement text.
  //  */
  // replace(searchValue: string | RegExp, replacer: (substring: string, ...args: any[]) => string): string;

  // /**
  //  * Finds the first substring match in a regular expression search.
  //  * @param regexp The regular expression pattern and applicable flags.
  //  */
  // search(regexp: string | RegExp): number;

  /**
   * Returns a section of a string.
   * @param start The index to the beginning of the specified portion of stringObj.
   * @param end The index to the end of the specified portion of stringObj. The substring includes the characters up to, but not including, the character indicated by end.
   * If this value is not specified, the substring continues to the end of stringObj.
   */
  slice(start?: number, end?: number): string;

  // /**
  //  * Split a string into substrings using the specified separator and return them as an array.
  //  * @param separator A string that identifies character or characters to use in separating the string. If omitted, a single-element array containing the entire string is returned.
  //  * @param limit A value used to limit the number of elements returned in the array.
  //  */
  // split(separator: string | RegExp, limit?: number): string[];

  // /**
  //  * Returns the substring at the specified location within a String object.
  //  * @param start The zero-based index number indicating the beginning of the substring.
  //  * @param end Zero-based index number indicating the end of the substring. The substring includes the characters up to, but not including, the character indicated by end.
  //  * If end is omitted, the characters from start through the end of the original string are returned.
  //  */
  // substring(start: number, end?: number): string;

  // /** Converts all the alphabetic characters in a string to lowercase. */
  // toLowerCase(): string;

  // /** Converts all alphabetic characters to lowercase, taking into account the host environment's current locale. */
  // toLocaleLowerCase(locales?: string | string[]): string;

  // /** Converts all the alphabetic characters in a string to uppercase. */
  // toUpperCase(): string;

  // /** Returns a string where all alphabetic characters have been converted to uppercase, taking into account the host environment's current locale. */
  // toLocaleUpperCase(locales?: string | string[]): string;

  // /** Removes the leading and trailing white space and line terminator characters from a string. */
  // trim(): string;

  // /** Returns the length of a String object. */
  // readonly length: number;

  // // IE extensions
  // /**
  //  * Gets a substring beginning at the specified location and having the specified length.
  //  * @deprecated A legacy feature for browser compatibility
  //  * @param from The starting position of the desired substring. The index of the first character in the string is zero.
  //  * @param length The number of characters to include in the returned substring.
  //  */
  // substr(from: number, length?: number): string;

  // /** Returns the primitive value of the specified object. */
  // valueOf(): string;

  // readonly [index: number]: string;
}

interface StringConstructor {
  new (value?: any): String;
  (value?: any): string;
  readonly prototype: String;
  fromCharCode(...codes: number[]): string;
}

/**
* Allows manipulation and formatting of text strings and determination and location of substrings within strings.
*/
declare var String: StringConstructor;

interface Error {
  name: string;
  message: string;
  stack?: string;
}

interface ErrorConstructor {
  new (message?: string): Error;
  (message?: string): Error;
  readonly prototype: Error;
}

declare var Error: ErrorConstructor;

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

interface ConcatArray<T> {
  // readonly length: number;
  // readonly [n: number]: T;
  join(separator?: string): string;
  slice(start?: number, end?: number): T[];
}

interface Array<T> {
    /**
     * Gets or sets the length of the array. This is a number one higher than the highest index in the array.
     */
    length: number;
    /**
     * Returns a string representation of an array.
     */
    toString(): string;
    /**
     * Returns a string representation of an array. The elements are converted to string using their toLocaleString methods.
     */
    toLocaleString(): string;
    /**
     * Removes the last element from an array and returns it.
     * If the array is empty, undefined is returned and the array is not modified.
     */
    pop(): T | undefined;
    /**
     * Appends new elements to the end of an array, and returns the new length of the array.
     * @param items New elements to add to the array.
     */
    push(...items: T[]): number;
    /**
     * Combines two or more arrays.
     * This method returns a new array without modifying any existing arrays.
     * @param items Additional arrays and/or items to add to the end of the array.
     */
    concat(...items: ConcatArray<T>[]): T[];
    /**
     * Combines two or more arrays.
     * This method returns a new array without modifying any existing arrays.
     * @param items Additional arrays and/or items to add to the end of the array.
     */
    concat(...items: (T | ConcatArray<T>)[]): T[];
    /**
     * Adds all the elements of an array into a string, separated by the specified separator string.
     * @param separator A string used to separate one element of the array from the next in the resulting string. If omitted, the array elements are separated with a comma.
     */
    join(separator?: string): string;
    /**
     * Reverses the elements in an array in place.
     * This method mutates the array and returns a reference to the same array.
     */
    reverse(): T[];
    /**
     * Removes the first element from an array and returns it.
     * If the array is empty, undefined is returned and the array is not modified.
     */
    shift(): T | undefined;
    /**
     * Returns a copy of a section of an array.
     * For both start and end, a negative index can be used to indicate an offset from the end of the array.
     * For example, -2 refers to the second to last element of the array.
     * @param start The beginning index of the specified portion of the array.
     * If start is undefined, then the slice begins at index 0.
     * @param end The end index of the specified portion of the array. This is exclusive of the element at the index 'end'.
     * If end is undefined, then the slice extends to the end of the array.
     */
    slice(start?: number, end?: number): T[];
    // /**
    //  * Sorts an array in place.
    //  * This method mutates the array and returns a reference to the same array.
    //  * @param compareFn Function used to determine the order of the elements. It is expected to return
    //  * a negative value if the first argument is less than the second argument, zero if they're equal, and a positive
    //  * value otherwise. If omitted, the elements are sorted in ascending, ASCII character order.
    //  * ```ts
    //  * [11,2,22,1].sort((a, b) => a - b)
    //  * ```
    //  */
    // sort(compareFn?: (a: T, b: T) => number): this;
    /**
     * Removes elements from an array and, if necessary, inserts new elements in their place, returning the deleted elements.
     * @param start The zero-based location in the array from which to start removing elements.
     * @param deleteCount The number of elements to remove.
     * @returns An array containing the elements that were deleted.
     */
    splice(start: number, deleteCount?: number): T[];
    /**
     * Removes elements from an array and, if necessary, inserts new elements in their place, returning the deleted elements.
     * @param start The zero-based location in the array from which to start removing elements.
     * @param deleteCount The number of elements to remove.
     * @param items Elements to insert into the array in place of the deleted elements.
     * @returns An array containing the elements that were deleted.
     */
    splice(start: number, deleteCount: number, ...items: T[]): T[];
    /**
     * Inserts new elements at the start of an array, and returns the new length of the array.
     * @param items Elements to insert at the start of the array.
     */
    unshift(...items: T[]): number;
    /**
     * Returns the index of the first occurrence of a value in an array, or -1 if it is not present.
     * @param searchElement The value to locate in the array.
     * @param fromIndex The array index at which to begin the search. If fromIndex is omitted, the search starts at index 0.
     */
    indexOf(searchElement: T, fromIndex?: number): number;
    /**
     * Returns the index of the last occurrence of a specified value in an array, or -1 if it is not present.
     * @param searchElement The value to locate in the array.
     * @param fromIndex The array index at which to begin searching backward. If fromIndex is omitted, the search starts at the last index in the array.
     */
    lastIndexOf(searchElement: T, fromIndex?: number): number;
    // /**
    //  * Determines whether all the members of an array satisfy the specified test.
    //  * @param predicate A function that accepts up to three arguments. The every method calls
    //  * the predicate function for each element in the array until the predicate returns a value
    //  * which is coercible to the Boolean value false, or until the end of the array.
    //  * @param thisArg An object to which the this keyword can refer in the predicate function.
    //  * If thisArg is omitted, undefined is used as the this value.
    //  */
    // every<S extends T>(predicate: (value: T, index: number, array: T[]) => value is S, thisArg?: any): this is S[];
    // /**
    //  * Determines whether all the members of an array satisfy the specified test.
    //  * @param predicate A function that accepts up to three arguments. The every method calls
    //  * the predicate function for each element in the array until the predicate returns a value
    //  * which is coercible to the Boolean value false, or until the end of the array.
    //  * @param thisArg An object to which the this keyword can refer in the predicate function.
    //  * If thisArg is omitted, undefined is used as the this value.
    //  */
    // every(predicate: (value: T, index: number, array: T[]) => unknown, thisArg?: any): boolean;
    // /**
    //  * Determines whether the specified callback function returns true for any element of an array.
    //  * @param predicate A function that accepts up to three arguments. The some method calls
    //  * the predicate function for each element in the array until the predicate returns a value
    //  * which is coercible to the Boolean value true, or until the end of the array.
    //  * @param thisArg An object to which the this keyword can refer in the predicate function.
    //  * If thisArg is omitted, undefined is used as the this value.
    //  */
    // some(predicate: (value: T, index: number, array: T[]) => unknown, thisArg?: any): boolean;
    // /**
    //  * Performs the specified action for each element in an array.
    //  * @param callbackfn  A function that accepts up to three arguments. forEach calls the callbackfn function one time for each element in the array.
    //  * @param thisArg  An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
    //  */
    // forEach(callbackfn: (value: T, index: number, array: T[]) => void, thisArg?: any): void;
    // /**
    //  * Calls a defined callback function on each element of an array, and returns an array that contains the results.
    //  * @param callbackfn A function that accepts up to three arguments. The map method calls the callbackfn function one time for each element in the array.
    //  * @param thisArg An object to which the this keyword can refer in the callbackfn function. If thisArg is omitted, undefined is used as the this value.
    //  */
    // map<U>(callbackfn: (value: T, index: number, array: T[]) => U, thisArg?: any): U[];
    // /**
    //  * Returns the elements of an array that meet the condition specified in a callback function.
    //  * @param predicate A function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
    //  * @param thisArg An object to which the this keyword can refer in the predicate function. If thisArg is omitted, undefined is used as the this value.
    //  */
    // filter<S extends T>(predicate: (value: T, index: number, array: T[]) => value is S, thisArg?: any): S[];
    // /**
    //  * Returns the elements of an array that meet the condition specified in a callback function.
    //  * @param predicate A function that accepts up to three arguments. The filter method calls the predicate function one time for each element in the array.
    //  * @param thisArg An object to which the this keyword can refer in the predicate function. If thisArg is omitted, undefined is used as the this value.
    //  */
    // filter(predicate: (value: T, index: number, array: T[]) => unknown, thisArg?: any): T[];
    // /**
    //  * Calls the specified callback function for all the elements in an array. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
    //  * @param callbackfn A function that accepts up to four arguments. The reduce method calls the callbackfn function one time for each element in the array.
    //  * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
    //  */
    // reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T): T;
    // reduce(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue: T): T;
    // /**
    //  * Calls the specified callback function for all the elements in an array. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
    //  * @param callbackfn A function that accepts up to four arguments. The reduce method calls the callbackfn function one time for each element in the array.
    //  * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
    //  */
    // reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;
    // /**
    //  * Calls the specified callback function for all the elements in an array, in descending order. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
    //  * @param callbackfn A function that accepts up to four arguments. The reduceRight method calls the callbackfn function one time for each element in the array.
    //  * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
    //  */
    // reduceRight(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T): T;
    // reduceRight(callbackfn: (previousValue: T, currentValue: T, currentIndex: number, array: T[]) => T, initialValue: T): T;
    // /**
    //  * Calls the specified callback function for all the elements in an array, in descending order. The return value of the callback function is the accumulated result, and is provided as an argument in the next call to the callback function.
    //  * @param callbackfn A function that accepts up to four arguments. The reduceRight method calls the callbackfn function one time for each element in the array.
    //  * @param initialValue If initialValue is specified, it is used as the initial value to start the accumulation. The first call to the callbackfn function provides this value as an argument instead of an array value.
    //  */
    // reduceRight<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: T[]) => U, initialValue: U): U;

    // [n: number]: T;
}

interface ArrayConstructor {
  new (arrayLength?: number): any[];
  new <T>(arrayLength: number): T[];
  new <T>(...items: T[]): T[];
  (arrayLength?: number): any[];
  <T>(arrayLength: number): T[];
  <T>(...items: T[]): T[];
  // isArray(arg: any): arg is any[];
  // readonly prototype: any[];
}


declare var Array: ArrayConstructor;