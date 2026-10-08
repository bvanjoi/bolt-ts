// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveConditionalCrash3.ts`, Apache-2.0 License
//@compiler-options: target=es2015
export {  }/**
 *
 * Some helper Types and Interfaces..
 *
 */
// This interface will be expanded in circular way.
/**
 * This function will return all possibile keys that can be expanded on T, only to the N deep level
 */
/**
 * Expand keys on `O` based on `Keys` parameter.
 */
/**
 * If I open the popup, (pointing with the mouse on the Expand), the compiler shows the type Expand, expanded as expected.
 *
 * It's fast and it doesn't use additional memory
 *
 */

/**
 * These two functions work as charm, also they are superfast and as expected they don't use additional Memory
 */
var y1;
var y2/**
 *
 * ... nevertheless when I need to use the Expand in other Types, as the following examples, the popup show "loading..." and without show any information and
 * the Memory Heap grows to 1.2gb (in my case) every time... You can see it opening the Chrome DevTools and check the memory Tab.
 *
 * *******
 * I think this is causing "FATAL ERROR: Ineffective mark-compacts near heap limit Allocation failed - JavaScript heap out of memory"
 * on my project during the `yarn start`.
 * *******
 *
 */
;
/**
 * but as you can see here, the expansion of Interface X it's still working.
 *
 * If a memory is still high, it may need some seconds to show popup.
 *
 */
var t;