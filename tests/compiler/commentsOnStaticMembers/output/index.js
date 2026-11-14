// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsOnStaticMembers.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: removeComments=false
class test {
  /**
     * p1 comment appears in output
     */
  static p1 = '';
  /**
     * p2 comment does not appear in output
     */
  static p2;
  /**
     * p3 comment appears in output
     */
  static p3 = '';
  /**
     * p4 comment does not appear in output
     */
  static p4;
}