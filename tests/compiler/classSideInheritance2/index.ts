// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/classSideInheritance2.ts`, Apache-2.0 License

interface IText {
  foo: number;
}

interface TextSpan {}

class SubText extends TextBase {
      //~^ ERROR: Class 'TextBase' used before its declaration.
      constructor(text: IText, span: TextSpan) {
          super();
      }
}

class TextBase implements IText {
      public foo: number;
      public subText(span: TextSpan): IText {

          return new SubText(this, span);
      }
}