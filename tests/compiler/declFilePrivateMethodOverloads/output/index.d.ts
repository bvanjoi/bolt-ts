interface IContext {
  someMethod(): any;
}
declare class c1 {
  _forEachBindingContext(bindingContext: IContext, fn: (bindingContext: IContext) => void): any;
  _forEachBindingContext(bindingContextArray: Array<IContext>, fn: (bindingContext: IContext) => void): any;
  _forEachBindingContext(context: any, fn: (bindingContext: IContext) => void): void;
  overloadWithArityDifference(bindingContext: IContext): any;
  overloadWithArityDifference(bindingContextArray: Array<IContext>, fn: (bindingContext: IContext) => void): any;
  overloadWithArityDifference(context: any): void;
}
declare class c2 {
  overload1(context: any, fn: any): any;
  overload2(context: any): any;
  overload2(context: any, fn: any): any;
}
