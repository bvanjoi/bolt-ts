// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/castExpressionParentheses.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false

(({
  a: 0  
}));
([1, 3]);
('string');
(23);
(1);
(1);
(1);
(120000000000000000000000000000000000);
(255);
(/regexp/g);
(false);
(true);
(null);
(this);
(this.x);
((a).x);
(a);
(a[0]);
(a.b['0']);
(a()).x;
(1).foo;
(1).foo;
(1).foo;
(120000000000000000000000000000000000).foo;
(255).foo;

((1));
(new A()).foo;
(typeof A).x;
(-A).x;
new (A())();
(() => {})();
(function foo() {})();
(-A).x;
((-A)).x;
((A));