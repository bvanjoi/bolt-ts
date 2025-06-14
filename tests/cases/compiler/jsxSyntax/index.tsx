// preset

const x: any = 1;
const children: any = {};
const aa: any = {};
const bb: any = {}

declare namespace JSX {
  interface IntrinsicElements {
    'app-def': any
  }
}

function App() {}

function App2() {}

namespace App3 {
  export function App4() {}
}

// jsx syntax:

<App>jsx is working</App>;

function e1() {
  return <App>jsx is working</App>  
}

<App/>;

<App id="w &lt; w" />;;;

<App id={4} />;

<App>
            // 1
            /* 2 */
        </App>;

<App>/* no */{/* 1 */ 123 /* 2 */}/* no */</App>;

<App>{/* this is a comment */}</App>;

<App><App2 />7x invalid-js-identifier</App>;

<App3.App4></App3.App4>;

(<App />) < x;;

<App {...{}} />;

<app-def test="&#x0026;&#38;">
bar
baz
</app-def>;

<App {...x}> {...children}{x}{...x}</App>;

function e14() {
  let x
  <App />
}

<App>&#x1f4a9;</App>;

<></>;

<App aa={aa.bb.cc} bb={bb.cc.dd}><App>{aa.b}</App></App>;

<App n:foo="bar"> {x} <App><App /></App></App>;

<>
  <>
    <>
      super deep
    </>
  </>
</>;
