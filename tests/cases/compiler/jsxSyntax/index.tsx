// preset

const x: any = 1;

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

const e0 = <App>jsx is working</App>
function e1() {
  return <App>jsx is working</App>  
}
const e2 = <App/>
const e3 = <App id="w &lt; w" />;;
const e4 = <App id={4} />
const e5 = <App>
            // 1
            /* 2 */
        </App>
const e6 = <App>/* no */{/* 1 */ 123 /* 2 */}/* no */</App>;
const e7 = <App>{/* this is a comment */}</App>
const e8 = <App><App2 />7x invalid-js-identifier</App>
const e9 = <App3.App4></App3.App4>
const e10 = (<App />) < x;
const e11 = <App {...{}} />
const e12 = <app-def test="&#x0026;&#38;">
bar
baz
</app-def>;
