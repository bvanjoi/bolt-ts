var x = 1;

function App() {}
function App2() {}
var App3 = {};
(function (App3) {

  function App4() {}
  App3.App4 = App4;
  
})(App3);
var e0 = <App >jsx is working</App>;
function e1() {
  return <App >jsx is working</App>
}
var e2 = <App  />;
var e3 = <App  id="w &lt; w" />;

var e4 = <App  id={4} />;
var e5 = <App >
            // 1
            /* 2 */
        </App>;
var e6 = <App >/* no */123/* no */</App>;
var e7 = <App ></App>;
var e8 = <App ><App2  />7x invalid-js-identifier</App>;
var e9 = <App3.App4 ></App3.App4>;
var e10 = (<App  />) < x;
var e11 = <App  {...{}} />;
var e12 = <app-def  test="&#x0026;&#38;">
bar
baz
</app-def>;