var x = 1;
var children = {};
var aa = {};
var bb = {};

function App() {}
function App2() {}
var App3 = {};
(function (App3) {

  function App4() {}
  App3.App4 = App4;
  
  var App5 = {};
  (function (App5) {
  
    function App6() {}
    App5.App6 = App6;
    
  })(App5);
  App3.App5 = App5;
  
})(App3);
<App >jsx is working</App>;
function e1() {
  return <App >jsx is working</App>
}
<App  />;
<App  id='w &lt; w' />;


<App  id={4} />;
<App >
            // 1
            /* 2 */
        </App>;
<App >/* no */{123}/* no */</App>;
<App >{}</App>;
<App ><App2  />7x invalid-js-identifier</App>;
<App3.App4 ></App3.App4>;
<App3.App5.App6 ></App3.App5.App6>;
(<App  />) < x;

<App  {...{}} />;
<app-def  test='&#x0026;&#38;'>
bar
baz
</app-def>;
<App  {...x}> {...children}{x}{...x}</App>;
function e14() {
  var x;
  <App  />;
}
<App >&#x1f4a9;</App>;
<></>;
<App  aa={aa.bb.cc} bb={bb.cc.dd}><App >{aa.b}</App></App>;
<App  n:foo='bar'> {x} <App ><App  /></App></App>;
<>
  <>
    <>
      super deep
    </>
  </>
</>;
<App >
  {true ? <App  attr={({theme}) => ({color: theme.blue})} /> : null}
</App>;
() => <App ></App>;
x ? <App >
    {() => null}
</App> : null;
<App  className={x.foo}>=</App>;
<App  className={x.foo}>=</App>;
<App >=</App>;
<App >=</App>;
<App  data-foo={x.foo} type='text' />;
<App  />;
<><App >x</App>=<App  /></>;