function App() {}

<App >>=</App>;
//~^ ERROR: Unexpected token. Did you mean `{'>'}` or `&gt;`?
<App className='123'>>=</App>;
//~^ ERROR: Unexpected token. Did you mean `{'>'}` or `&gt;`?
<App className={123}>>=</App>;
//~^ ERROR: Unexpected token. Did you mean `{'>'}` or `&gt;`?
