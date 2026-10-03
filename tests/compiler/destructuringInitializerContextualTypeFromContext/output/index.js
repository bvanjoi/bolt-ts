var Parent = ({children, name = 'Artemis', ...props}) => (Child({
  name,
  ...props  
}));
var Child = ({children, name = 'Artemis', ...props}) => (`name: ${name} props: ${JSON.stringify(props)}`);
f(([_1, _2 = undefined]) => (undefined));