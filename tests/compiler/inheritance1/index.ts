// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inheritance1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Control {
    private state: any;
}
interface SelectableControl extends Control {
    select(): void;
}

class Button extends Control implements SelectableControl {
    select() { }
}
class TextBox extends Control {
    select() { }
}
class ImageBase extends Control implements SelectableControl{
  //~^ ERROR: Property 'select' is missing.
}
class Image1 extends Control {
}
class Locations implements SelectableControl {
  //~^ ERROR: Property 'state' is missing.
    select() { }
}
class Locations1 {
    select() { }
}
declare var sc: SelectableControl;
declare var c: Control;

declare var b: Button;
sc = b;
c = b;
b = sc;
b = c;
//~^ ERROR: Property 'select' is missing.

declare var t: TextBox;
sc = t;
c = t;
t = sc;
t = c;
//~^ ERROR: Property 'select' is missing.

declare var i: ImageBase;
sc = i;
//~^ ERROR: Property 'select' is missing.
c = i;
i = sc;
i = c;

declare var i1: Image1;
sc = i1;
//~^ ERROR: Property 'select' is missing.
c = i1;
i1 = sc;
i1 = c;

declare var l: Locations;
sc = l;
//~^ ERROR: Property 'state' is missing.
c = l;
//~^ ERROR: Property 'state' is missing.
l = sc;
l = c;
//~^ ERROR: Property 'select' is missing.

declare var l1: Locations1;
sc = l1;
//~^ ERROR: Property 'state' is missing.
c = l1;
//~^ ERROR: Property 'state' is missing.
l1 = sc;
l1 = c;
//~^ ERROR: Property 'select' is missing.