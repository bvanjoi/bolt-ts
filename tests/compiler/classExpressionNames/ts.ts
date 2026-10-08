var o = {
    C: class {
    }
}
var oc = new o.C()

var V = class {
}
var v = new V()

var A;
A = class {
}
var a = new A()

const {
    B = class { }
} = ({ B: undefined });
var b = new B();
