type A = string;
type B = number;
declare namespace NS1 {
    export { NS2, A };
}
declare namespace NS2 {
    export { NS1, B };
}
export {};
declare const try1: NS1.A;
declare const try2: NS2.B;
declare const try3: NS1.NS2.B;
declare const try4: NS2.NS1.A;
declare const try5: NS1.NS2.NS1.A;
declare const try6: NS2.NS1.NS2.B;
