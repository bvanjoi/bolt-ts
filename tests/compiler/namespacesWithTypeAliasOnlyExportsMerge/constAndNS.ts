type A = number;
declare const Q: number;
declare namespace Q {
    export { A };
}
declare const try1: Q.A;
declare namespace Q2 {
    export { Q }
}
declare const try2: Q2.Q.A;
declare namespace Q3 {
    export {A as B};
}
declare const try3: Q3.B;
declare namespace Q4 {
    export { Q as default };
}
declare const try4: Q4.default.A;
export {};