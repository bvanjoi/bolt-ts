// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericsManyTypeParameters.ts`, Apache-2.0 License

function Foo<
    a1, a21, a31, a41, a51, a61,
    a119, a22, a32, a42, a52, a62,
    a219, a23, a33, a43, a53, a63,
    a319, a24, a34, a44, a54, a64,
    a419, a25, a35, a45, a55, a65,
    a519, a26, a36, a46, a56, a66,
    a619, a27, a37, a47, a57, a67,
    a71, a28, a38, a48, a58, a68,
    a81, a29, a39, a49, a59, a69,
    a91, a210, a310, a410, a510, a610,
    a111, a211, a311, a411, a511, a611,
    a112, a212, a312, a412, a512, a612,
    a113, a213, a313, a413, a513, a613,
    a114, a214, a314, a414, a514, a614,
    a115, a215, a315, a415, a515, a615,
    a116, a216, a316, a416, a516, a616,
    a117, a217, a317, a417, a517, a617,
    a118, a218, a318, a418, a518, a618>
    (
        x1: a1, y1: a21, z1: a31, a1: a41, b1: a51, c1: a61,
        x2: a119, y2: a22, z2: a32, a2: a42, b2: a52, c2: a62,
        x3: a219, y3: a23, z3: a33, a3: a43, b3: a53, c3: a63,
        x4: a319, y4: a24, z4: a34, a4: a44, b4: a54, c4: a64,
        x5: a419, y5: a25, z5: a35, a5: a45, b5: a55, c5: a65,
        x6: a519, y6: a26, z6: a36, a6: a46, b6: a56, c6: a66,
        x7: a619, y7: a27, z7: a37, a7: a47, b7: a57, c7: a67,
        x8: a71, y8: a28, z8: a38, a8: a48, b8: a58, c8: a68,
        x9: a81, y9: a29, z9: a39, a9: a49, b9: a59, c9: a69,
        x10: a91, y12: a210, z10: a310, a10: a410, b10: a510, c10: a610,
        x11: a111, y13: a211, z11: a311, a11: a411, b11: a511, c11: a611,
        x12: a112, y14: a212, z12: a312, a12: a412, b12: a512, c12: a612,
        x13: a113, y15: a213, z13: a313, a13: a413, b13: a513, c13: a613,
        x14: a114, y16: a214, z14: a314, a14: a414, b14: a514, c14: a614,
        x15: a115, y17: a215, z15: a315, a15: a415, b15: a515, c15: a615,
        x16: a116, y18: a216, z16: a316, a16: a416, b16: a516, c16: a616,
        x17: a117, y19: a217, z17: a317, a17: a417, b17: a517, c17: a617,
        x18: a118, y10: a218, z18: a318, a18: a418, b18: a518, c18: a618
    )
    {
        return [x1 , y1 , z1 , a1 , b1 , c1,
           x2 , y2 , z2 , a2 , b2 , c2,
           x3 , y3 , z3 , a3 , b3 , c3,
           x4 , y4 , z4 , a4 , b4 , c4,
           x5 , y5 , z5 , a5 , b5 , c5,
           x6 , y6 , z6 , a6 , b6 , c6,
           x7 , y7 , z7 , a7 , b7 , c7,
           x8 , y8 , z8 , a8 , b8 , c8,
           x9 , y9 , z9 , a9 , b9 , c9,
           x10 , y12 , z10 , a10 , b10 , c10,
           x11 , y13 , z11 , a11 , b11 , c11,
           x12 , y14 , z12 , a12 , b12 , c12,
           x13 , y15 , z13 , a13 , b13 , c13,
           x14 , y16 , z14 , a14 , b14 , c14,
           x15 , y17 , z15 , a15 , b15 , c15,
           x16 , y18 , z16 , a16 , b16 , c16,
           x17 , y19 , z17 , a17 , b17 , c17,
           x18 , y10 , z18 , a18 , b18 , c18];
    }