// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reachabilityChecks4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noFallthroughCasesInSwitch

function foo(x, y) {
    switch (x) {
        case 1:
        case 2:
            return 1;
        case 3:
            //~^ ERROR: Fallthrough case in switch.
            if (y) {
                return 2;
            }
        case 4:
            return 3;
    }
}

declare function noop(): void;
declare function fail(): never;

function f1(x: 0 | 1 | 2) {
    switch (x) {
        case 0:
            fail();
        case 1:
            //~^ ERROR: Fallthrough case in switch.
            noop();
        case 2:
            return;
    }
}

// Repro from #34021

type Behavior = 'SLIDE' | 'SLIDE_OUT'
type Direction = 'LEFT' | 'RIGHT' | 'TOP' | 'BOTTOM'

interface Transition {
  behavior: Behavior
  direction: Direction
}

function f2(transition: Transition): any {
    switch (transition.behavior) {
        case 'SLIDE':
            switch (transition.direction) {
                case 'LEFT':
                    return []
                case 'RIGHT':
                    return []
                case 'TOP':
                    return []
                case 'BOTTOM':
                    return []
            }
        case 'SLIDE_OUT':
            switch (transition.direction) {
                case 'LEFT':
                    return []
                case 'RIGHT':
                    return []
                case 'TOP':
                    return []
                case 'BOTTOM':
                    return []
            }
    }
}
