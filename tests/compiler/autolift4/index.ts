// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/autoLift4.ts`, Apache-2.0 License

class Point {

    constructor(public x: number, public y: number) {

    }
    getDist() { 
        return Math.sqrt(this.x*this.x + this.y*this.y); 
    }
    static origin = new Point(0,0);
}

class Point3D extends Point {

    constructor(x: number, y: number, public z: number, m:number) {
        super(x, y);
    }
    
    getDist() {
        return Math.sqrt(this.x*this.x + this.y*this.y + this.z*this.m);
        //~^ ERROR: Property 'm' does not exist on type 'Point3D'.
    }        
}
