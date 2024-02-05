class RustInteger {
    constructor(jsNumber) {
        this.jsNumber = jsNumber;
    }

    copy() {
        return new RustInteger(this.jsNumber);
    }
    eq(other) {
        return new RustBool(this.jsNumber === other.jsNumber);
    }
    ne(other) {
        return new RustBool(this.jsNumber !== other.jsNumber);
    }
    add(other) {
        return new RustInteger(this.jsNumber + other.jsNumber);
    }
    addAssign(other) {
        this.jsNumber += other.jsNumber;
    }
    abs() {
        return new RustInteger(Math.abs(this.jsNumber));
    }
    rem(other) {
        return new RustInteger(this.jsNumber % other.jsNumber);
    }
}
