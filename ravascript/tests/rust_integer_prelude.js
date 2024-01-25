class RustInteger {
    constructor(jsNumber) {
        this.jsNumber = jsNumber;
    }

    eq(other) {
        return this.jsNumber === other.jsNumber;
    }
    ne(other) {
        return this.jsNumber !== other.jsNumber;
    }
    add(other) {
        return new RustInteger(this.jsNumber + other.jsNumber);
    }
    abs() {
        return new RustInteger(Math.abs(this.jsNumber));
    }
    rem(other) {
        return new RustInteger(this.jsNumber % other.jsNumber);
    }
}
