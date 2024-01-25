class RustFloat {
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
        return new RustFloat(this.jsNumber + other.jsNumber);
    }
    abs() {
        return new RustFloat(Math.abs(this.jsNumber));
    }
    exp(other) {
        return new RustFloat(Math.exp(this.jsNumber));
    }
}
