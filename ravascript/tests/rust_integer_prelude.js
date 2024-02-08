class RustInteger {
    constructor(jsNumber) {
        this.jsNumber = jsNumber;
    }

    inner() {
        return this.jsNumber;
    }
    copy() {
        return this.jsNumber;
    }
    eq(other) {
        return this.jsNumber === other.inner();
    }
    ne(other) {
        return this.jsNumber !== other.inner();
    }
    add(other) {
        return this.jsNumber + other.inner();
    }
    derefAssign(other) {
        this.jsNumber = other.inner();
    }
    addAssign(other) {
        this.jsNumber += other.inner();
    }
    abs() {
        return Math.abs(this.jsNumber);
    }
    rem(other) {
        return this.jsNumber % other.inner();
    }
}
