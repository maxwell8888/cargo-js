class RustBool {
    constructor(jsBoolean) {
        this.jsBoolean = jsBoolean;
    }

    eq(other) {
        return new RustBool(this.jsBoolean === other.jsBoolean);
    }
    ne(other) {
        return new RustBool(this.jsBoolean !== other.jsBoolean);
    }
    boolAnd(other) {
        return new RustBool(this.jsBoolean && other.jsBoolean);
    }
}
