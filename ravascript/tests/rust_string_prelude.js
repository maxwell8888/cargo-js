class RustString {
    constructor(jsString) {
        this.jsString = jsString;
    }

    eq(other) {
        return new RustBool(this.jsString === other.jsString);
    }
    ne(other) {
        return new RustBool(this.jsString !== other.jsString);
    }
    toString() {
        return this.clone();
    }
    clone() {
        return new RustString(this.jsString);
    }
}
