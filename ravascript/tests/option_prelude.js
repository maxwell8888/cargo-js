class Option {
    static someId = "Some";
    static noneId = "None";
    static None = new Option("None", null);
    constructor(id, data) {
        this.id = id;
        this.data = data;
    }
    static Some(arg_0) {
        return new Option("Some", [arg_0]);
    }
    isSomeAnd(f) {
        var ifTempAssignment;
        if (this.id === Option.noneId) {
            ifTempAssignment = false;
        } else if (this.id === Option.someId) {
            var [x] = this.data;
            ifTempAssignment = f(x);
        } else {
            ifTempAssignment = "this shouldn't exist";
        }
        return ifTempAssignment;
    }
    expect(msg) {
        var ifTempAssignment;
        if (this.id === Option.someId) {
            var [val] = this.data;
            ifTempAssignment = val;
        } else if (this.id === Option.noneId) {
            throw new Error(`${msg}`);
        } else {
            ifTempAssignment = "this shouldn't exist";
        }
        return ifTempAssignment;
    }
    unwrap() {
        var ifTempAssignment;
        if (this.id === Option.someId) {
            var [val] = this.data;
            ifTempAssignment = val;
        } else if (this.id === Option.noneId) {
            throw new Error("called `Option::unwrap()` on a `None` value");
        } else {
            ifTempAssignment = "this shouldn't exist";
        }
        return ifTempAssignment;
    }
    unwrapOr(defaultVzxyw) {
        var ifTempAssignment;
        if (this.id === Option.someId) {
            var [x] = this.data;
            ifTempAssignment = x;
        } else if (this.id === Option.noneId) {
            ifTempAssignment = defaultVzxyw;
        } else {
            ifTempAssignment = "this shouldn't exist";
        }
        return ifTempAssignment;
    }
    unwrapOrElse(f) {
        var ifTempAssignment;
        if (this.id === Option.someId) {
            var [x] = this.data;
            ifTempAssignment = x;
        } else if (this.id === Option.noneId) {
            ifTempAssignment = f();
        } else {
            ifTempAssignment = "this shouldn't exist";
        }
        return ifTempAssignment;
    }
    map(f) {
        var ifTempAssignment;
        if (this.id === Option.someId) {
            var [x] = this.data;
            ifTempAssignment = Some(f(x));
        } else if (this.id === Option.noneId) {
            ifTempAssignment = None;
        } else {
            ifTempAssignment = "this shouldn't exist";
        }
        return ifTempAssignment;
    }
}
