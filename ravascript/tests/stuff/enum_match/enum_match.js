class RustInteger {
    constructor(jsNumber) {
        this.jsNumber = jsNumber;
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
class RustBool {
    constructor(jsBoolean) {
        this.jsBoolean = jsBoolean;
    }

    eq(other) {
        return new RustBool(this.jsBoolean.eq(other.jsBoolean));
    }
    ne(other) {
        return new RustBool(this.jsBoolean.ne(other.jsBoolean));
    }
    boolAnd(other) {
        return new RustBool(this.jsBoolean && other.jsBoolean);
    }
}
class MyEnum {
    static fooBarId = "FooBar";
    static FooBar = new MyEnum("FooBar", null);
    static barId = "Bar";
    static bazId = "Baz";
    constructor(id, data) {
        this.id = id;
        this.data = data;
    }
    static Bar(data) {
        return { id: "Bar", data };
    }
    static Baz(arg_0, arg_1) {
        return new MyEnum("Baz", [arg_0, arg_1]);
    }
}
(function main() {
    var myData = MyEnum.FooBar;
    var myData = MyEnum.Bar({
        x: new RustInteger(4),
        y: new RustString("Hello"),
    });
    var myData = MyEnum.Baz(new RustString("Hi"), new RustInteger(5));
    var matchResult;
    if (myData.id === MyEnum.fooBarId) {
        matchResult = new RustInteger(1);
    } else if (myData.id === MyEnum.barId) {
        var { x, y } = myData.data;
        console.log(x);
        console.log(y);
        matchResult = x;
    } else if (myData.id === MyEnum.bazId) {
        var [text, num] = myData.data;
        console.log(text);
        console.log(num);
        matchResult = num;
    } else {
        throw new Error("couldn't match enum variant");
    }
    console.assert(matchResult.eq(new RustInteger(5)));
})();
