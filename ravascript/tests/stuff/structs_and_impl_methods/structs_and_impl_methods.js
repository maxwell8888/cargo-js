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

class MyStruct {
    constructor(age, name) {
        this.age = age;
        this.name = name;
    }

    static new(age, name) {
        return new MyStruct(age, name);
    }
    myMethod() {
        return this.name;
    }
    myMethodWithArg(inc) {
        return this.age.add(inc);
    }
    static myAssociatedMethod(inc) {
        return inc.add(new RustInteger(10));
    }
    withGeneric(inc) {
        return this.age;
    }
    getAge() {
        return this.age;
    }
}
(function main() {
    var thing = MyStruct.new(new RustInteger(2), new RustString("Bruce"));
    console.assert(thing.myMethod().eq(new RustString("Bruce")));
    console.assert(thing.myMethodWithArg(new RustInteger(2)).eq(new RustInteger(4)));
    console.assert(MyStruct.myAssociatedMethod(new RustInteger(2)).eq(new RustInteger(12)));
    console.assert(thing.withGeneric(new RustInteger(99)).eq(new RustInteger(2)));
    console.assert(thing.getAge().eq(new RustInteger(2)));
})();
