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
        return this.age + inc;
    }
    static myAssociatedMethod(inc) {
        return inc + 10;
    }
    withGeneric(inc) {
        return this.age;
    }
    getAge() {
        return this.age;
    }
}
(function main() {
    var thing = MyStruct.new(2, "Bruce");
    var one = thing.myMethod() === "Bruce";
    var two = thing.myMethodWithArg(2) === 4;
    var three = MyStruct.myAssociatedMethod(2) === 12;
    var four = thing.withGeneric(99) === 2;
    var five = thing.getAge() === 2;
    return one && two && three && four && five;
})();
