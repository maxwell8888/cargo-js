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
        return {
            id: "Bar",
            data
        };
    }
    static Baz(arg_0, arg_1) {
        return new MyEnum("Baz", [
            arg_0, 
            arg_1
        ]);
    }
}
(function main() {
    var myData = MyEnum.FooBar;
    var myData = MyEnum.Bar({ x: 4, y: "Hello" });
    var myData = MyEnum.Baz("Hi", 5);
    var matchResult;
    if (myData.id === MyEnum.fooBarId) {
        matchResult = 1;
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
        matchResult = "this shouldn't exist";
    }
    return matchResult === 5;
})();
