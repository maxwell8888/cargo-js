class MyEnum {
    constructor() {}
    static fooBarId = "FooBar";
    static FooBar = {
        id: "FooBar"
    };
    static barId = "Bar";
    static bazId = "Baz";
    static Bar(data) {
        return {
            id: "Bar",
            data
        };
    }
    static Baz(arg_0, arg_1) {
        const data = {
            id: "Baz"
        };
        data.data = [arg_0, arg_1];
        return data;
    }
}
function main() {
    var myData = MyEnum.FooBar;
    var myData = MyEnum.Baz("Hi", 5);
    var matchResult;
    if (myData.id === MyEnum.fooBarId) {
        matchResult = 1;
    } else if (myData.id === MyEnum.barId) {
        var { 
          x,
          y
        } = myData.data;
        console.log(x);
        console.log(y);
        matchResult = x;
    } else if (myData.id === MyEnum.bazId) {
        var [text,
        num] = myData.data;
        console.log(text);
        console.log(num);
        matchResult = num;
    } else {
        matchResult = "this shouldn't exist";
    }
    return matchResult === 5;
}
