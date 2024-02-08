String.prototype.inner = function () {
    return this.valueOf();
};
String.prototype.eq = function (other) {
    return this.valueOf() === other.inner();
};
String.prototype.ne = function (other) {
    return this.valueOf() !== other.inner();
};
String.prototype.add = function (other) {
    return this.valueOf() + other.inner();
};
