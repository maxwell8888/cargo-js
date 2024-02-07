Number.prototype.inner = function () {
    return this;
};
// TODO not sure why I originally wrapper the vars in Number, but it doesn't seem necessary as `Number(4) === 4`
// Number.prototype.eq = function(other) { return Number(this) === Number(other); };
Number.prototype.eq = function (other) {
    return this === other.inner();
};
Number.prototype.ne = function (other) {
    return this !== other.inner();
};
Number.prototype.add = function (other) {
    return this + other.inner();
};
Number.prototype.abs = function () {
    return Math.abs(this);
};
Number.prototype.rem = function (other) {
    return this % other.inner();
};
