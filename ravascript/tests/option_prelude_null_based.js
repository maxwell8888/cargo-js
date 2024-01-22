function optionIsSomeAnd(self, f) {
    if (self === null) {
        return false;
    } else if (self !== null) {
        return f(self);
    } else {
        throw new Error("couldn't match enum variant");
    }
}
function optionExpect(self, msg) {
    if (self !== null) {
        return self;
    } else if (self === null) {
        throw new Error(`${msg}`);
    } else {
        throw new Error("couldn't match enum variant");
    }
}
function optionUnwrap(self) {
    if (self !== null) {
        return self;
    } else if (self === null) {
        throw new Error("called `Option::unwrap()` on a `None` value");
    } else {
        throw new Error("couldn't match enum variant");
    }
}
function optionUnwrapOr(self, defaultVzxyw) {
    if (self !== null) {
        return self;
    } else if (self === null) {
        return defaultVzxyw;
    } else {
        throw new Error("couldn't match enum variant");
    }
}
function optionUnwrapOrElse(self, f) {
    if (self !== null) {
        return self;
    } else if (self === null) {
        return f();
    } else {
        throw new Error("couldn't match enum variant");
    }
}
function optionMap(self, f) {
    if (self !== null) {
        return f(x);
    } else if (self === null) {
        return null;
    } else {
        throw new Error("couldn't match enum variant");
    }
}
