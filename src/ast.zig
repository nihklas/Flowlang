const Expr = union(enum) {
    literal: struct {
        value: Token,
    },
    grouping: struct {
        expr: *Expr,
        token: Token,
    },
    unary: struct {
        op: Token,
        expr: *Expr,
    },
    binary: struct {
        left: *Expr,
        op: Token,
        right: *Expr,
    },
    logical: struct {
        left: *Expr,
        op: Token,
        right: *Expr,
    },
    assignment: struct {
        name: Token,
        value: *Expr,
    },
};

const Stmt = union(enum) {
    expr: struct {
        expr: *Expr,
    },
    block: struct {
        stmts: []*Stmt,
    },
    loop: struct {
        condition: *Expr,
        body: *Stmt,
    },
    @"if": struct {
        condition: *Expr,
        true_branch: *Stmt,
        false_branch: ?*Stmt,
    },
    @"return": struct {
        token: Token,
        value: *Expr,
    },
    channel_read: struct {
        channel: Token,
        op: Token,
        result: Token,
    },
    channel_write: struct {
        channel: Token,
        op: Token,
        value: *Expr,
    },
    variable: struct {
        name: Token,
        constant: bool,
        value: ?*Expr,
        // TODO: Add Type information
    },
    channel: struct {
        name: Token,
        // TODO: Add Type information
    },
    function: struct {
        name: Token,
        params: []Token,
        body: []*Stmt,
    },
};

const Token = @import("Token.zig");
