const util = @import("util.zig");
const io = @import("io.zig");
const types = @import("types.zig");
const strings = @import("strings.zig");

pub const print = io.print;
pub const readline = io.readline;
pub const readfile = io.readfile;
pub const writefile = io.writefile;

pub const time = util.time;
pub const len = util.len;
pub const exit = util.exit;

pub const typeOf = types.typeOf;
pub const boolval = types.bool;
pub const intval = types.int;
pub const floatval = types.float;
pub const stringval = types.string;

pub const substring = strings.substring;
pub const split = strings.split;
pub const charAt = strings.charAt;
