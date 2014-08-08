// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        return f;
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            return t.f = t.f.f();
        } else {
            return t.f;
        }
    } else {
        return t;
    }
}

// Export Haste, A and E. Haste because we need to preserve exports, A and E
// because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = A(f, [[0, str.charCodeAt(i)], acc]);
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = A(f, [mv.x]);
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
    }
}

var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.target.offsetLeft || 0),
	    posy - (e.target.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                A(cb,[[0,k.keyCode],0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,x.button],[0,mx,my],0]);
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,mx,my],0]);
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[0,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,[0,xhr.responseText]],0]);
            } else {
                A(cb,[[0],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=false,_1=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_2=function(_3){var _4=A(_3,[_]);return E(_4);},_5=function(_6){return _2(function(_){var _=0;return eval(E(_6)[1]);});},_7=new T(function(){return _5(_1);}),_8=[0,0],_9=[0],_a=function(_b,_){return _9;},_c=function(_){return _9;},_d=[0,_c,_a],_e=2,_f=[1],_g=[0],_h=[0,_g,_8,_e,_d,_0,_f],_i=function(_){var _=0,_j=newMVar(),_=putMVar(_j,_h);return [0,_j];},_k=new T(function(){return _2(_i);}),_l=function(_m,_n,_){var _o=E(_k)[1],_p=takeMVar(_o),_q=A(_m,[_p,_]),_r=E(_q),_s=E(_r[1]),_t=_s[1],_u=_s[2],_=putMVar(_o,new T(function(){var _v=E(_r[2]);return [0,_v[1],_v[2],_v[3],_v[4],_0,_v[6]];}));if(!E(E(_p)[5])){var _w=A(_t,[_n,_]);return _u;}else{var _x=A(_7,[E(E(_n)[1]),_]),_y=A(_t,[[0,_x],_]);return _u;}},_z=unCStr("id"),_A=0,_B=function(_C,_D,_E,_F){return A(_C,[new T(function(){return function(_){var _G=jsSetAttr(E(_D)[1],toJSStr(E(_E)),toJSStr(E(_F)));return _A;};})]);},_H=function(_I){return E(_I);},_J=function(_K,_L,_M,_){var _N=E(_L),_O=A(_K,[_M,_]),_P=A(_B,[_H,_O,_N[1],_N[2],_]);return _O;},_Q=function(_R,_S){while(1){var _T=(function(_U,_V){var _W=E(_V);if(!_W[0]){return E(_U);}else{_R=function(_X,_){return _J(_U,_W[1],_X,_);};_S=_W[2];return null;}})(_R,_S);if(_T!=null){return _T;}}},_Y=function(_Z,_10,_){return [0,_A,_Z];},_11=function(_12,_){return [0,_12,_12];},_13=[0,coercionToken],_14=function(_15,_16,_){var _17=A(_15,[_]);return A(_16,[_]);},_18=function(_19,_1a,_){return _14(_19,_1a,_);},_1b=function(_1c,_1d,_){var _1e=A(_1c,[_]);return A(_1d,[_1e,_]);},_1f=unCStr("base"),_1g=unCStr("GHC.IO.Exception"),_1h=unCStr("IOException"),_1i=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1f,_1g,_1h],_1j=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1i,_g],_1k=function(_1l){return E(_1j);},_1m=function(_1n){return E(E(_1n)[1]);},_1o=unCStr("Maybe.fromJust: Nothing"),_1p=new T(function(){return err(_1o);}),_1q=function(_1r,_1s,_1t){var _1u=new T(function(){var _1v=A(_1r,[_1t]),_1w=A(_1s,[new T(function(){var _1x=E(_1u);return _1x[0]==0?E(_1p):E(_1x[1]);})]),_1y=hs_eqWord64(_1v[1],_1w[1]);if(!E(_1y)){return [0];}else{var _1z=hs_eqWord64(_1v[2],_1w[2]);return E(_1z)==0?[0]:[1,_1t];}});return E(_1u);},_1A=function(_1B){var _1C=E(_1B);return _1q(_1m(_1C[1]),_1k,_1C[2]);},_1D=unCStr(": "),_1E=[0,41],_1F=unCStr(" ("),_1G=function(_1H,_1I){var _1J=E(_1H);return _1J[0]==0?E(_1I):[1,_1J[1],new T(function(){return _1G(_1J[2],_1I);})];},_1K=unCStr("already exists"),_1L=unCStr("does not exist"),_1M=unCStr("protocol error"),_1N=unCStr("failed"),_1O=unCStr("invalid argument"),_1P=unCStr("inappropriate type"),_1Q=unCStr("hardware fault"),_1R=unCStr("unsupported operation"),_1S=unCStr("timeout"),_1T=unCStr("resource vanished"),_1U=unCStr("interrupted"),_1V=unCStr("resource busy"),_1W=unCStr("resource exhausted"),_1X=unCStr("end of file"),_1Y=unCStr("illegal operation"),_1Z=unCStr("permission denied"),_20=unCStr("user error"),_21=unCStr("unsatisified constraints"),_22=unCStr("system error"),_23=function(_24,_25){switch(E(_24)){case 0:return _1G(_1K,_25);case 1:return _1G(_1L,_25);case 2:return _1G(_1V,_25);case 3:return _1G(_1W,_25);case 4:return _1G(_1X,_25);case 5:return _1G(_1Y,_25);case 6:return _1G(_1Z,_25);case 7:return _1G(_20,_25);case 8:return _1G(_21,_25);case 9:return _1G(_22,_25);case 10:return _1G(_1M,_25);case 11:return _1G(_1N,_25);case 12:return _1G(_1O,_25);case 13:return _1G(_1P,_25);case 14:return _1G(_1Q,_25);case 15:return _1G(_1R,_25);case 16:return _1G(_1S,_25);case 17:return _1G(_1T,_25);default:return _1G(_1U,_25);}},_26=[0,125],_27=unCStr("{handle: "),_28=function(_29,_2a,_2b,_2c,_2d,_2e){var _2f=new T(function(){var _2g=new T(function(){return _23(_2a,new T(function(){var _2h=E(_2c);return _2h[0]==0?E(_2e):_1G(_1F,new T(function(){return _1G(_2h,[1,_1E,_2e]);}));}));}),_2i=E(_2b);return _2i[0]==0?E(_2g):_1G(_2i,new T(function(){return _1G(_1D,_2g);}));}),_2j=E(_2d);if(!_2j[0]){var _2k=E(_29);if(!_2k[0]){return E(_2f);}else{var _2l=E(_2k[1]);return _2l[0]==0?_1G(_27,new T(function(){return _1G(_2l[1],[1,_26,new T(function(){return _1G(_1D,_2f);})]);})):_1G(_27,new T(function(){return _1G(_2l[1],[1,_26,new T(function(){return _1G(_1D,_2f);})]);}));}}else{return _1G(_2j[1],new T(function(){return _1G(_1D,_2f);}));}},_2m=function(_2n){var _2o=E(_2n);return _28(_2o[1],_2o[2],_2o[3],_2o[4],_2o[6],_g);},_2p=function(_2q,_2r){var _2s=E(_2q);return _28(_2s[1],_2s[2],_2s[3],_2s[4],_2s[6],_2r);},_2t=[0,44],_2u=[0,93],_2v=[0,91],_2w=function(_2x,_2y,_2z){var _2A=E(_2y);return _2A[0]==0?unAppCStr("[]",_2z):[1,_2v,new T(function(){return A(_2x,[_2A[1],new T(function(){var _2B=function(_2C){var _2D=E(_2C);return _2D[0]==0?E([1,_2u,_2z]):[1,_2t,new T(function(){return A(_2x,[_2D[1],new T(function(){return _2B(_2D[2]);})]);})];};return _2B(_2A[2]);})]);})];},_2E=function(_2F,_2G){return _2w(_2p,_2F,_2G);},_2H=function(_2I,_2J,_2K){var _2L=E(_2J);return _28(_2L[1],_2L[2],_2L[3],_2L[4],_2L[6],_2K);},_2M=[0,_2H,_2m,_2E],_2N=new T(function(){return [0,_1k,_2M,_2O,_1A];}),_2O=function(_2P){return [0,_2N,_2P];},_2Q=7,_2R=function(_2S){return [0,_9,_2Q,_g,_2S,_9,_9];},_2T=function(_2U,_){return die(new T(function(){return _2O(new T(function(){return _2R(_2U);}));}));},_2V=function(_2W,_){return _2T(_2W,_);},_2X=function(_2Y,_){return _2Y;},_2Z=[0,_1b,_18,_2X,_2V],_30=function(_31){return E(E(_31)[1]);},_32=function(_33,_34,_35,_36){return A(_30,[_33,new T(function(){return A(_34,[_36]);}),function(_37){return A(_35,[new T(function(){return E(E(_37)[1]);}),new T(function(){return E(E(_37)[2]);})]);}]);},_38=function(_39,_3a,_3b,_3c){return A(_30,[_39,new T(function(){return A(_3a,[_3c]);}),function(_3d){return A(_3b,[new T(function(){return E(E(_3d)[2]);})]);}]);},_3e=function(_3f,_3g,_3h,_3i){return _38(_3f,_3g,_3h,_3i);},_3j=function(_3k){return E(E(_3k)[4]);},_3l=function(_3m,_3n){var _3o=new T(function(){return A(_3j,[_3m,_3n]);});return function(_3p){return E(_3o);};},_3q=function(_3r){return E(E(_3r)[3]);},_3s=function(_3t){var _3u=new T(function(){return _3q(_3t);});return [0,function(_3g,_3h,_3i){return _32(_3t,_3g,_3h,_3i);},function(_3g,_3h,_3i){return _3e(_3t,_3g,_3h,_3i);},function(_3v,_3w){return A(_3u,[[0,_3v,_3w]]);},function(_3i){return _3l(_3t,_3i);}];},_3x=new T(function(){return _3s(_2Z);}),_3y=[0,112],_3z=function(_3A,_3B){var _3C=jsShowI(_3A);return _1G(fromJSStr(_3C),_3B);},_3D=[0,41],_3E=[0,40],_3F=function(_3G,_3H,_3I){return _3H>=0?_3z(_3H,_3I):_3G<=6?_3z(_3H,_3I):[1,_3E,new T(function(){var _3J=jsShowI(_3H);return _1G(fromJSStr(_3J),[1,_3D,_3I]);})];},_3K=function(_3L,_3M,_3N,_3O){var _3P=E(_3M);return A(_3P[1],[new T(function(){var _3Q=E(_3L);return E(_3N);}),function(_3R){var _3S=new T(function(){return E(E(_3R)[2]);});return A(_3P[2],[new T(function(){return A(_3O,[new T(function(){var _3T=E(new T(function(){var _3U=E(_3L);return [0,coercionToken];})),_3V=E(_3R);return [0,_3V[1],new T(function(){return [0,E(_3S)[1]+1|0];}),_3V[3],_3V[4],_3V[5],_3V[6]];})]);}),new T(function(){return A(_3P[3],[[1,_3y,new T(function(){return _1G(_3F(0,E(_3S)[1],_g),new T(function(){return E(E(_3R)[1]);}));})]]);})]);}]);},_3W=new T(function(){return _3K(_13,_3x,_11,_Y);}),_3X=unCStr("span"),_3Y=function(_3Z,_40,_){var _41=jsCreateElem(toJSStr(E(_3Z))),_42=jsAppendChild(_41,E(_40)[1]);return [0,_41];},_43=function(_X,_){return _3Y(_3X,_X,_);},_44=unCStr(" could be found!"),_45=function(_46){return err(unAppCStr("No element with ID ",new T(function(){return _1G(_46,_44);})));},_47=function(_48,_49,_){var _4a=E(_49),_4b=jsFind(toJSStr(_4a)),_4c=E(_4b);if(!_4c[0]){return _45(_4a);}else{var _4d=E(_4c[1]),_4e=jsClearChildren(_4d[1]);return _l(_48,_4d,_);}},_4f=function(_4g,_4h,_4i,_){var _4j=A(_3W,[_4i,_]),_4k=E(_4j),_4l=_4k[1],_4m=E(_4k[2]),_4n=_4m[2],_4o=E(_4m[4]),_4p=A(_4g,[[0,_4m[1],_4n,_4m[3],[0,function(_){return _47(function(_4q,_){var _4r=A(_4g,[new T(function(){var _4s=E(_4q);return [0,_4s[1],_4n,_4s[3],_4s[4],_4s[5],_4s[6]];}),_]);return [0,[0,_2X,E(E(_4r)[1])[2]],_4q];},_4l,_);},function(_4t,_){var _4u=_47(new T(function(){return A(_4h,[_4t]);}),_4l,_),_4v=E(_4u);return _4v[0]==0?_9:A(_4o[2],[_4v[1],_]);}],_4m[5],_4m[6]],_]),_4w=E(_4p),_4x=_4w[2],_4y=E(_4w[1]),_4z=_4y[1],_4A=new T(function(){return _Q(_43,[1,[0,_z,_4l],_g]);}),_4B=E(_4y[2]);if(!_4B[0]){return [0,[0,function(_4C,_){var _4D=A(_4z,[_4C,_]),_4E=A(_4A,[_4C,_]);return _4C;},_9],new T(function(){var _4F=E(_4x);return [0,_4F[1],_4F[2],_4F[3],_4o,_4F[5],_4F[6]];})];}else{var _4G=A(_4h,[_4B[1],new T(function(){var _4H=E(_4x);return [0,_4H[1],_4H[2],_4H[3],_4o,_4H[5],_4H[6]];}),_]),_4I=E(_4G),_4J=E(_4I[1]);return [0,[0,function(_4K,_){var _4L=A(_4z,[_4K,_]),_4M=A(_4A,[_4K,_]),_4N=A(_4J[1],[_4M,_]);return _4K;},_4J[2]],_4I[2]];}},_4O=unCStr("padding:15px;border-style:dotted"),_4P=unCStr("border-collapse:collapse"),_4Q=unCStr("vertical-align:top"),_4R=[0,3],_4S=function(_4T,_4U,_){var _4V=jsCreateTextNode(toJSStr(E(_4T))),_4W=jsAppendChild(_4V,E(_4U)[1]);return [0,_4V];},_4X=[0,112],_4Y=[1,_4X,_g],_4Z=function(_50,_51){var _52=new T(function(){return A(_50,[_51]);});return function(_53,_){var _54=jsCreateElem(toJSStr(_4Y)),_55=jsAppendChild(_54,E(_53)[1]),_56=[0,_54],_57=A(_52,[_56,_]);return _56;};},_58=function(_59){return _3F(0,E(_59)[1],_g);},_5a=[0,98],_5b=[1,_5a,_g],_5c=function(_5d,_5e){var _5f=new T(function(){return A(_5d,[_5e]);});return function(_5g,_){var _5h=jsCreateElem(toJSStr(_5b)),_5i=jsAppendChild(_5h,E(_5g)[1]),_5j=[0,_5h],_5k=A(_5f,[_5j,_]);return _5j;};},_5l=unCStr("br"),_5m=function(_5n,_){var _5o=jsCreateElem(toJSStr(E(_5l))),_5p=jsAppendChild(_5o,E(_5n)[1]);return [0,_5o];},_5q=[1,_A],_5r=unCStr("result: "),_5s=function(_5t){var _5u=new T(function(){return _5c(_4S,new T(function(){return _58(_5t);}));});return function(_5v,_){return [0,[0,function(_5w,_){var _5x=_5m(_5w,_),_5y=_4S(_5r,_5w,_),_5z=A(_5u,[_5w,_]);return _5w;},_5q],_5v];};},_5A=unCStr(" numbers and append the result using a fold"),_5B=[0,0],_5C=[1,_5B],_5D=[0,_2X,_5C],_5E=function(_5F,_){return [0,_5D,_5F];},_5G=function(_5H,_5I,_5J,_){var _5K=_3Y(_5H,_5J,_),_5L=A(_5I,[_5K,_]);return _5K;},_5M=unCStr("()"),_5N=unCStr("GHC.Tuple"),_5O=unCStr("ghc-prim"),_5P=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5O,_5N,_5M],_5Q=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5P,_g],_5R=function(_5S){return E(_5Q);},_5T=unCStr("haste-perch-0.1.0.1"),_5U=unCStr("Haste.Perch"),_5V=unCStr("PerchM"),_5W=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_5T,_5U,_5V],_5X=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_5W,_g],_5Y=function(_5Z){return E(_5X);},_60=function(_61){var _62=E(_61);return _62[0]==0?[0]:_1G(_62[1],new T(function(){return _60(_62[2]);}));},_63=function(_64,_65){var _66=E(_64);if(!_66){return [0,_g,_65];}else{var _67=E(_65);if(!_67[0]){return [0,_g,_g];}else{var _68=new T(function(){var _69=_63(_66-1|0,_67[2]);return [0,_69[1],_69[2]];});return [0,[1,_67[1],new T(function(){return E(E(_68)[1]);})],new T(function(){return E(E(_68)[2]);})];}}},_6a=[0,120],_6b=[0,48],_6c=function(_6d){var _6e=new T(function(){var _6f=_63(8,new T(function(){var _6g=md5(toJSStr(E(_6d)));return fromJSStr(_6g);}));return [0,_6f[1],_6f[2]];}),_6h=parseInt([0,toJSStr([1,_6b,[1,_6a,new T(function(){return E(E(_6e)[1]);})]])]),_6i=new T(function(){var _6j=_63(8,new T(function(){return E(E(_6e)[2]);}));return [0,_6j[1],_6j[2]];}),_6k=parseInt([0,toJSStr([1,_6b,[1,_6a,new T(function(){return E(E(_6i)[1]);})]])]),_6l=hs_mkWord64(_6h,_6k),_6m=parseInt([0,toJSStr([1,_6b,[1,_6a,new T(function(){return E(_63(8,new T(function(){return E(E(_6i)[2]);}))[1]);})]])]),_6n=hs_mkWord64(_6m,_6m);return [0,_6l,_6n];},_6o=function(_6p,_6q){var _6r=E(_6q);return _6r[0]==0?[0]:[1,new T(function(){return A(_6p,[_6r[1]]);}),new T(function(){return _6o(_6p,_6r[2]);})];},_6s=function(_6t,_6u){var _6v=jsShowI(_6t),_6w=md5(_6v);return _1G(fromJSStr(_6w),new T(function(){var _6x=jsShowI(_6u),_6y=md5(_6x);return fromJSStr(_6y);}));},_6z=function(_6A){var _6B=E(_6A);return _6s(_6B[1],_6B[2]);},_6C=function(_6D){var _6E=E(_6D);if(!_6E[0]){return [0];}else{var _6F=E(_6E[1]);return [1,[0,_6F[1],_6F[2]],new T(function(){return _6C(_6E[2]);})];}},_6G=unCStr("Prelude.undefined"),_6H=new T(function(){return err(_6G);}),_6I=function(_6J,_6K){return function(_6L){return E(new T(function(){var _6M=A(_6J,[_6H]),_6N=E(_6M[3]),_6O=_6N[1],_6P=_6N[2],_6Q=_1G(_6M[4],[1,new T(function(){return A(_6K,[_6H]);}),_g]);if(!_6Q[0]){return [0,_6O,_6P,_6N,_g];}else{var _6R=_6c(new T(function(){return _60(_6o(_6z,[1,[0,_6O,_6P],new T(function(){return _6C(_6Q);})]));}));return [0,_6R[1],_6R[2],_6N,_6Q];}}));};},_6S=new T(function(){return _6I(_5Y,_5R);}),_6T=unCStr("value"),_6U=unCStr("onclick"),_6V=unCStr("checked"),_6W=[0,_6V,_g],_6X=[1,_6W,_g],_6Y=unCStr("type"),_6Z=unCStr("input"),_70=function(_71,_){return _3Y(_6Z,_71,_);},_72=function(_73,_74,_75,_76,_77){var _78=new T(function(){var _79=new T(function(){return _Q(_70,[1,[0,_6Y,_74],[1,[0,_z,_73],[1,[0,_6T,_75],_g]]]);});return !E(_76)?E(_79):_Q(_79,_6X);}),_7a=E(_77);return _7a[0]==0?E(_78):_Q(_78,[1,[0,_6U,_7a[1]],_g]);},_7b=unCStr("href"),_7c=[0,97],_7d=[1,_7c,_g],_7e=function(_7f,_){return _3Y(_7d,_7f,_);},_7g=function(_7h,_7i){var _7j=new T(function(){return _Q(_7e,[1,[0,_7b,_7h],_g]);});return function(_7k,_){var _7l=A(_7j,[_7k,_]),_7m=A(_7i,[_7l,_]);return _7l;};},_7n=function(_7o){return _7g(_7o,function(_X,_){return _4S(_7o,_X,_);});},_7p=unCStr("option"),_7q=function(_7r,_){return _3Y(_7p,_7r,_);},_7s=unCStr("selected"),_7t=[0,_7s,_g],_7u=[1,_7t,_g],_7v=function(_7w,_7x,_7y){var _7z=new T(function(){return _Q(_7q,[1,[0,_6T,_7w],_g]);}),_7A=function(_7B,_){var _7C=A(_7z,[_7B,_]),_7D=A(_7x,[_7C,_]);return _7C;};return !E(_7y)?E(_7A):_Q(_7A,_7u);},_7E=function(_7F,_7G){return _7v(_7F,function(_X,_){return _4S(_7F,_X,_);},_7G);},_7H=unCStr("method"),_7I=unCStr("action"),_7J=unCStr("UTF-8"),_7K=unCStr("acceptCharset"),_7L=[0,_7K,_7J],_7M=unCStr("form"),_7N=function(_7O,_){return _3Y(_7M,_7O,_);},_7P=function(_7Q,_7R,_7S){var _7T=new T(function(){return _Q(_7N,[1,_7L,[1,[0,_7I,_7Q],[1,[0,_7H,_7R],_g]]]);});return function(_7U,_){var _7V=A(_7T,[_7U,_]),_7W=A(_7S,[_7V,_]);return _7V;};},_7X=unCStr("select"),_7Y=function(_7Z,_){return _3Y(_7X,_7Z,_);},_80=function(_81,_82){var _83=new T(function(){return _Q(_7Y,[1,[0,_z,_81],_g]);});return function(_84,_){var _85=A(_83,[_84,_]),_86=A(_82,[_85,_]);return _85;};},_87=unCStr("textarea"),_88=function(_89,_){return _3Y(_87,_89,_);},_8a=function(_8b,_8c){var _8d=new T(function(){return _Q(_88,[1,[0,_z,_8b],_g]);});return function(_8e,_){var _8f=A(_8d,[_8e,_]),_8g=_4S(_8c,_8f,_);return _8f;};},_8h=unCStr("color:red"),_8i=unCStr("style"),_8j=[0,_8i,_8h],_8k=[1,_8j,_g],_8l=[0,98],_8m=[1,_8l,_g],_8n=function(_8o){return _Q(function(_8p,_){var _8q=_3Y(_8m,_8p,_),_8r=A(_8o,[_8q,_]);return _8q;},_8k);},_8s=function(_8t,_8u,_){var _8v=E(_8t);if(!_8v[0]){return _8u;}else{var _8w=A(_8v[1],[_8u,_]),_8x=_8s(_8v[2],_8u,_);return _8u;}},_8y=function(_8z,_8A,_8B,_){var _8C=A(_8z,[_8B,_]),_8D=A(_8A,[_8B,_]);return _8B;},_8E=[0,_2X,_8y,_8s],_8F=[0,_8E,_6S,_4S,_4S,_5G,_8n,_7g,_7n,_72,_8a,_80,_7v,_7E,_7P,_Q],_8G=function(_8H,_8I,_){var _8J=A(_8I,[_]);return _8H;},_8K=function(_8L,_8M,_){var _8N=A(_8M,[_]);return new T(function(){return A(_8L,[_8N]);});},_8O=[0,_8K,_8G],_8P=function(_8Q){var _8R=E(_8Q);return _8R[0]==0?0:E(_8R[1])[1]+_8P(_8R[2])|0;},_8S=function(_8T){return [0,_8P(_8T)];},_8U=function(_8V,_8W){return [0,E(_8V)[1]+E(_8W)[1]|0];},_8X=[0,_5B,_8U,_8S],_8Y=function(_8Z,_90){var _91=E(_90);return _91[0]==0?[0]:[1,new T(function(){return A(_8Z,[_91[1]]);})];},_92=function(_93){return E(E(_93)[1]);},_94=function(_95){return E(E(_95)[2]);},_96=function(_97,_98,_99,_9a,_9b,_9c){var _9d=new T(function(){return _94(_97);});return A(_98,[new T(function(){return A(_9a,[_9c]);}),function(_9e){var _9f=E(_9e),_9g=E(_9f[1]);return A(_98,[new T(function(){return A(_9b,[_9f[2]]);}),function(_9h){var _9i=E(_9h),_9j=E(_9i[1]);return A(_99,[[0,[0,new T(function(){return A(_9d,[_9g[1],_9j[1]]);}),new T(function(){var _9k=E(_9g[2]);if(!_9k[0]){return [0];}else{var _9l=E(_9j[2]);return _9l[0]==0?[0]:[1,new T(function(){return A(_9k[1],[_9l[1]]);})];}})],_9i[2]]]);}]);}]);},_9m=function(_9n){return E(E(_9n)[1]);},_9o=function(_9p,_9q,_9r,_9s,_9t,_9u){var _9v=new T(function(){return _92(_9p);});return function(_9w){var _9x=E(_9q);return _96(_9v,_9x[1],_9x[3],function(_9y){return A(new T(function(){var _9z=new T(function(){return _94(_9s);});return A(_9m,[_9r,function(_9A){return [0,new T(function(){var _9B=E(E(_9A)[1]);return [0,_9B[1],new T(function(){return _8Y(_9z,_9B[2]);})];}),new T(function(){return E(E(_9A)[2]);})];}]);}),[new T(function(){return A(_9t,[_9y]);})]);},_9u,_9w);};},_9C=function(_9D,_9E){while(1){var _9F=(function(_9G,_9H){var _9I=E(_9H);if(!_9I[0]){return E(_9G);}else{_9D=new T(function(){return _9o(_8F,_2Z,_8O,_8X,_9G,_9I[1]);});_9E=_9I[2];return null;}})(_9D,_9E);if(_9F!=null){return _9F;}}},_9J=[13,coercionToken],_9K=unCStr("text"),_9L=[0,_2Z,_H],_9M=unCStr("base"),_9N=unCStr("Control.Exception.Base"),_9O=unCStr("PatternMatchFail"),_9P=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9M,_9N,_9O],_9Q=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9P,_g],_9R=function(_9S){return E(_9Q);},_9T=function(_9U){var _9V=E(_9U);return _1q(_1m(_9V[1]),_9R,_9V[2]);},_9W=function(_9X){return E(E(_9X)[1]);},_9Y=function(_9Z,_a0){return _1G(E(_9Z)[1],_a0);},_a1=function(_a2,_a3){return _2w(_9Y,_a2,_a3);},_a4=function(_a5,_a6,_a7){return _1G(E(_a6)[1],_a7);},_a8=[0,_a4,_9W,_a1],_a9=new T(function(){return [0,_9R,_a8,_aa,_9T];}),_aa=function(_ab){return [0,_a9,_ab];},_ac=unCStr("Non-exhaustive patterns in"),_ad=function(_ae,_af){return die(new T(function(){return A(_af,[_ae]);}));},_ag=function(_ah,_ai){var _aj=E(_ai);if(!_aj[0]){return [0,_g,_g];}else{var _ak=_aj[1];if(!A(_ah,[_ak])){return [0,_g,_aj];}else{var _al=new T(function(){var _am=_ag(_ah,_aj[2]);return [0,_am[1],_am[2]];});return [0,[1,_ak,new T(function(){return E(E(_al)[1]);})],new T(function(){return E(E(_al)[2]);})];}}},_an=[0,32],_ao=[0,10],_ap=[1,_ao,_g],_aq=function(_ar){return E(E(_ar)[1])==124?false:true;},_as=function(_at,_au){var _av=_ag(_aq,unCStr(_at)),_aw=_av[1],_ax=function(_ay,_az){return _1G(_ay,new T(function(){return unAppCStr(": ",new T(function(){return _1G(_au,new T(function(){return _1G(_az,_ap);}));}));}));},_aA=E(_av[2]);return _aA[0]==0?_ax(_aw,_g):E(E(_aA[1])[1])==124?_ax(_aw,[1,_an,_aA[2]]):_ax(_aw,_g);},_aB=function(_aC){return _ad([0,new T(function(){return _as(_aC,_ac);})],_aa);},_aD=new T(function(){return _aB("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_aE=function(_aF,_aG){while(1){var _aH=(function(_aI,_aJ){var _aK=E(_aI);switch(_aK[0]){case 0:var _aL=E(_aJ);if(!_aL[0]){return [0];}else{_aF=A(_aK[1],[_aL[1]]);_aG=_aL[2];return null;}break;case 1:var _aM=A(_aK[1],[_aJ]),_aN=_aJ;_aF=_aM;_aG=_aN;return null;case 2:return [0];case 3:return [1,[0,_aK[1],_aJ],new T(function(){return _aE(_aK[2],_aJ);})];default:return E(_aK[1]);}})(_aF,_aG);if(_aH!=null){return _aH;}}},_aO=function(_aP,_aQ){var _aR=new T(function(){var _aS=E(_aQ);if(_aS[0]==3){return [3,_aS[1],new T(function(){return _aO(_aP,_aS[2]);})];}else{var _aT=E(_aP);if(_aT[0]==2){return E(_aS);}else{var _aU=E(_aS);if(_aU[0]==2){return E(_aT);}else{var _aV=new T(function(){var _aW=E(_aU);if(_aW[0]==4){return [1,function(_aX){return [4,new T(function(){return _1G(_aE(_aT,_aX),_aW[1]);})];}];}else{var _aY=E(_aT);if(_aY[0]==1){var _aZ=_aY[1],_b0=E(_aW);return _b0[0]==0?[1,function(_b1){return _aO(A(_aZ,[_b1]),_b0);}]:[1,function(_b2){return _aO(A(_aZ,[_b2]),new T(function(){return A(_b0[1],[_b2]);}));}];}else{var _b3=E(_aW);return _b3[0]==0?E(_aD):[1,function(_b4){return _aO(_aY,new T(function(){return A(_b3[1],[_b4]);}));}];}}}),_b5=E(_aT);switch(_b5[0]){case 1:var _b6=E(_aU);return _b6[0]==4?[1,function(_b7){return [4,new T(function(){return _1G(_aE(A(_b5[1],[_b7]),_b7),_b6[1]);})];}]:E(_aV);case 4:var _b8=_b5[1],_b9=E(_aU);switch(_b9[0]){case 0:return [1,function(_ba){return [4,new T(function(){return _1G(_b8,new T(function(){return _aE(_b9,_ba);}));})];}];case 1:return [1,function(_bb){return [4,new T(function(){return _1G(_b8,new T(function(){return _aE(A(_b9[1],[_bb]),_bb);}));})];}];default:return [4,new T(function(){return _1G(_b8,_b9[1]);})];}break;default:return E(_aV);}}}}}),_bc=E(_aP);switch(_bc[0]){case 0:var _bd=E(_aQ);return _bd[0]==0?[0,function(_be){return _aO(A(_bc[1],[_be]),new T(function(){return A(_bd[1],[_be]);}));}]:E(_aR);case 3:return [3,_bc[1],new T(function(){return _aO(_bc[2],_aQ);})];default:return E(_aR);}},_bf=function(_bg,_bh){return E(_bg)[1]!=E(_bh)[1];},_bi=function(_bj,_bk){return E(_bj)[1]==E(_bk)[1];},_bl=[0,_bi,_bf],_bm=function(_bn){return E(E(_bn)[1]);},_bo=function(_bp,_bq,_br){while(1){var _bs=E(_bq);if(!_bs[0]){return E(_br)[0]==0?true:false;}else{var _bt=E(_br);if(!_bt[0]){return false;}else{if(!A(_bm,[_bp,_bs[1],_bt[1]])){return false;}else{_bq=_bs[2];_br=_bt[2];continue;}}}}},_bu=function(_bv,_bw,_bx){return !_bo(_bv,_bw,_bx)?true:false;},_by=function(_bz){return [0,function(_bA,_bB){return _bo(_bz,_bA,_bB);},function(_bA,_bB){return _bu(_bz,_bA,_bB);}];},_bC=new T(function(){return _by(_bl);}),_bD=function(_bE,_bF){var _bG=E(_bE);switch(_bG[0]){case 0:return [0,function(_bH){return _bD(A(_bG[1],[_bH]),_bF);}];case 1:return [1,function(_bI){return _bD(A(_bG[1],[_bI]),_bF);}];case 2:return [2];case 3:return _aO(A(_bF,[_bG[1]]),new T(function(){return _bD(_bG[2],_bF);}));default:var _bJ=function(_bK){var _bL=E(_bK);if(!_bL[0]){return [0];}else{var _bM=E(_bL[1]);return _1G(_aE(A(_bF,[_bM[1]]),_bM[2]),new T(function(){return _bJ(_bL[2]);}));}},_bN=_bJ(_bG[1]);return _bN[0]==0?[2]:[4,_bN];}},_bO=[2],_bP=function(_bQ){return [3,_bQ,_bO];},_bR=function(_bS,_bT){var _bU=E(_bS);if(!_bU){return A(_bT,[_A]);}else{var _bV=new T(function(){return _bR(_bU-1|0,_bT);});return [0,function(_bW){return E(_bV);}];}},_bX=function(_bY,_bZ,_c0){var _c1=new T(function(){return A(_bY,[_bP]);});return [1,function(_c2){return A(function(_c3,_c4,_c5){while(1){var _c6=(function(_c7,_c8,_c9){var _ca=E(_c7);switch(_ca[0]){case 0:var _cb=E(_c8);if(!_cb[0]){return E(_bZ);}else{_c3=A(_ca[1],[_cb[1]]);_c4=_cb[2];var _cc=_c9+1|0;_c5=_cc;return null;}break;case 1:var _cd=A(_ca[1],[_c8]),_ce=_c8,_cc=_c9;_c3=_cd;_c4=_ce;_c5=_cc;return null;case 2:return E(_bZ);case 3:return function(_cf){var _cg=new T(function(){return _bD(_ca,_cf);});return _bR(_c9,function(_ch){return E(_cg);});};default:return function(_ci){return _bD(_ca,_ci);};}})(_c3,_c4,_c5);if(_c6!=null){return _c6;}}},[_c1,_c2,0,_c0]);}];},_cj=[6],_ck=unCStr("valDig: Bad base"),_cl=new T(function(){return err(_ck);}),_cm=function(_cn,_co){var _cp=function(_cq,_cr){var _cs=E(_cq);if(!_cs[0]){var _ct=new T(function(){return A(_cr,[_g]);});return function(_cu){return A(_cu,[_ct]);};}else{var _cv=E(_cs[1])[1],_cw=function(_cx){var _cy=new T(function(){return _cp(_cs[2],function(_cz){return A(_cr,[[1,_cx,_cz]]);});});return function(_cA){var _cB=new T(function(){return A(_cy,[_cA]);});return [0,function(_cC){return E(_cB);}];};};switch(E(E(_cn)[1])){case 8:if(48>_cv){var _cD=new T(function(){return A(_cr,[_g]);});return function(_cE){return A(_cE,[_cD]);};}else{if(_cv>55){var _cF=new T(function(){return A(_cr,[_g]);});return function(_cG){return A(_cG,[_cF]);};}else{return _cw([0,_cv-48|0]);}}break;case 10:if(48>_cv){var _cH=new T(function(){return A(_cr,[_g]);});return function(_cI){return A(_cI,[_cH]);};}else{if(_cv>57){var _cJ=new T(function(){return A(_cr,[_g]);});return function(_cK){return A(_cK,[_cJ]);};}else{return _cw([0,_cv-48|0]);}}break;case 16:var _cL=new T(function(){return 97>_cv?65>_cv?[0]:_cv>70?[0]:[1,[0,(_cv-65|0)+10|0]]:_cv>102?65>_cv?[0]:_cv>70?[0]:[1,[0,(_cv-65|0)+10|0]]:[1,[0,(_cv-97|0)+10|0]];});if(48>_cv){var _cM=E(_cL);if(!_cM[0]){var _cN=new T(function(){return A(_cr,[_g]);});return function(_cO){return A(_cO,[_cN]);};}else{return _cw(_cM[1]);}}else{if(_cv>57){var _cP=E(_cL);if(!_cP[0]){var _cQ=new T(function(){return A(_cr,[_g]);});return function(_cR){return A(_cR,[_cQ]);};}else{return _cw(_cP[1]);}}else{return _cw([0,_cv-48|0]);}}break;default:return E(_cl);}}};return [1,function(_cS){return A(_cp,[_cS,_H,function(_cT){var _cU=E(_cT);return _cU[0]==0?[2]:A(_co,[_cU]);}]);}];},_cV=[0,10],_cW=[0,1],_cX=[0,2147483647],_cY=function(_cZ,_d0){while(1){var _d1=E(_cZ);if(!_d1[0]){var _d2=_d1[1],_d3=E(_d0);if(!_d3[0]){var _d4=_d3[1],_d5=addC(_d2,_d4);if(!E(_d5[2])){return [0,_d5[1]];}else{_cZ=[1,I_fromInt(_d2)];_d0=[1,I_fromInt(_d4)];continue;}}else{_cZ=[1,I_fromInt(_d2)];_d0=_d3;continue;}}else{var _d6=E(_d0);if(!_d6[0]){_cZ=_d1;_d0=[1,I_fromInt(_d6[1])];continue;}else{return [1,I_add(_d1[1],_d6[1])];}}}},_d7=new T(function(){return _cY(_cX,_cW);}),_d8=function(_d9){var _da=E(_d9);if(!_da[0]){var _db=E(_da[1]);return _db==(-2147483648)?E(_d7):[0, -_db];}else{return [1,I_negate(_da[1])];}},_dc=[0,10],_dd=[0,0],_de=function(_df,_dg){while(1){var _dh=E(_df);if(!_dh[0]){var _di=_dh[1],_dj=E(_dg);if(!_dj[0]){var _dk=_dj[1];if(!(imul(_di,_dk)|0)){return [0,imul(_di,_dk)|0];}else{_df=[1,I_fromInt(_di)];_dg=[1,I_fromInt(_dk)];continue;}}else{_df=[1,I_fromInt(_di)];_dg=_dj;continue;}}else{var _dl=E(_dg);if(!_dl[0]){_df=_dh;_dg=[1,I_fromInt(_dl[1])];continue;}else{return [1,I_mul(_dh[1],_dl[1])];}}}},_dm=function(_dn,_do,_dp){while(1){var _dq=E(_dp);if(!_dq[0]){return E(_do);}else{var _dr=_cY(_de(_do,_dn),_dq[1]);_dp=_dq[2];_do=_dr;continue;}}},_ds=function(_dt){var _du=new T(function(){return _aO(_aO([0,function(_dv){return E(E(_dv)[1])==45?_cm(_cV,function(_dw){return A(_dt,[[1,new T(function(){return _d8(_dm(_dc,_dd,_dw));})]]);}):[2];}],[0,function(_dx){return E(E(_dx)[1])==43?_cm(_cV,function(_dy){return A(_dt,[[1,new T(function(){return _dm(_dc,_dd,_dy);})]]);}):[2];}]),new T(function(){return _cm(_cV,function(_dz){return A(_dt,[[1,new T(function(){return _dm(_dc,_dd,_dz);})]]);});}));});return _aO([0,function(_dA){return E(E(_dA)[1])==101?E(_du):[2];}],[0,function(_dB){return E(E(_dB)[1])==69?E(_du):[2];}]);},_dC=function(_dD){return A(_dD,[_9]);},_dE=function(_dF){return A(_dF,[_9]);},_dG=function(_dH){var _dI=new T(function(){return _cm(_cV,function(_dJ){return A(_dH,[[1,_dJ]]);});});return [0,function(_dK){return E(E(_dK)[1])==46?E(_dI):[2];}];},_dL=function(_dM){return _cm(_cV,function(_dN){return _bX(_dG,_dC,function(_dO){return _bX(_ds,_dE,function(_dP){return A(_dM,[[5,[1,_dN,_dO,_dP]]]);});});});},_dQ=function(_dR,_dS,_dT){while(1){var _dU=E(_dT);if(!_dU[0]){return false;}else{if(!A(_bm,[_dR,_dS,_dU[1]])){_dT=_dU[2];continue;}else{return true;}}}},_dV=unCStr("!@#$%&*+./<=>?\\^|:-~"),_dW=function(_dX){return _dQ(_bl,_dX,_dV);},_dY=[0,8],_dZ=[0,16],_e0=function(_e1){var _e2=new T(function(){return _cm(_dZ,function(_e3){return A(_e1,[[5,[0,_dZ,_e3]]]);});}),_e4=new T(function(){return _cm(_dY,function(_e5){return A(_e1,[[5,[0,_dY,_e5]]]);});}),_e6=new T(function(){return _cm(_dZ,function(_e7){return A(_e1,[[5,[0,_dZ,_e7]]]);});}),_e8=new T(function(){return _cm(_dY,function(_e9){return A(_e1,[[5,[0,_dY,_e9]]]);});});return [0,function(_ea){return E(E(_ea)[1])==48?E([0,function(_eb){switch(E(E(_eb)[1])){case 79:return E(_e8);case 88:return E(_e6);case 111:return E(_e4);case 120:return E(_e2);default:return [2];}}]):[2];}];},_ec=true,_ed=function(_ee){var _ef=new T(function(){return A(_ee,[_dZ]);}),_eg=new T(function(){return A(_ee,[_dY]);}),_eh=new T(function(){return A(_ee,[_dZ]);}),_ei=new T(function(){return A(_ee,[_dY]);});return [0,function(_ej){switch(E(E(_ej)[1])){case 79:return E(_ei);case 88:return E(_eh);case 111:return E(_eg);case 120:return E(_ef);default:return [2];}}];},_ek=function(_el){return A(_el,[_cV]);},_em=function(_en){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _3F(9,_en,_g);})));},_eo=function(_ep){var _eq=E(_ep);return _eq[0]==0?E(_eq[1]):I_toInt(_eq[1]);},_er=function(_es,_et){var _eu=E(_es);if(!_eu[0]){var _ev=_eu[1],_ew=E(_et);return _ew[0]==0?_ev<=_ew[1]:I_compareInt(_ew[1],_ev)>=0;}else{var _ex=_eu[1],_ey=E(_et);return _ey[0]==0?I_compareInt(_ex,_ey[1])<=0:I_compare(_ex,_ey[1])<=0;}},_ez=function(_eA){return [2];},_eB=function(_eC){var _eD=E(_eC);if(!_eD[0]){return E(_ez);}else{var _eE=_eD[1],_eF=E(_eD[2]);if(!_eF[0]){return E(_eE);}else{var _eG=new T(function(){return _eB(_eF);});return function(_eH){return _aO(A(_eE,[_eH]),new T(function(){return A(_eG,[_eH]);}));};}}},_eI=unCStr("NUL"),_eJ=function(_eK){return [2];},_eL=function(_eM){return _eJ(_eM);},_eN=function(_eO,_eP){var _eQ=function(_eR,_eS){var _eT=E(_eR);if(!_eT[0]){return function(_eU){return A(_eU,[_eO]);};}else{var _eV=E(_eS);if(!_eV[0]){return E(_eJ);}else{if(E(_eT[1])[1]!=E(_eV[1])[1]){return E(_eL);}else{var _eW=new T(function(){return _eQ(_eT[2],_eV[2]);});return function(_eX){var _eY=new T(function(){return A(_eW,[_eX]);});return [0,function(_eZ){return E(_eY);}];};}}}};return [1,function(_f0){return A(_eQ,[_eO,_f0,_eP]);}];},_f1=[0,0],_f2=function(_f3){var _f4=new T(function(){return A(_f3,[_f1]);});return _eN(_eI,function(_f5){return E(_f4);});},_f6=unCStr("STX"),_f7=[0,2],_f8=function(_f9){var _fa=new T(function(){return A(_f9,[_f7]);});return _eN(_f6,function(_fb){return E(_fa);});},_fc=unCStr("ETX"),_fd=[0,3],_fe=function(_ff){var _fg=new T(function(){return A(_ff,[_fd]);});return _eN(_fc,function(_fh){return E(_fg);});},_fi=unCStr("EOT"),_fj=[0,4],_fk=function(_fl){var _fm=new T(function(){return A(_fl,[_fj]);});return _eN(_fi,function(_fn){return E(_fm);});},_fo=unCStr("ENQ"),_fp=[0,5],_fq=function(_fr){var _fs=new T(function(){return A(_fr,[_fp]);});return _eN(_fo,function(_ft){return E(_fs);});},_fu=unCStr("ACK"),_fv=[0,6],_fw=function(_fx){var _fy=new T(function(){return A(_fx,[_fv]);});return _eN(_fu,function(_fz){return E(_fy);});},_fA=unCStr("BEL"),_fB=[0,7],_fC=function(_fD){var _fE=new T(function(){return A(_fD,[_fB]);});return _eN(_fA,function(_fF){return E(_fE);});},_fG=unCStr("BS"),_fH=[0,8],_fI=function(_fJ){var _fK=new T(function(){return A(_fJ,[_fH]);});return _eN(_fG,function(_fL){return E(_fK);});},_fM=unCStr("HT"),_fN=[0,9],_fO=function(_fP){var _fQ=new T(function(){return A(_fP,[_fN]);});return _eN(_fM,function(_fR){return E(_fQ);});},_fS=unCStr("LF"),_fT=[0,10],_fU=function(_fV){var _fW=new T(function(){return A(_fV,[_fT]);});return _eN(_fS,function(_fX){return E(_fW);});},_fY=unCStr("VT"),_fZ=[0,11],_g0=function(_g1){var _g2=new T(function(){return A(_g1,[_fZ]);});return _eN(_fY,function(_g3){return E(_g2);});},_g4=unCStr("FF"),_g5=[0,12],_g6=function(_g7){var _g8=new T(function(){return A(_g7,[_g5]);});return _eN(_g4,function(_g9){return E(_g8);});},_ga=unCStr("CR"),_gb=[0,13],_gc=function(_gd){var _ge=new T(function(){return A(_gd,[_gb]);});return _eN(_ga,function(_gf){return E(_ge);});},_gg=unCStr("SI"),_gh=[0,15],_gi=function(_gj){var _gk=new T(function(){return A(_gj,[_gh]);});return _eN(_gg,function(_gl){return E(_gk);});},_gm=unCStr("DLE"),_gn=[0,16],_go=function(_gp){var _gq=new T(function(){return A(_gp,[_gn]);});return _eN(_gm,function(_gr){return E(_gq);});},_gs=unCStr("DC1"),_gt=[0,17],_gu=function(_gv){var _gw=new T(function(){return A(_gv,[_gt]);});return _eN(_gs,function(_gx){return E(_gw);});},_gy=unCStr("DC2"),_gz=[0,18],_gA=function(_gB){var _gC=new T(function(){return A(_gB,[_gz]);});return _eN(_gy,function(_gD){return E(_gC);});},_gE=unCStr("DC3"),_gF=[0,19],_gG=function(_gH){var _gI=new T(function(){return A(_gH,[_gF]);});return _eN(_gE,function(_gJ){return E(_gI);});},_gK=unCStr("DC4"),_gL=[0,20],_gM=function(_gN){var _gO=new T(function(){return A(_gN,[_gL]);});return _eN(_gK,function(_gP){return E(_gO);});},_gQ=unCStr("NAK"),_gR=[0,21],_gS=function(_gT){var _gU=new T(function(){return A(_gT,[_gR]);});return _eN(_gQ,function(_gV){return E(_gU);});},_gW=unCStr("SYN"),_gX=[0,22],_gY=function(_gZ){var _h0=new T(function(){return A(_gZ,[_gX]);});return _eN(_gW,function(_h1){return E(_h0);});},_h2=unCStr("ETB"),_h3=[0,23],_h4=function(_h5){var _h6=new T(function(){return A(_h5,[_h3]);});return _eN(_h2,function(_h7){return E(_h6);});},_h8=unCStr("CAN"),_h9=[0,24],_ha=function(_hb){var _hc=new T(function(){return A(_hb,[_h9]);});return _eN(_h8,function(_hd){return E(_hc);});},_he=unCStr("EM"),_hf=[0,25],_hg=function(_hh){var _hi=new T(function(){return A(_hh,[_hf]);});return _eN(_he,function(_hj){return E(_hi);});},_hk=unCStr("SUB"),_hl=[0,26],_hm=function(_hn){var _ho=new T(function(){return A(_hn,[_hl]);});return _eN(_hk,function(_hp){return E(_ho);});},_hq=unCStr("ESC"),_hr=[0,27],_hs=function(_ht){var _hu=new T(function(){return A(_ht,[_hr]);});return _eN(_hq,function(_hv){return E(_hu);});},_hw=unCStr("FS"),_hx=[0,28],_hy=function(_hz){var _hA=new T(function(){return A(_hz,[_hx]);});return _eN(_hw,function(_hB){return E(_hA);});},_hC=unCStr("GS"),_hD=[0,29],_hE=function(_hF){var _hG=new T(function(){return A(_hF,[_hD]);});return _eN(_hC,function(_hH){return E(_hG);});},_hI=unCStr("RS"),_hJ=[0,30],_hK=function(_hL){var _hM=new T(function(){return A(_hL,[_hJ]);});return _eN(_hI,function(_hN){return E(_hM);});},_hO=unCStr("US"),_hP=[0,31],_hQ=function(_hR){var _hS=new T(function(){return A(_hR,[_hP]);});return _eN(_hO,function(_hT){return E(_hS);});},_hU=unCStr("SP"),_hV=[0,32],_hW=function(_hX){var _hY=new T(function(){return A(_hX,[_hV]);});return _eN(_hU,function(_hZ){return E(_hY);});},_i0=unCStr("DEL"),_i1=[0,127],_i2=function(_i3){var _i4=new T(function(){return A(_i3,[_i1]);});return _eN(_i0,function(_i5){return E(_i4);});},_i6=[1,_i2,_g],_i7=[1,_hW,_i6],_i8=[1,_hQ,_i7],_i9=[1,_hK,_i8],_ia=[1,_hE,_i9],_ib=[1,_hy,_ia],_ic=[1,_hs,_ib],_id=[1,_hm,_ic],_ie=[1,_hg,_id],_if=[1,_ha,_ie],_ig=[1,_h4,_if],_ih=[1,_gY,_ig],_ii=[1,_gS,_ih],_ij=[1,_gM,_ii],_ik=[1,_gG,_ij],_il=[1,_gA,_ik],_im=[1,_gu,_il],_in=[1,_go,_im],_io=[1,_gi,_in],_ip=[1,_gc,_io],_iq=[1,_g6,_ip],_ir=[1,_g0,_iq],_is=[1,_fU,_ir],_it=[1,_fO,_is],_iu=[1,_fI,_it],_iv=[1,_fC,_iu],_iw=[1,_fw,_iv],_ix=[1,_fq,_iw],_iy=[1,_fk,_ix],_iz=[1,_fe,_iy],_iA=[1,_f8,_iz],_iB=[1,_f2,_iA],_iC=unCStr("SOH"),_iD=[0,1],_iE=function(_iF){var _iG=new T(function(){return A(_iF,[_iD]);});return _eN(_iC,function(_iH){return E(_iG);});},_iI=unCStr("SO"),_iJ=[0,14],_iK=function(_iL){var _iM=new T(function(){return A(_iL,[_iJ]);});return _eN(_iI,function(_iN){return E(_iM);});},_iO=function(_iP){return _bX(_iE,_iK,_iP);},_iQ=[1,_iO,_iB],_iR=new T(function(){return _eB(_iQ);}),_iS=[0,1114111],_iT=[0,34],_iU=[0,_iT,_ec],_iV=[0,39],_iW=[0,_iV,_ec],_iX=[0,92],_iY=[0,_iX,_ec],_iZ=[0,_fB,_ec],_j0=[0,_fH,_ec],_j1=[0,_g5,_ec],_j2=[0,_fT,_ec],_j3=[0,_gb,_ec],_j4=[0,_fN,_ec],_j5=[0,_fZ,_ec],_j6=[0,_f1,_ec],_j7=[0,_iD,_ec],_j8=[0,_f7,_ec],_j9=[0,_fd,_ec],_ja=[0,_fj,_ec],_jb=[0,_fp,_ec],_jc=[0,_fv,_ec],_jd=[0,_fB,_ec],_je=[0,_fH,_ec],_jf=[0,_fN,_ec],_jg=[0,_fT,_ec],_jh=[0,_fZ,_ec],_ji=[0,_g5,_ec],_jj=[0,_gb,_ec],_jk=[0,_iJ,_ec],_jl=[0,_gh,_ec],_jm=[0,_gn,_ec],_jn=[0,_gt,_ec],_jo=[0,_gz,_ec],_jp=[0,_gF,_ec],_jq=[0,_gL,_ec],_jr=[0,_gR,_ec],_js=[0,_gX,_ec],_jt=[0,_h3,_ec],_ju=[0,_h9,_ec],_jv=[0,_hf,_ec],_jw=[0,_hl,_ec],_jx=[0,_hr,_ec],_jy=[0,_hx,_ec],_jz=[0,_hD,_ec],_jA=[0,_hJ,_ec],_jB=[0,_hP,_ec],_jC=function(_jD){return [0,_jD];},_jE=function(_jF){var _jG=new T(function(){return A(_jF,[_j5]);}),_jH=new T(function(){return A(_jF,[_j4]);}),_jI=new T(function(){return A(_jF,[_j3]);}),_jJ=new T(function(){return A(_jF,[_j2]);}),_jK=new T(function(){return A(_jF,[_j1]);}),_jL=new T(function(){return A(_jF,[_j0]);}),_jM=new T(function(){return A(_jF,[_iZ]);}),_jN=new T(function(){return A(_jF,[_iY]);}),_jO=new T(function(){return A(_jF,[_iW]);}),_jP=new T(function(){return A(_jF,[_iU]);});return _aO([0,function(_jQ){switch(E(E(_jQ)[1])){case 34:return E(_jP);case 39:return E(_jO);case 92:return E(_jN);case 97:return E(_jM);case 98:return E(_jL);case 102:return E(_jK);case 110:return E(_jJ);case 114:return E(_jI);case 116:return E(_jH);case 118:return E(_jG);default:return [2];}}],new T(function(){return _aO(_bX(_ed,_ek,function(_jR){var _jS=new T(function(){return _jC(E(_jR)[1]);});return _cm(_jR,function(_jT){var _jU=_dm(_jS,_dd,_jT);return !_er(_jU,_iS)?[2]:A(_jF,[[0,new T(function(){var _jV=_eo(_jU);return _jV>>>0>1114111?_em(_jV):[0,_jV];}),_ec]]);});}),new T(function(){var _jW=new T(function(){return A(_jF,[_jB]);}),_jX=new T(function(){return A(_jF,[_jA]);}),_jY=new T(function(){return A(_jF,[_jz]);}),_jZ=new T(function(){return A(_jF,[_jy]);}),_k0=new T(function(){return A(_jF,[_jx]);}),_k1=new T(function(){return A(_jF,[_jw]);}),_k2=new T(function(){return A(_jF,[_jv]);}),_k3=new T(function(){return A(_jF,[_ju]);}),_k4=new T(function(){return A(_jF,[_jt]);}),_k5=new T(function(){return A(_jF,[_js]);}),_k6=new T(function(){return A(_jF,[_jr]);}),_k7=new T(function(){return A(_jF,[_jq]);}),_k8=new T(function(){return A(_jF,[_jp]);}),_k9=new T(function(){return A(_jF,[_jo]);}),_ka=new T(function(){return A(_jF,[_jn]);}),_kb=new T(function(){return A(_jF,[_jm]);}),_kc=new T(function(){return A(_jF,[_jl]);}),_kd=new T(function(){return A(_jF,[_jk]);}),_ke=new T(function(){return A(_jF,[_jj]);}),_kf=new T(function(){return A(_jF,[_ji]);}),_kg=new T(function(){return A(_jF,[_jh]);}),_kh=new T(function(){return A(_jF,[_jg]);}),_ki=new T(function(){return A(_jF,[_jf]);}),_kj=new T(function(){return A(_jF,[_je]);}),_kk=new T(function(){return A(_jF,[_jd]);}),_kl=new T(function(){return A(_jF,[_jc]);}),_km=new T(function(){return A(_jF,[_jb]);}),_kn=new T(function(){return A(_jF,[_ja]);}),_ko=new T(function(){return A(_jF,[_j9]);}),_kp=new T(function(){return A(_jF,[_j8]);}),_kq=new T(function(){return A(_jF,[_j7]);}),_kr=new T(function(){return A(_jF,[_j6]);});return _aO([0,function(_ks){return E(E(_ks)[1])==94?E([0,function(_kt){switch(E(E(_kt)[1])){case 64:return E(_kr);case 65:return E(_kq);case 66:return E(_kp);case 67:return E(_ko);case 68:return E(_kn);case 69:return E(_km);case 70:return E(_kl);case 71:return E(_kk);case 72:return E(_kj);case 73:return E(_ki);case 74:return E(_kh);case 75:return E(_kg);case 76:return E(_kf);case 77:return E(_ke);case 78:return E(_kd);case 79:return E(_kc);case 80:return E(_kb);case 81:return E(_ka);case 82:return E(_k9);case 83:return E(_k8);case 84:return E(_k7);case 85:return E(_k6);case 86:return E(_k5);case 87:return E(_k4);case 88:return E(_k3);case 89:return E(_k2);case 90:return E(_k1);case 91:return E(_k0);case 92:return E(_jZ);case 93:return E(_jY);case 94:return E(_jX);case 95:return E(_jW);default:return [2];}}]):[2];}],new T(function(){return A(_iR,[function(_ku){return A(_jF,[[0,_ku,_ec]]);}]);}));}));}));},_kv=function(_kw){return A(_kw,[_A]);},_kx=function(_ky){var _kz=E(_ky);if(!_kz[0]){return E(_kv);}else{var _kA=_kz[2],_kB=E(E(_kz[1])[1]);switch(_kB){case 9:var _kC=new T(function(){return _kx(_kA);});return function(_kD){var _kE=new T(function(){return A(_kC,[_kD]);});return [0,function(_kF){return E(_kE);}];};case 10:var _kG=new T(function(){return _kx(_kA);});return function(_kH){var _kI=new T(function(){return A(_kG,[_kH]);});return [0,function(_kJ){return E(_kI);}];};case 11:var _kK=new T(function(){return _kx(_kA);});return function(_kL){var _kM=new T(function(){return A(_kK,[_kL]);});return [0,function(_kN){return E(_kM);}];};case 12:var _kO=new T(function(){return _kx(_kA);});return function(_kP){var _kQ=new T(function(){return A(_kO,[_kP]);});return [0,function(_kR){return E(_kQ);}];};case 13:var _kS=new T(function(){return _kx(_kA);});return function(_kT){var _kU=new T(function(){return A(_kS,[_kT]);});return [0,function(_kV){return E(_kU);}];};case 32:var _kW=new T(function(){return _kx(_kA);});return function(_kX){var _kY=new T(function(){return A(_kW,[_kX]);});return [0,function(_kZ){return E(_kY);}];};case 160:var _l0=new T(function(){return _kx(_kA);});return function(_l1){var _l2=new T(function(){return A(_l0,[_l1]);});return [0,function(_l3){return E(_l2);}];};default:var _l4=u_iswspace(_kB);if(!E(_l4)){return E(_kv);}else{var _l5=new T(function(){return _kx(_kA);});return function(_l6){var _l7=new T(function(){return A(_l5,[_l6]);});return [0,function(_l8){return E(_l7);}];};}}}},_l9=function(_la){var _lb=new T(function(){return _jE(_la);}),_lc=new T(function(){return _l9(_la);}),_ld=[1,function(_le){return A(_kx,[_le,function(_lf){return E([0,function(_lg){return E(E(_lg)[1])==92?E(_lc):[2];}]);}]);}];return _aO([0,function(_lh){return E(E(_lh)[1])==92?E([0,function(_li){var _lj=E(E(_li)[1]);switch(_lj){case 9:return E(_ld);case 10:return E(_ld);case 11:return E(_ld);case 12:return E(_ld);case 13:return E(_ld);case 32:return E(_ld);case 38:return E(_lc);case 160:return E(_ld);default:var _lk=u_iswspace(_lj);return E(_lk)==0?[2]:E(_ld);}}]):[2];}],[0,function(_ll){var _lm=E(_ll);return E(_lm[1])==92?E(_lb):A(_la,[[0,_lm,_0]]);}]);},_ln=function(_lo,_lp){var _lq=new T(function(){return A(_lp,[[1,new T(function(){return A(_lo,[_g]);})]]);});return _l9(function(_lr){var _ls=E(_lr),_lt=E(_ls[1]);return E(_lt[1])==34?!E(_ls[2])?E(_lq):_ln(function(_lu){return A(_lo,[[1,_lt,_lu]]);},_lp):_ln(function(_lv){return A(_lo,[[1,_lt,_lv]]);},_lp);});},_lw=unCStr("_\'"),_lx=function(_ly){var _lz=u_iswalnum(_ly);return E(_lz)==0?_dQ(_bl,[0,_ly],_lw):true;},_lA=function(_lB){return _lx(E(_lB)[1]);},_lC=unCStr(",;()[]{}`"),_lD=function(_lE){return A(_lE,[_g]);},_lF=function(_lG,_lH){var _lI=function(_lJ){var _lK=E(_lJ);if(!_lK[0]){return E(_lD);}else{var _lL=_lK[1];if(!A(_lG,[_lL])){return E(_lD);}else{var _lM=new T(function(){return _lI(_lK[2]);});return function(_lN){var _lO=new T(function(){return A(_lM,[function(_lP){return A(_lN,[[1,_lL,_lP]]);}]);});return [0,function(_lQ){return E(_lO);}];};}}};return [1,function(_lR){return A(_lI,[_lR,_lH]);}];},_lS=unCStr(".."),_lT=unCStr("::"),_lU=unCStr("->"),_lV=[0,64],_lW=[1,_lV,_g],_lX=[0,126],_lY=[1,_lX,_g],_lZ=unCStr("=>"),_m0=[1,_lZ,_g],_m1=[1,_lY,_m0],_m2=[1,_lW,_m1],_m3=[1,_lU,_m2],_m4=unCStr("<-"),_m5=[1,_m4,_m3],_m6=[0,124],_m7=[1,_m6,_g],_m8=[1,_m7,_m5],_m9=[1,_iX,_g],_ma=[1,_m9,_m8],_mb=[0,61],_mc=[1,_mb,_g],_md=[1,_mc,_ma],_me=[1,_lT,_md],_mf=[1,_lS,_me],_mg=function(_mh){var _mi=new T(function(){return A(_mh,[_cj]);});return _aO([1,function(_mj){return E(_mj)[0]==0?E(_mi):[2];}],new T(function(){var _mk=new T(function(){return _jE(function(_ml){var _mm=E(_ml);return (function(_mn,_mo){var _mp=new T(function(){return A(_mh,[[0,_mn]]);});return !E(_mo)?E(E(_mn)[1])==39?[2]:[0,function(_mq){return E(E(_mq)[1])==39?E(_mp):[2];}]:[0,function(_mr){return E(E(_mr)[1])==39?E(_mp):[2];}];})(_mm[1],_mm[2]);});});return _aO([0,function(_ms){return E(E(_ms)[1])==39?E([0,function(_mt){var _mu=E(_mt);switch(E(_mu[1])){case 39:return [2];case 92:return E(_mk);default:var _mv=new T(function(){return A(_mh,[[0,_mu]]);});return [0,function(_mw){return E(E(_mw)[1])==39?E(_mv):[2];}];}}]):[2];}],new T(function(){var _mx=new T(function(){return _ln(_H,_mh);});return _aO([0,function(_my){return E(E(_my)[1])==34?E(_mx):[2];}],new T(function(){return _aO([0,function(_mz){return !_dQ(_bl,_mz,_lC)?[2]:A(_mh,[[2,[1,_mz,_g]]]);}],new T(function(){return _aO([0,function(_mA){return !_dQ(_bl,_mA,_dV)?[2]:_lF(_dW,function(_mB){var _mC=[1,_mA,_mB];return !_dQ(_bC,_mC,_mf)?A(_mh,[[4,_mC]]):A(_mh,[[2,_mC]]);});}],new T(function(){return _aO([0,function(_mD){var _mE=E(_mD),_mF=_mE[1],_mG=u_iswalpha(_mF);return E(_mG)==0?E(_mF)==95?_lF(_lA,function(_mH){return A(_mh,[[3,[1,_mE,_mH]]]);}):[2]:_lF(_lA,function(_mI){return A(_mh,[[3,[1,_mE,_mI]]]);});}],new T(function(){return _bX(_e0,_dL,_mh);}));}));}));}));}));}));},_mJ=function(_mK){var _mL=new T(function(){return _mg(_mK);});return [1,function(_mM){return A(_kx,[_mM,function(_mN){return E(_mL);}]);}];},_mO=[0,0],_mP=function(_mQ,_mR){var _mS=new T(function(){return A(_mQ,[_mO,function(_mT){var _mU=new T(function(){return A(_mR,[_mT]);});return _mJ(function(_mV){var _mW=E(_mV);if(_mW[0]==2){var _mX=E(_mW[1]);return _mX[0]==0?[2]:E(E(_mX[1])[1])==41?E(_mX[2])[0]==0?E(_mU):[2]:[2];}else{return [2];}});}]);});return _mJ(function(_mY){var _mZ=E(_mY);if(_mZ[0]==2){var _n0=E(_mZ[1]);return _n0[0]==0?[2]:E(E(_n0[1])[1])==40?E(_n0[2])[0]==0?E(_mS):[2]:[2];}else{return [2];}});},_n1=function(_n2,_n3,_n4){var _n5=function(_n6,_n7){var _n8=new T(function(){return _mg(function(_n9){return A(_n2,[_n9,_n6,function(_na){return A(_n7,[new T(function(){return [0, -E(_na)[1]];})]);}]);});});return _aO(_mJ(function(_nb){var _nc=E(_nb);if(_nc[0]==4){var _nd=E(_nc[1]);return _nd[0]==0?A(_n2,[_nc,_n6,_n7]):E(E(_nd[1])[1])==45?E(_nd[2])[0]==0?E([1,function(_ne){return A(_kx,[_ne,function(_nf){return E(_n8);}]);}]):A(_n2,[_nc,_n6,_n7]):A(_n2,[_nc,_n6,_n7]);}else{return A(_n2,[_nc,_n6,_n7]);}}),new T(function(){return _mP(_n5,_n7);}));};return _n5(_n3,_n4);},_ng=function(_nh,_ni){return [2];},_nj=function(_nk,_nl){return _ng(_nk,_nl);},_nm=function(_nn){var _no=E(_nn);return _no[0]==0?[1,new T(function(){return _dm(new T(function(){return _jC(E(_no[1])[1]);}),_dd,_no[2]);})]:E(_no[2])[0]==0?E(_no[3])[0]==0?[1,new T(function(){return _dm(_dc,_dd,_no[1]);})]:[0]:[0];},_np=function(_nq){var _nr=E(_nq);if(_nr[0]==5){var _ns=_nm(_nr[1]);if(!_ns[0]){return E(_ng);}else{var _nt=new T(function(){return [0,_eo(_ns[1])];});return function(_nu,_nv){return A(_nv,[_nt]);};}}else{return E(_nj);}},_nw=function(_nk,_nl){return _n1(_np,_nk,_nl);},_nx=function(_ny,_nz){var _nA=function(_nB,_nC){var _nD=new T(function(){return A(_nC,[_g]);}),_nE=new T(function(){return A(_ny,[_mO,function(_nF){return _nA(_ec,function(_nG){return A(_nC,[[1,_nF,_nG]]);});}]);});return _mJ(function(_nH){var _nI=E(_nH);if(_nI[0]==2){var _nJ=E(_nI[1]);if(!_nJ[0]){return [2];}else{var _nK=_nJ[2];switch(E(E(_nJ[1])[1])){case 44:return E(_nK)[0]==0?!E(_nB)?[2]:E(_nE):[2];case 93:return E(_nK)[0]==0?E(_nD):[2];default:return [2];}}}else{return [2];}});},_nL=function(_nM){var _nN=new T(function(){return _aO(_nA(_0,_nM),new T(function(){return A(_ny,[_mO,function(_nO){return _nA(_ec,function(_nP){return A(_nM,[[1,_nO,_nP]]);});}]);}));});return _aO(_mJ(function(_nQ){var _nR=E(_nQ);if(_nR[0]==2){var _nS=E(_nR[1]);return _nS[0]==0?[2]:E(E(_nS[1])[1])==91?E(_nS[2])[0]==0?E(_nN):[2]:[2];}else{return [2];}}),new T(function(){return _mP(function(_nT,_nU){return _nL(_nU);},_nM);}));};return _nL(_nz);},_nV=function(_nW,_nX){return _nx(_nw,_nX);},_nY=new T(function(){return _nx(_nw,_bP);}),_nZ=function(_nl){return _aE(_nY,_nl);},_o0=function(_o1){var _o2=new T(function(){return _n1(_np,_o1,_bP);});return function(_ci){return _aE(_o2,_ci);};},_o3=[0,_o0,_nZ,_nw,_nV],_o4=function(_o5,_o6){return _3F(0,E(_o5)[1],_o6);},_o7=function(_o8,_o9){return _2w(_o4,_o8,_o9);},_oa=function(_ob,_oc,_od){return _3F(E(_ob)[1],E(_oc)[1],_od);},_oe=[0,_oa,_58,_o7],_of=unCStr("GHC.Types"),_og=unCStr("Int"),_oh=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5O,_of,_og],_oi=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_oh,_g],_oj=function(_ok){return E(_oi);},_ol=function(_om){return E(E(_om)[1]);},_on=function(_oo){return E(E(_oo)[2]);},_op=function(_oq,_or){var _os=new T(function(){return A(_on,[_oq,_or]);}),_ot=new T(function(){return _ol(_oq);}),_ou=new T(function(){return _3q(_ot);}),_ov=new T(function(){return _30(_ot);});return function(_ow){return A(_ov,[_os,function(_ox){return A(_ou,[[0,_ox,_ow]]);}]);};},_oy=function(_oz,_oA){return A(_oz,[function(_){return jsFind(toJSStr(E(_oA)));}]);},_oB=[0],_oC=function(_oD){return E(E(_oD)[3]);},_oE=new T(function(){return E(_6H);}),_oF=new T(function(){return [0,"value"];}),_oG=function(_oH){return E(E(_oH)[6]);},_oI=unCStr("[]"),_oJ=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5O,_of,_oI],_oK=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_oJ,_g],_oL=function(_oM){return E(_oK);},_oN=unCStr("Char"),_oO=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5O,_of,_oN],_oP=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_oO,_g],_oQ=function(_oR){return E(_oP);},_oS=new T(function(){return _6I(_oL,_oQ);}),_oT=new T(function(){return A(_oS,[_6H]);}),_oU=function(_oV){return E(E(_oV)[1]);},_oW=[0,0],_oX=[0,32],_oY=[0,10],_oZ=function(_p0){var _p1=E(_p0);if(!_p1[0]){return E(_H);}else{var _p2=_p1[1],_p3=E(_p1[2]);if(!_p3[0]){return _p4(_oY,_p2);}else{var _p5=new T(function(){return _oZ(_p3);}),_p6=new T(function(){return _p4(_oY,_p2);});return function(_p7){return A(_p6,[[1,_oX,new T(function(){return A(_p5,[_p7]);})]]);};}}},_p8=unCStr("->"),_p9=[1,_p8,_g],_pa=[1,_of,_p9],_pb=[1,_5O,_pa],_pc=[0,32],_pd=function(_pe){var _pf=E(_pe);if(!_pf[0]){return [0];}else{var _pg=_pf[1],_ph=E(_pf[2]);return _ph[0]==0?E(_pg):_1G(_pg,[1,_pc,new T(function(){return _pd(_ph);})]);}},_pi=new T(function(){return _pd(_pb);}),_pj=new T(function(){var _pk=_6c(_pi);return [0,_pk[1],_pk[2],_5O,_of,_p8];}),_pl=function(_pm,_pn){var _po=E(_pm);return _po[0]==0?E(_pn):A(_po[1],[new T(function(){return _pl(_po[2],_pn);})]);},_pp=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_pq=[1,_5Q,_g],_pr=function(_ps){var _pt=E(_ps);if(!_pt[0]){return [0];}else{var _pu=E(_pt[1]);return [1,[0,_pu[1],_pu[2]],new T(function(){return _pr(_pt[2]);})];}},_pv=new T(function(){var _pw=_1G(_g,_pq);if(!_pw[0]){return E(_oJ);}else{var _px=_6c(new T(function(){return _60(_6o(_6z,[1,_pp,new T(function(){return _pr(_pw);})]));}));return E(_oJ);}}),_py=[0,40],_pz=function(_pA){return _p4(_oY,_pA);},_pB=[0,8],_pC=unCStr(" -> "),_pD=[0,9],_pE=[0,93],_pF=[0,91],_pG=[0,41],_pH=[0,44],_pI=function(_pA){return [1,_pH,_pA];},_pJ=function(_pK,_pL){var _pM=E(_pL);return _pM[0]==0?[0]:[1,_pK,[1,_pM[1],new T(function(){return _pJ(_pK,_pM[2]);})]];},_p4=function(_pN,_pO){var _pP=E(_pO),_pQ=_pP[3],_pR=E(_pP[4]);if(!_pR[0]){return function(_pS){return _1G(E(_pQ)[5],_pS);};}else{var _pT=_pR[1],_pU=new T(function(){var _pV=E(_pQ)[5],_pW=new T(function(){return _oZ(_pR);}),_pX=new T(function(){return E(_pN)[1]<=9?function(_pY){return _1G(_pV,[1,_oX,new T(function(){return A(_pW,[_pY]);})]);}:function(_pZ){return [1,_3E,new T(function(){return _1G(_pV,[1,_oX,new T(function(){return A(_pW,[[1,_3D,_pZ]]);})]);})];};}),_q0=E(_pV);if(!_q0[0]){return E(_pX);}else{if(E(E(_q0[1])[1])==40){var _q1=E(_q0[2]);return _q1[0]==0?E(_pX):E(E(_q1[1])[1])==44?function(_q2){return [1,_py,new T(function(){return A(new T(function(){var _q3=_6o(_pz,_pR);if(!_q3[0]){return E(_H);}else{var _q4=new T(function(){return _pJ(_pI,_q3[2]);});return function(_ci){return _pl([1,_q3[1],_q4],_ci);};}}),[[1,_pG,_q2]]);})];}:E(_pX);}else{return E(_pX);}}}),_q5=E(_pR[2]);if(!_q5[0]){var _q6=E(_pQ),_q7=E(_pv),_q8=hs_eqWord64(_q6[1],_q7[1]);if(!E(_q8)){return E(_pU);}else{var _q9=hs_eqWord64(_q6[2],_q7[2]);if(!E(_q9)){return E(_pU);}else{var _qa=new T(function(){return _p4(_oW,_pT);});return function(_qb){return [1,_pF,new T(function(){return A(_qa,[[1,_pE,_qb]]);})];};}}}else{if(!E(_q5[2])[0]){var _qc=E(_pQ),_qd=E(_pj),_qe=hs_eqWord64(_qc[1],_qd[1]);if(!E(_qe)){return E(_pU);}else{var _qf=hs_eqWord64(_qc[2],_qd[2]);if(!E(_qf)){return E(_pU);}else{var _qg=new T(function(){return _p4(_pB,_q5[1]);}),_qh=new T(function(){return _p4(_pD,_pT);});return E(_pN)[1]<=8?function(_qi){return A(_qh,[new T(function(){return _1G(_pC,new T(function(){return A(_qg,[_qi]);}));})]);}:function(_qj){return [1,_3E,new T(function(){return A(_qh,[new T(function(){return _1G(_pC,new T(function(){return A(_qg,[[1,_3D,_qj]]);}));})]);})];};}}}else{return E(_pU);}}}},_qk=function(_ql,_qm,_qn,_qo,_qp,_qq){var _qr=E(_ql),_qs=_qr[1],_qt=_qr[3],_qu=new T(function(){return A(_qt,[_oB]);}),_qv=new T(function(){return _oC(_qp);}),_qw=new T(function(){return _oG(_qp);}),_qx=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_p4,[_oW,A(_qn,[_oE]),_g]);}));}),_qy=new T(function(){return A(_oU,[_qo,_8]);});return A(_qs,[new T(function(){return _oy(_qm,_qq);}),function(_qz){var _qA=E(_qz);return _qA[0]==0?E(_qu):A(_qs,[new T(function(){return A(_qm,[function(_){var _qB=jsGet(E(_qA[1])[1],E(_oF)[1]);return [1,new T(function(){return fromJSStr(_qB);})];}]);}),function(_qC){var _qD=E(_qC);if(!_qD[0]){return E(_qu);}else{var _qE=_qD[1];if(!E(new T(function(){var _qF=A(_qn,[_oE]),_qG=E(_oT),_qH=hs_eqWord64(_qF[1],_qG[1]);if(!E(_qH)){return false;}else{var _qI=hs_eqWord64(_qF[2],_qG[2]);return E(_qI)==0?false:true;}}))){var _qJ=new T(function(){return A(_qt,[[1,_qE,new T(function(){return A(_qw,[new T(function(){return A(_qv,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1G(_qE,_qx);}));})]);})]);})]]);}),_qK=A(_qy,[_qE]);if(!_qK[0]){return E(_qJ);}else{var _qL=E(_qK[1]);return E(_qL[2])[0]==0?E(_qK[2])[0]==0?A(_qt,[[2,_qL[1]]]):E(_qJ):E(_qJ);}}else{return A(_qt,[[2,_qE]]);}}}]);}]);},_qM=1,_qN=function(_qO){return E(E(_qO)[9]);},_qP=function(_qQ,_qR){return A(_3q,[_qQ,[0,_qR,_qR]]);},_qS=function(_qT,_qU,_qV){return A(_3q,[_qT,[0,_A,_qU]]);},_qW=function(_qX){return E(E(_qX)[2]);},_qY=function(_qZ,_r0,_r1,_r2,_r3){var _r4=new T(function(){return _92(_qZ);}),_r5=new T(function(){return _94(_r4);}),_r6=new T(function(){return _ol(_r0);}),_r7=new T(function(){return _3s(_r6);}),_r8=new T(function(){return _3K([0,coercionToken],_r7,function(_r9){return _qP(_r6,_r9);},function(_ra,_rb){return _qS(_r6,_ra,_rb);});}),_rc=new T(function(){return _3q(_r6);}),_rd=new T(function(){return _30(_r6);}),_re=new T(function(){return _3q(_r6);}),_rf=new T(function(){return _30(_r6);}),_rg=new T(function(){return _3q(_r6);}),_rh=new T(function(){return _30(_r6);}),_ri=new T(function(){return _3q(_r6);}),_rj=new T(function(){return _30(_r6);}),_rk=new T(function(){return _qW(_r2);}),_rl=new T(function(){return _qN(_qZ);});return function(_rm,_rn,_ro){return function(_rp){return A(_rj,[new T(function(){var _rq=E(_rm);return _rq[0]==0?A(_r8,[_rp]):A(_ri,[[0,_rq[1],_rp]]);}),function(_rr){var _rs=new T(function(){return E(E(_rr)[1]);}),_rt=new T(function(){return _qk(_r7,function(_ru){return _op(_r0,_ru);},_r1,_r3,_qZ,_rs);}),_rv=new T(function(){return A(_rl,[_rs,_rn,new T(function(){var _rw=E(_ro);if(!_rw[0]){return [0];}else{var _rx=_rw[1],_ry=_1q(_r1,_oS,_rx);return _ry[0]==0?A(_rk,[_rx]):E(_ry[1]);}}),_0,_9]);});return A(_rh,[new T(function(){var _rz=new T(function(){return E(E(_rr)[2]);});return A(_rg,[[0,_rz,_rz]]);}),function(_rA){return A(_rf,[new T(function(){return A(_re,[[0,_A,new T(function(){var _rB=E(E(_rA)[1]);return [0,_rB[1],_rB[2],_qM,_rB[4],_rB[5],_rB[6]];})]]);}),function(_rC){return A(_rd,[new T(function(){return A(_rt,[new T(function(){return E(E(_rC)[2]);})]);}),function(_rD){var _rE=E(_rD),_rF=_rE[2],_rG=E(_rE[1]);switch(_rG[0]){case 0:return A(_rc,[[0,[0,_rv,_9],_rF]]);case 1:return A(_rc,[[0,[0,new T(function(){return A(_r5,[new T(function(){return A(_rl,[_rs,_rn,_rG[1],_0,_9]);}),_rG[2]]);}),_9],_rF]]);default:var _rH=_rG[1];return A(_rc,[[0,[0,new T(function(){return A(_rl,[_rs,_rn,new T(function(){var _rI=_1q(_r1,_oS,_rH);return _rI[0]==0?A(_rk,[_rH]):E(_rI[1]);}),_0,_9]);}),[1,_rH]],_rF]]);}}]);}]);}]);}]);};};},_rJ=new T(function(){return _qY(_8F,_9L,_oj,_oe,_o3);}),_rK=new T(function(){return A(_rJ,[_9,_9K,_9]);}),_rL=unCStr("keydown"),_rM=unCStr("mousemove"),_rN=unCStr("blur"),_rO=unCStr("focus"),_rP=unCStr("change"),_rQ=unCStr("unload"),_rR=unCStr("load"),_rS=unCStr("keyup"),_rT=unCStr("keypress"),_rU=unCStr("mouseup"),_rV=unCStr("mousedown"),_rW=unCStr("dblclick"),_rX=unCStr("click"),_rY=unCStr("mouseout"),_rZ=unCStr("mouseover"),_s0=function(_s1){switch(E(_s1)[0]){case 0:return E(_rR);case 1:return E(_rQ);case 2:return E(_rP);case 3:return E(_rO);case 4:return E(_rN);case 5:return E(_rM);case 6:return E(_rZ);case 7:return E(_rY);case 8:return E(_rX);case 9:return E(_rW);case 10:return E(_rV);case 11:return E(_rU);case 12:return E(_rT);case 13:return E(_rS);default:return E(_rL);}},_s2=[0],_s3=unCStr("OnLoad"),_s4=[0,_s3,_s2],_s5=function(_){var _=0,_s6=newMVar(),_=putMVar(_s6,_s4);return [0,_s6];},_s7=new T(function(){return _2(_s5);}),_s8=function(_s9,_sa,_){var _sb=A(_s9,[_]);return die(_sa);},_sc=function(_sd,_se,_sf,_){return _s8(function(_){var _=putMVar(_se,_sd);return _A;},_sf,_);},_sg=function(_sh,_){var _si=0;if(!E(_si)){return (function(_){var _sj=E(_s7)[1],_sk=takeMVar(_sj),_sl=jsCatch(function(_){return (function(_){return _sh;})();},function(_X,_){return _sc(_sk,_sj,_X,_);}),_=putMVar(_sj,_sl);return _A;})();}else{var _sm=E(_s7)[1],_sn=takeMVar(_sm),_so=jsCatch(function(_){return _sh;},function(_X,_){return _sc(_sn,_sm,_X,_);}),_=putMVar(_sm,_so);return _A;}},_sp=unCStr("true"),_sq=function(_sr,_ss){while(1){var _st=E(_sr);if(!_st[0]){return E(_ss)[0]==0?true:false;}else{var _su=E(_ss);if(!_su[0]){return false;}else{if(E(_st[1])[1]!=E(_su[1])[1]){return false;}else{_sr=_st[2];_ss=_su[2];continue;}}}}},_sv=new T(function(){return [0,"keydown"];}),_sw=new T(function(){return [0,"mousemove"];}),_sx=new T(function(){return [0,"blur"];}),_sy=new T(function(){return [0,"focus"];}),_sz=new T(function(){return [0,"change"];}),_sA=new T(function(){return [0,"unload"];}),_sB=new T(function(){return [0,"load"];}),_sC=new T(function(){return [0,"keyup"];}),_sD=new T(function(){return [0,"keypress"];}),_sE=new T(function(){return [0,"mouseup"];}),_sF=new T(function(){return [0,"mousedown"];}),_sG=new T(function(){return [0,"dblclick"];}),_sH=new T(function(){return [0,"click"];}),_sI=new T(function(){return [0,"mouseout"];}),_sJ=new T(function(){return [0,"mouseover"];}),_sK=function(_sL){switch(E(_sL)[0]){case 0:return E(_sB);case 1:return E(_sA);case 2:return E(_sz);case 3:return E(_sy);case 4:return E(_sx);case 5:return E(_sw);case 6:return E(_sJ);case 7:return E(_sI);case 8:return E(_sH);case 9:return E(_sG);case 10:return E(_sF);case 11:return E(_sE);case 12:return E(_sD);case 13:return E(_sC);default:return E(_sv);}},_sM=function(_sN,_sO,_sP){var _sQ=new T(function(){return _s0(_sO);}),_sR=new T(function(){return _sK(_sO);});return function(_sS,_){var _sT=A(_sN,[_sS,_]),_sU=E(_sT),_sV=_sU[1],_sW=E(_sQ),_sX=jsGetAttr(_sV,toJSStr(_sW));if(!_sq(fromJSStr(_sX),_sp)){var _sY=E(_sP),_sZ=jsSetCB(_sV,E(_sR)[1],E([0,_sP])[1]),_t0=A(_B,[_H,_sU,_sW,_sp,_]);return _sU;}else{return _sU;}};},_t1=function(_t2,_t3){var _t4=new T(function(){return _s0(_t3);}),_t5=[0,_t4,_s2];return function(_t6,_){var _t7=E(_t6),_t8=E(_t7[4]),_t9=_t8[1],_ta=_t8[2],_tb=A(_t2,[_t7,_]),_tc=E(_tb),_td=E(_tc[1]),_te=_td[1];return [0,[0,new T(function(){var _tf=E(_t3);switch(_tf[0]){case 0:return _sM(_te,_tf,function(_){var _tg=_sg(_t5,_),_th=A(_t9,[_]),_ti=E(_th);if(!_ti[0]){return _A;}else{var _tj=A(_ta,[_ti[1],_]);return _A;}});case 1:return _sM(_te,_tf,function(_){var _tk=_sg(_t5,_),_tl=A(_t9,[_]),_tm=E(_tl);if(!_tm[0]){return _A;}else{var _tn=A(_ta,[_tm[1],_]);return _A;}});case 2:return _sM(_te,_tf,function(_){var _to=_sg(_t5,_),_tp=A(_t9,[_]),_tq=E(_tp);if(!_tq[0]){return _A;}else{var _tr=A(_ta,[_tq[1],_]);return _A;}});case 3:return _sM(_te,_tf,function(_){var _ts=_sg(_t5,_),_tt=A(_t9,[_]),_tu=E(_tt);if(!_tu[0]){return _A;}else{var _tv=A(_ta,[_tu[1],_]);return _A;}});case 4:return _sM(_te,_tf,function(_){var _tw=_sg(_t5,_),_tx=A(_t9,[_]),_ty=E(_tx);if(!_ty[0]){return _A;}else{var _tz=A(_ta,[_ty[1],_]);return _A;}});case 5:return _sM(_te,_tf,function(_tA,_){var _tB=_sg([0,_t4,[2,E(_tA)]],_),_tC=A(_t9,[_]),_tD=E(_tC);if(!_tD[0]){return _A;}else{var _tE=A(_ta,[_tD[1],_]);return _A;}});case 6:return _sM(_te,_tf,function(_tF,_){var _tG=_sg([0,_t4,[2,E(_tF)]],_),_tH=A(_t9,[_]),_tI=E(_tH);if(!_tI[0]){return _A;}else{var _tJ=A(_ta,[_tI[1],_]);return _A;}});case 7:return _sM(_te,_tf,function(_){var _tK=A(_t9,[_]),_tL=E(_tK);if(!_tL[0]){return _A;}else{var _tM=A(_ta,[_tL[1],_]);return _A;}});case 8:return _sM(_te,_tf,function(_tN,_tO,_){var _tP=_sg([0,_t4,[1,_tN,E(_tO)]],_),_tQ=A(_t9,[_]),_tR=E(_tQ);if(!_tR[0]){return _A;}else{var _tS=A(_ta,[_tR[1],_]);return _A;}});case 9:return _sM(_te,_tf,function(_tT,_tU,_){var _tV=_sg([0,_t4,[1,_tT,E(_tU)]],_),_tW=A(_t9,[_]),_tX=E(_tW);if(!_tX[0]){return _A;}else{var _tY=A(_ta,[_tX[1],_]);return _A;}});case 10:return _sM(_te,_tf,function(_tZ,_u0,_){var _u1=_sg([0,_t4,[1,_tZ,E(_u0)]],_),_u2=A(_t9,[_]),_u3=E(_u2);if(!_u3[0]){return _A;}else{var _u4=A(_ta,[_u3[1],_]);return _A;}});case 11:return _sM(_te,_tf,function(_u5,_u6,_){var _u7=_sg([0,_t4,[1,_u5,E(_u6)]],_),_u8=A(_t9,[_]),_u9=E(_u8);if(!_u9[0]){return _A;}else{var _ua=A(_ta,[_u9[1],_]);return _A;}});case 12:return _sM(_te,_tf,function(_ub,_){var _uc=_sg([0,_t4,[3,_ub]],_),_ud=A(_t9,[_]),_ue=E(_ud);if(!_ue[0]){return _A;}else{var _uf=A(_ta,[_ue[1],_]);return _A;}});case 13:return _sM(_te,_tf,function(_ug,_){var _uh=_sg([0,_t4,[3,_ug]],_),_ui=A(_t9,[_]),_uj=E(_ui);if(!_uj[0]){return _A;}else{var _uk=A(_ta,[_uj[1],_]);return _A;}});default:return _sM(_te,_tf,function(_ul,_){var _um=_sg([0,_t4,[3,_ul]],_),_un=A(_t9,[_]),_uo=E(_un);if(!_uo[0]){return _A;}else{var _up=A(_ta,[_uo[1],_]);return _A;}});}}),_td[2]],_tc[2]];};},_uq=new T(function(){return _t1(_rK,_9J);}),_ur=function(_us,_){var _ut=A(_uq,[_us,_]),_uu=E(_ut),_uv=E(_uu[1]);return [0,[0,function(_uw,_){var _ux=A(_uv[1],[_uw,_]),_uy=_5m(_uw,_);return _uw;},_uv[2]],_uu[2]];},_uz=new T(function(){return [1,_ur,_uz];}),_uA=function(_uB,_uC){var _uD=E(_uB);if(!_uD){return [0];}else{var _uE=E(_uC);return _uE[0]==0?[0]:[1,_uE[1],new T(function(){return _uA(_uD-1|0,_uE[2]);})];}},_uF=function(_uG,_uH){return _uG<0?[0]:_uA(_uG,_uH);},_uI=function(_uJ,_uK){var _uL=E(_uJ)[1];return _uL>0?_uF(_uL,_uK):[0];},_uM=function(_uN){return E(_uN);},_uO=function(_uP){var _uQ=new T(function(){return _9C(_5E,_uI(_uP,_uz));}),_uR=new T(function(){return _4Z(_4S,new T(function(){return unAppCStr("This widget sum ",new T(function(){return _1G(_3F(0,E(_uP)[1],_g),_5A);}));}));});return function(_uS,_){var _uT=_4f(_uQ,_5s,_uS,_),_uU=E(_uT),_uV=E(_uU[1]),_uW=new T(function(){return _4Z(_uM,_uV[1]);});return [0,[0,function(_uX,_){var _uY=A(_uR,[_uX,_]),_uZ=A(_uW,[_uX,_]);return _uX;},_uV[2]],_uU[2]];};},_v0=new T(function(){return _uO(_4R);}),_v1=unCStr("center"),_v2=function(_v3,_v4){var _v5=new T(function(){return A(_v3,[_v4]);});return function(_v6,_){var _v7=jsCreateElem(toJSStr(E(_v1))),_v8=jsAppendChild(_v7,E(_v6)[1]),_v9=[0,_v7],_va=A(_v5,[_v9,_]);return _v9;};},_vb=function(_vc,_){return _vc;},_vd=unCStr("Two counters. One is pure and recursive, the other is stateful"),_ve=new T(function(){return _4Z(_4S,_vd);}),_vf=[8,coercionToken],_vg=function(_vh){return _aO(_mJ(function(_vi){var _vj=E(_vi);return _vj[0]==0?A(_vh,[_vj[1]]):[2];}),new T(function(){return _mP(_vk,_vh);}));},_vk=function(_vl,_vm){return _vg(_vm);},_vn=function(_vo){return _aO(_aO(_mJ(function(_vp){var _vq=E(_vp);return _vq[0]==1?A(_vo,[_vq[1]]):[2];}),new T(function(){return _nx(_vk,_vo);})),new T(function(){return _mP(_vr,_vo);}));},_vr=function(_vs,_vt){return _vn(_vt);},_vu=new T(function(){return _mP(_vr,_bP);}),_vv=new T(function(){return _nx(_vk,_bP);}),_vw=function(_vx){var _vy=E(_vx);return _vy[0]==1?[3,_vy[1],_bO]:[2];},_vz=new T(function(){return _mg(_vw);}),_vA=function(_vB){return E(_vz);},_vC=function(_vD){return A(_kx,[_vD,_vA]);},_vE=[1,_vC],_vF=new T(function(){return _aO(_vE,_vv);}),_vG=new T(function(){return _aO(_vF,_vu);}),_vH=function(_nl){return _aE(_vG,_nl);},_vI=new T(function(){return _vg(_bP);}),_vJ=function(_nl){return _aE(_vI,_nl);},_vK=function(_vL){return E(_vJ);},_vM=[0,_vK,_vH,_vk,_vr],_vN=function(_vO){return E(E(_vO)[4]);},_vP=function(_vQ,_vR,_vS){return _nx(new T(function(){return _vN(_vQ);}),_vS);},_vT=function(_vU){var _vV=new T(function(){return _nx(new T(function(){return _vN(_vU);}),_bP);});return function(_ci){return _aE(_vV,_ci);};},_vW=function(_vX,_vY){var _vZ=new T(function(){return A(_vN,[_vX,_vY,_bP]);});return function(_ci){return _aE(_vZ,_ci);};},_w0=function(_w1){return [0,function(_nl){return _vW(_w1,_nl);},new T(function(){return _vT(_w1);}),new T(function(){return _vN(_w1);}),function(_nk,_nl){return _vP(_w1,_nk,_nl);}];},_w2=new T(function(){return _w0(_vM);}),_w3=unCStr("Prelude.(!!): negative index\n"),_w4=new T(function(){return err(_w3);}),_w5=unCStr("Prelude.(!!): index too large\n"),_w6=new T(function(){return err(_w5);}),_w7=function(_w8,_w9){while(1){var _wa=E(_w8);if(!_wa[0]){return E(_w6);}else{var _wb=E(_w9);if(!_wb){return E(_wa[1]);}else{_w8=_wa[2];_w9=_wb-1|0;continue;}}}},_wc=unCStr("ACK"),_wd=unCStr("BEL"),_we=unCStr("BS"),_wf=unCStr("SP"),_wg=[1,_wf,_g],_wh=unCStr("US"),_wi=[1,_wh,_wg],_wj=unCStr("RS"),_wk=[1,_wj,_wi],_wl=unCStr("GS"),_wm=[1,_wl,_wk],_wn=unCStr("FS"),_wo=[1,_wn,_wm],_wp=unCStr("ESC"),_wq=[1,_wp,_wo],_wr=unCStr("SUB"),_ws=[1,_wr,_wq],_wt=unCStr("EM"),_wu=[1,_wt,_ws],_wv=unCStr("CAN"),_ww=[1,_wv,_wu],_wx=unCStr("ETB"),_wy=[1,_wx,_ww],_wz=unCStr("SYN"),_wA=[1,_wz,_wy],_wB=unCStr("NAK"),_wC=[1,_wB,_wA],_wD=unCStr("DC4"),_wE=[1,_wD,_wC],_wF=unCStr("DC3"),_wG=[1,_wF,_wE],_wH=unCStr("DC2"),_wI=[1,_wH,_wG],_wJ=unCStr("DC1"),_wK=[1,_wJ,_wI],_wL=unCStr("DLE"),_wM=[1,_wL,_wK],_wN=unCStr("SI"),_wO=[1,_wN,_wM],_wP=unCStr("SO"),_wQ=[1,_wP,_wO],_wR=unCStr("CR"),_wS=[1,_wR,_wQ],_wT=unCStr("FF"),_wU=[1,_wT,_wS],_wV=unCStr("VT"),_wW=[1,_wV,_wU],_wX=unCStr("LF"),_wY=[1,_wX,_wW],_wZ=unCStr("HT"),_x0=[1,_wZ,_wY],_x1=[1,_we,_x0],_x2=[1,_wd,_x1],_x3=[1,_wc,_x2],_x4=unCStr("ENQ"),_x5=[1,_x4,_x3],_x6=unCStr("EOT"),_x7=[1,_x6,_x5],_x8=unCStr("ETX"),_x9=[1,_x8,_x7],_xa=unCStr("STX"),_xb=[1,_xa,_x9],_xc=unCStr("SOH"),_xd=[1,_xc,_xb],_xe=unCStr("NUL"),_xf=[1,_xe,_xd],_xg=[0,92],_xh=unCStr("\\DEL"),_xi=unCStr("\\a"),_xj=unCStr("\\\\"),_xk=unCStr("\\SO"),_xl=unCStr("\\r"),_xm=unCStr("\\f"),_xn=unCStr("\\v"),_xo=unCStr("\\n"),_xp=unCStr("\\t"),_xq=unCStr("\\b"),_xr=function(_xs,_xt){if(_xs<=127){var _xu=E(_xs);switch(_xu){case 92:return _1G(_xj,_xt);case 127:return _1G(_xh,_xt);default:if(_xu<32){var _xv=E(_xu);switch(_xv){case 7:return _1G(_xi,_xt);case 8:return _1G(_xq,_xt);case 9:return _1G(_xp,_xt);case 10:return _1G(_xo,_xt);case 11:return _1G(_xn,_xt);case 12:return _1G(_xm,_xt);case 13:return _1G(_xl,_xt);case 14:return _1G(_xk,new T(function(){var _xw=E(_xt);return _xw[0]==0?[0]:E(E(_xw[1])[1])==72?unAppCStr("\\&",_xw):E(_xw);}));default:return _1G([1,_xg,new T(function(){var _xx=_xv;return _xx>=0?_w7(_xf,_xx):E(_w4);})],_xt);}}else{return [1,[0,_xu],_xt];}}}else{return [1,_xg,new T(function(){var _xy=jsShowI(_xs);return _1G(fromJSStr(_xy),new T(function(){var _xz=E(_xt);if(!_xz[0]){return [0];}else{var _xA=E(_xz[1])[1];return _xA<48?E(_xz):_xA>57?E(_xz):unAppCStr("\\&",_xz);}}));})];}},_xB=[0,39],_xC=[1,_xB,_g],_xD=unCStr("\'\\\'\'"),_xE=function(_xF){var _xG=E(E(_xF)[1]);return _xG==39?E(_xD):[1,_xB,new T(function(){return _xr(_xG,_xC);})];},_xH=[0,34],_xI=unCStr("\\\""),_xJ=function(_xK,_xL){var _xM=E(_xK);if(!_xM[0]){return E(_xL);}else{var _xN=_xM[2],_xO=E(E(_xM[1])[1]);return _xO==34?_1G(_xI,new T(function(){return _xJ(_xN,_xL);})):_xr(_xO,new T(function(){return _xJ(_xN,_xL);}));}},_xP=function(_xQ,_xR){return [1,_xH,new T(function(){return _xJ(_xQ,[1,_xH,_xR]);})];},_xS=function(_xT){return _1G(_xD,_xT);},_xU=function(_xV,_xW){var _xX=E(E(_xW)[1]);return _xX==39?E(_xS):function(_xY){return [1,_xB,new T(function(){return _xr(_xX,[1,_xB,_xY]);})];};},_xZ=[0,_xU,_xE,_xP],_y0=function(_y1){return E(E(_y1)[3]);},_y2=function(_y3,_y4){return A(_y0,[_y3,_y4,_g]);},_y5=function(_y6,_y7,_y8){return _2w(new T(function(){return _y0(_y6);}),_y7,_y8);},_y9=function(_ya){var _yb=new T(function(){return _y0(_ya);});return [0,function(_yc){return E(_yb);},function(_xT){return _y2(_ya,_xT);},function(_yd,_xT){return _y5(_ya,_yd,_xT);}];},_ye=new T(function(){return _y9(_xZ);}),_yf=unCStr("submit"),_yg=new T(function(){return A(_qY,[_8F,_9L,_oS,_ye,_w2,_9,_yf]);}),_yh=[0,43],_yi=[1,_yh,_g],_yj=[1,_yi],_yk=new T(function(){return A(_yg,[_yj]);}),_yl=new T(function(){return _t1(_yk,_vf);}),_ym=function(_yn,_yo,_yp,_){var _yq=A(_yo,[_yp,_]),_yr=E(_yq),_ys=E(_yr[1]);return [0,[0,function(_yt,_){var _yu=_3Y(_3X,_yt,_),_yv=A(_B,[_H,_yu,_z,_yn,_]),_yw=A(_ys[1],[_yu,_]);return _yu;},_ys[2]],_yr[2]];},_yx=new T(function(){return _3K(_13,_3x,_11,_Y);}),_yy=new T(function(){return _3K(_13,_3x,_11,_Y);}),_yz=function(_yA,_yB,_yC,_){var _yD=A(_yy,[_yC,_]),_yE=A(_yx,[new T(function(){return E(E(_yD)[2]);}),_]),_yF=new T(function(){return E(E(_yD)[1]);});return _4f(function(_X,_){return _ym(_yF,_yA,_X,_);},function(_yG){var _yH=new T(function(){return A(_yB,[_yG]);});return function(_yI,_){var _yJ=A(_yH,[_yI,_]),_yK=E(_yJ),_yL=E(_yK[1]);return [0,[0,function(_yM,_){var _yN=E(_yF),_yO=jsFind(toJSStr(_yN)),_yP=E(_yO);if(!_yP[0]){return _45(_yN);}else{var _yQ=E(_yP[1]),_yR=A(_7,[E(_yQ[1]),_]),_yS=jsKillChild(E(_yQ)[1],_yR),_yT=A(_yL[1],[_yM,_]);return _yM;}},_yL[2]],_yK[2]];};},new T(function(){return E(E(_yE)[2]);}),_);},_yU=function(_yV){var _yW=new T(function(){return _yU(new T(function(){return [0,E(_yV)[1]+1|0];}));}),_yX=new T(function(){return _5c(_4S,new T(function(){return _58(_yV);}));});return function(_ci,_yY){return _yz(function(_yZ,_){var _z0=A(_yl,[_yZ,_]),_z1=E(_z0),_z2=E(_z1[1]);return [0,[0,function(_z3,_){var _z4=A(_yX,[_z3,_]),_z5=A(_z2[1],[_z3,_]);return _z3;},_z2[2]],_z1[2]];},function(_z6){return E(_yW);},_ci,_yY);};},_z7=unCStr("main"),_z8=unCStr("Main"),_z9=unCStr("Counter"),_za=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_z7,_z8,_z9],_zb=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_za,_g],_zc=function(_zd,_ze){var _zf=hs_leWord64(_zd,_ze);return E(_zf)==0?false:true;},_zg=function(_zh,_zi,_zj,_zk){var _zl=hs_eqWord64(_zh,_zj);if(!E(_zl)){var _zm=hs_leWord64(_zh,_zj);return E(_zm)==0?false:true;}else{return _zc(_zi,_zk);}},_zn=function(_zo,_zp){var _zq=E(_zo),_zr=_zq[1],_zs=_zq[2],_zt=E(_zp),_zu=_zt[1],_zv=_zt[2],_zw=hs_eqWord64(_zr,_zu);if(!E(_zw)){return !_zg(_zr,_zs,_zu,_zv)?2:0;}else{var _zx=hs_eqWord64(_zs,_zv);return E(_zx)==0?!_zg(_zr,_zs,_zu,_zv)?2:0:1;}},_zy=unCStr("Failure in Data.Map.balanceL"),_zz=new T(function(){return err(_zy);}),_zA=function(_zB,_zC,_zD,_zE){var _zF=E(_zE);if(!_zF[0]){var _zG=_zF[1],_zH=E(_zD);if(!_zH[0]){var _zI=_zH[1],_zJ=_zH[2],_zK=_zH[3];if(_zI<=(imul(3,_zG)|0)){return [0,(1+_zI|0)+_zG|0,E(E(_zB)),_zC,E(_zH),E(_zF)];}else{var _zL=E(_zH[4]);if(!_zL[0]){var _zM=_zL[1],_zN=E(_zH[5]);if(!_zN[0]){var _zO=_zN[1],_zP=_zN[2],_zQ=_zN[3],_zR=_zN[4];if(_zO>=(imul(2,_zM)|0)){var _zS=function(_zT){var _zU=E(_zN[5]);return _zU[0]==0?[0,(1+_zI|0)+_zG|0,E(_zP),_zQ,E([0,(1+_zM|0)+_zT|0,E(_zJ),_zK,E(_zL),E(_zR)]),E([0,(1+_zG|0)+_zU[1]|0,E(E(_zB)),_zC,E(_zU),E(_zF)])]:[0,(1+_zI|0)+_zG|0,E(_zP),_zQ,E([0,(1+_zM|0)+_zT|0,E(_zJ),_zK,E(_zL),E(_zR)]),E([0,1+_zG|0,E(E(_zB)),_zC,E(_f),E(_zF)])];},_zV=E(_zR);return _zV[0]==0?_zS(_zV[1]):_zS(0);}else{return [0,(1+_zI|0)+_zG|0,E(_zJ),_zK,E(_zL),E([0,(1+_zG|0)+_zO|0,E(E(_zB)),_zC,E(_zN),E(_zF)])];}}else{return E(_zz);}}else{return E(_zz);}}}else{return [0,1+_zG|0,E(E(_zB)),_zC,E(_f),E(_zF)];}}else{var _zW=E(_zD);if(!_zW[0]){var _zX=_zW[1],_zY=_zW[2],_zZ=_zW[3],_A0=_zW[5],_A1=E(_zW[4]);if(!_A1[0]){var _A2=_A1[1],_A3=E(_A0);if(!_A3[0]){var _A4=_A3[1],_A5=_A3[2],_A6=_A3[3],_A7=_A3[4];if(_A4>=(imul(2,_A2)|0)){var _A8=function(_A9){var _Aa=E(_A3[5]);return _Aa[0]==0?[0,1+_zX|0,E(_A5),_A6,E([0,(1+_A2|0)+_A9|0,E(_zY),_zZ,E(_A1),E(_A7)]),E([0,1+_Aa[1]|0,E(E(_zB)),_zC,E(_Aa),E(_f)])]:[0,1+_zX|0,E(_A5),_A6,E([0,(1+_A2|0)+_A9|0,E(_zY),_zZ,E(_A1),E(_A7)]),E([0,1,E(E(_zB)),_zC,E(_f),E(_f)])];},_Ab=E(_A7);return _Ab[0]==0?_A8(_Ab[1]):_A8(0);}else{return [0,1+_zX|0,E(_zY),_zZ,E(_A1),E([0,1+_A4|0,E(E(_zB)),_zC,E(_A3),E(_f)])];}}else{return [0,3,E(_zY),_zZ,E(_A1),E([0,1,E(E(_zB)),_zC,E(_f),E(_f)])];}}else{var _Ac=E(_A0);return _Ac[0]==0?[0,3,E(_Ac[2]),_Ac[3],E([0,1,E(_zY),_zZ,E(_f),E(_f)]),E([0,1,E(E(_zB)),_zC,E(_f),E(_f)])]:[0,2,E(E(_zB)),_zC,E(_zW),E(_f)];}}else{return [0,1,E(E(_zB)),_zC,E(_f),E(_f)];}}},_Ad=unCStr("Failure in Data.Map.balanceR"),_Ae=new T(function(){return err(_Ad);}),_Af=function(_Ag,_Ah,_Ai,_Aj){var _Ak=E(_Ai);if(!_Ak[0]){var _Al=_Ak[1],_Am=E(_Aj);if(!_Am[0]){var _An=_Am[1],_Ao=_Am[2],_Ap=_Am[3];if(_An<=(imul(3,_Al)|0)){return [0,(1+_Al|0)+_An|0,E(E(_Ag)),_Ah,E(_Ak),E(_Am)];}else{var _Aq=E(_Am[4]);if(!_Aq[0]){var _Ar=_Aq[1],_As=_Aq[2],_At=_Aq[3],_Au=_Aq[4],_Av=E(_Am[5]);if(!_Av[0]){var _Aw=_Av[1];if(_Ar>=(imul(2,_Aw)|0)){var _Ax=function(_Ay){var _Az=E(_Ag),_AA=E(_Aq[5]);return _AA[0]==0?[0,(1+_Al|0)+_An|0,E(_As),_At,E([0,(1+_Al|0)+_Ay|0,E(_Az),_Ah,E(_Ak),E(_Au)]),E([0,(1+_Aw|0)+_AA[1]|0,E(_Ao),_Ap,E(_AA),E(_Av)])]:[0,(1+_Al|0)+_An|0,E(_As),_At,E([0,(1+_Al|0)+_Ay|0,E(_Az),_Ah,E(_Ak),E(_Au)]),E([0,1+_Aw|0,E(_Ao),_Ap,E(_f),E(_Av)])];},_AB=E(_Au);return _AB[0]==0?_Ax(_AB[1]):_Ax(0);}else{return [0,(1+_Al|0)+_An|0,E(_Ao),_Ap,E([0,(1+_Al|0)+_Ar|0,E(E(_Ag)),_Ah,E(_Ak),E(_Aq)]),E(_Av)];}}else{return E(_Ae);}}else{return E(_Ae);}}}else{return [0,1+_Al|0,E(E(_Ag)),_Ah,E(_Ak),E(_f)];}}else{var _AC=E(_Aj);if(!_AC[0]){var _AD=_AC[1],_AE=_AC[2],_AF=_AC[3],_AG=_AC[5],_AH=E(_AC[4]);if(!_AH[0]){var _AI=_AH[1],_AJ=_AH[2],_AK=_AH[3],_AL=_AH[4],_AM=E(_AG);if(!_AM[0]){var _AN=_AM[1];if(_AI>=(imul(2,_AN)|0)){var _AO=function(_AP){var _AQ=E(_Ag),_AR=E(_AH[5]);return _AR[0]==0?[0,1+_AD|0,E(_AJ),_AK,E([0,1+_AP|0,E(_AQ),_Ah,E(_f),E(_AL)]),E([0,(1+_AN|0)+_AR[1]|0,E(_AE),_AF,E(_AR),E(_AM)])]:[0,1+_AD|0,E(_AJ),_AK,E([0,1+_AP|0,E(_AQ),_Ah,E(_f),E(_AL)]),E([0,1+_AN|0,E(_AE),_AF,E(_f),E(_AM)])];},_AS=E(_AL);return _AS[0]==0?_AO(_AS[1]):_AO(0);}else{return [0,1+_AD|0,E(_AE),_AF,E([0,1+_AI|0,E(E(_Ag)),_Ah,E(_f),E(_AH)]),E(_AM)];}}else{return [0,3,E(_AJ),_AK,E([0,1,E(E(_Ag)),_Ah,E(_f),E(_f)]),E([0,1,E(_AE),_AF,E(_f),E(_f)])];}}else{var _AT=E(_AG);return _AT[0]==0?[0,3,E(_AE),_AF,E([0,1,E(E(_Ag)),_Ah,E(_f),E(_f)]),E(_AT)]:[0,2,E(E(_Ag)),_Ah,E(_f),E(_AC)];}}else{return [0,1,E(E(_Ag)),_Ah,E(_f),E(_f)];}}},_AU=function(_AV,_AW,_AX){var _AY=E(_AV),_AZ=E(_AX);if(!_AZ[0]){var _B0=_AZ[2],_B1=_AZ[3],_B2=_AZ[4],_B3=_AZ[5];switch(_zn(_AY,_B0)){case 0:return _zA(_B0,_B1,_AU(_AY,_AW,_B2),_B3);case 1:return [0,_AZ[1],E(_AY),_AW,E(_B2),E(_B3)];default:return _Af(_B0,_B1,_B2,_AU(_AY,_AW,_B3));}}else{return [0,1,E(_AY),_AW,E(_f),E(_f)];}},_B4=[0,_2X,_5q],_B5=function(_B6,_){return [0,[0,_2X,[1,_B6]],_B6];},_B7=[1,_A],_B8=function(_B9){var _Ba=new T(function(){return [0,E(_B9)[1]+1|0];}),_Bb=new T(function(){return _5c(_4S,new T(function(){return _58(_B9);}));});return function(_ci,_yY){return _4f(function(_Bc,_){return [0,[0,_Bb,_B7],_Bc];},function(_Bd,_Be,_){return (function(_Be,_){return _4f(_B5,function(_Bf){return function(_Bg,_){return [0,_B4,new T(function(){var _Bh=E(_Bf);return [0,_Bh[1],_Bh[2],_Bh[3],_Bh[4],_Bh[5],new T(function(){return _AU(_zb,_Ba,_Bh[6]);})];})];};},_Be,_);})(_Be,_);},_ci,_yY);};},_Bi=function(_Bj){return E(_zb);},_Bk=function(_Bl,_Bm){while(1){var _Bn=E(_Bl),_Bo=E(_Bm);if(!_Bo[0]){switch(_zn(_Bn,_Bo[2])){case 0:_Bl=_Bn;_Bm=_Bo[4];continue;case 1:return [1,_Bo[3]];default:_Bl=_Bn;_Bm=_Bo[5];continue;}}else{return [0];}}},_Bp=function(_Bq,_Br,_Bs,_Bt){var _Bu=E(_Br),_Bv=_Bu[1],_Bw=_Bu[3],_Bx=new T(function(){return A(_Bt,[_oE]);}),_By=new T(function(){return A(_Bw,[_9]);});return A(_Bv,[new T(function(){return A(_Bv,[_Bs,function(_Bz){return A(_Bw,[new T(function(){var _BA=E(_Bq);return E(E(_Bz)[6]);})]);}]);}),function(_BB){var _BC=_Bk(_Bx,_BB);return _BC[0]==0?E(_By):A(_Bw,[[1,_BC[1]]]);}]);},_BD=new T(function(){return _Bp(_13,_3x,_11,_Bi);}),_BE=function(_BF){var _BG=new T(function(){return _yU(_BF);});return function(_BH,_){var _BI=A(_BG,[_BH,_]),_BJ=E(_BI),_BK=E(_BJ[1]),_BL=_4f(_yl,function(_BM){return function(_Be,_){return _4f(function(_BN,_){var _BO=A(_BD,[_BN,_]);return [0,[0,_vb,new T(function(){var _BP=E(E(_BO)[1]);return _BP[0]==0?E([1,_BF]):E(_BP);})],new T(function(){return E(E(_BO)[2]);})];},_B8,_Be,_);};},_BJ[2],_),_BQ=E(_BL),_BR=E(_BQ[1]),_BS=new T(function(){return _v2(_uM,function(_BT,_){var _BU=A(_BK[1],[_BT,_]),_BV=A(_BR[1],[_BT,_]);return _BT;});});return [0,[0,function(_BW,_){var _BX=A(_ve,[_BW,_]),_BY=_5m(_BW,_),_BZ=A(_BS,[_BW,_]);return _BW;},new T(function(){var _C0=E(_BK[2]);return _C0[0]==0?E(_BR[2]):E(_C0);})],_BQ[2]];};},_C1=new T(function(){return _BE(_4R);}),_C2=[0,4],_C3=function(_C4,_C5){return [1,_C5,new T(function(){return _C3(_C4,new T(function(){return A(_C4,[_C5]);}));})];},_C6=[0,1],_C7=[1,_C6,_g],_C8=[1,_5B,_g],_C9=function(_Ca,_Cb,_Cc){var _Cd=E(_Cb);if(!_Cd[0]){return [0];}else{var _Ce=E(_Cc);return _Ce[0]==0?[0]:[1,new T(function(){return A(_Ca,[_Cd[1],_Ce[1]]);}),new T(function(){return _C9(_Ca,_Cd[2],_Ce[2]);})];}},_Cf=function(_Cg){return _C9(_8U,[1,_5B,_Cg],new T(function(){return _1G(_Cg,_C8);}));},_Ch=new T(function(){return _C3(_Cf,_C7);}),_Ci=unCStr(" rows of the Pascal triangle "),_Cj=function(_Ck){var _Cl=new T(function(){return _2w(_o4,_Ck,_g);});return function(_ci,_yY){return _4S(_Cl,_ci,_yY);};},_Cm=unCStr("text-align:center"),_Cn=unCStr("style"),_Co=function(_Cp,_Cq){var _Cr=new T(function(){return _4Z(_Cj,_Cp);});return [1,function(_Cs,_){var _Ct=A(_Cr,[_Cs,_]),_Cu=A(_B,[_H,_Ct,_Cn,_Cm,_]);return _Ct;},_Cq];},_Cv=function(_Cw,_Cx){var _Cy=E(_Cw);if(!_Cy[0]){return [0];}else{var _Cz=_Cy[1];return _Cx>1?_Co(_Cz,new T(function(){return _Cv(_Cy[2],_Cx-1|0);})):_Co(_Cz,_g);}},_CA=function(_CB){var _CC=new T(function(){return _4Z(_4S,new T(function(){return unAppCStr("Show ",new T(function(){return _1G(_3F(0,E(_CB)[1],_g),_Ci);}));}));});return function(_CD,_){return [0,[0,function(_CE,_){var _CF=A(_CC,[_CE,_]),_CG=_8s(new T(function(){var _CH=E(_CB)[1];return _CH>0?_Cv(_Ch,_CH):[0];}),_CE,_);return _CE;},_9],_CD];};},_CI=new T(function(){return _CA(_C2);}),_CJ=unCStr("Different input elements:"),_CK=new T(function(){return _4Z(_4S,_CJ);}),_CL=unCStr(" returns: "),_CM=[1,_xH,_g],_CN=function(_CO){var _CP=new T(function(){return _5c(_4S,[1,_xH,new T(function(){return _xJ(_CO,_CM);})]);});return function(_CQ,_){return [0,[0,function(_CR,_){var _CS=_4S(_CL,_CR,_),_CT=A(_CP,[_CR,_]);return _CR;},_B7],_CQ];};},_CU=unCStr("blue"),_CV=[1,_CU,_g],_CW=unCStr("green"),_CX=[1,_CW,_CV],_CY=unCStr("red"),_CZ=[1,_CY,_CX],_D0=function(_D1){return E(E(_D1)[15]);},_D2=function(_D3,_D4,_){var _D5=jsGet(_D3,toJSStr(E(_D4)));return new T(function(){return fromJSStr(_D5);});},_D6=function(_D7,_D8,_){return _D2(E(_D7)[1],_D8,_);},_D9=unCStr("radio"),_Da=new T(function(){return A(_oS,[_6H]);}),_Db=unCStr("name"),_Dc=unCStr("true"),_Dd=function(_De,_Df,_Dg,_Dh){var _Di=new T(function(){return _ol(_Df);}),_Dj=new T(function(){return _3K([0,coercionToken],_3s(_Di),function(_Dk){return _qP(_Di,_Dk);},function(_Dl,_Dm){return _qS(_Di,_Dl,_Dm);});}),_Dn=new T(function(){return _3q(_Di);}),_Do=new T(function(){return _3q(_Di);}),_Dp=new T(function(){return _30(_Di);}),_Dq=new T(function(){return _30(_Di);}),_Dr=new T(function(){return _3q(_Di);}),_Ds=new T(function(){return _30(_Di);}),_Dt=new T(function(){return _3q(_Di);}),_Du=new T(function(){return _30(_Di);}),_Dv=new T(function(){return _qN(_De);}),_Dw=new T(function(){return _D0(_De);}),_Dx=new T(function(){return _qW(_Dh);});return function(_Dy,_Dz){return function(_DA){return A(_Dp,[new T(function(){return A(_Dj,[_DA]);}),function(_DB){var _DC=new T(function(){return E(E(_DB)[1]);}),_DD=new T(function(){return _op(_Df,function(_){return jsFind(toJSStr(E(_DC)));});});return A(_Du,[new T(function(){var _DE=new T(function(){return E(E(_DB)[2]);});return A(_Dt,[[0,_DE,_DE]]);}),function(_DF){return A(_Ds,[new T(function(){return A(_Dr,[[0,_A,new T(function(){var _DG=E(E(_DF)[1]);return [0,_DG[1],_DG[2],_qM,_DG[4],_DG[5],_DG[6]];})]]);}),function(_DH){return A(_Dq,[new T(function(){return A(_DD,[new T(function(){return E(E(_DH)[2]);})]);}),function(_DI){return A(_Dp,[new T(function(){var _DJ=E(_DI),_DK=_DJ[2],_DL=E(_DJ[1]);return _DL[0]==0?A(_Do,[[0,_g,_DK]]):A(_op,[_Df,function(_){return _D6(_DL[1],_6V,_);},_DK]);}),function(_DM){var _DN=new T(function(){return !_sq(E(_DM)[1],_Dc)?[0]:E([1,_Dy]);});return A(_Dn,[[0,[0,new T(function(){return A(_Dw,[new T(function(){return A(_Dv,[_DC,_D9,new T(function(){var _DO=A(_Dg,[_Dy]),_DP=E(_Da),_DQ=hs_eqWord64(_DO[1],_DP[1]);if(!E(_DQ)){return A(_Dx,[_Dy]);}else{var _DR=hs_eqWord64(_DO[2],_DP[2]);return E(_DR)==0?A(_Dx,[_Dy]):E(_Dy);}}),new T(function(){return E(_DN)[0]==0?false:true;}),_9]);}),[1,[0,_Db,_Dz],_g]]);}),new T(function(){var _DS=E(_DN);return _DS[0]==0?[0]:[1,_DS[1]];})],new T(function(){return E(E(_DM)[2]);})]]);}]);}]);}]);}]);}]);};};},_DT=new T(function(){return _6I(_oL,_oQ);}),_DU=new T(function(){return _y9(_xZ);}),_DV=new T(function(){return _Dd(_8F,_9L,_DT,_DU);}),_DW=function(_DX){var _DY=E(_DX);if(!_DY[0]){return [0];}else{var _DZ=_DY[1];return [1,function(_E0){var _E1=new T(function(){return _t1(new T(function(){return A(_DV,[_DZ,_E0]);}),_vf);});return function(_E2,_){var _E3=A(_E1,[_E2,_]),_E4=E(_E3),_E5=E(_E4[1]);return [0,[0,function(_E6,_){var _E7=_4S(_DZ,_E6,_),_E8=A(_E5[1],[_E6,_]);return _E6;},_E5[2]],_E4[2]];};},new T(function(){return _DW(_DY[2]);})];}},_E9=new T(function(){return _DW(_CZ);}),_Ea=function(_Eb){return E(E(_Eb)[1]);},_Ec=function(_Ed,_Ee){var _Ef=new T(function(){return _92(_Ee);}),_Eg=new T(function(){return _Ea(_Ef);}),_Eh=new T(function(){return _94(_Ef);}),_Ei=function(_Ej){var _Ek=E(_Ej);if(!_Ek[0]){return [0,_Eg,_9];}else{var _El=E(_Ek[1]),_Em=_Ei(_Ek[2]);return [0,new T(function(){return A(_Eh,[_El[1],_Em[1]]);}),new T(function(){var _En=E(_El[2]);return _En[0]==0?E(_Em[2]):E(_En);})];}},_Eo=new T(function(){return _3q(_Ed);}),_Ep=new T(function(){return _3K([0,coercionToken],_3s(_Ed),function(_Eq){return _qP(_Ed,_Eq);},function(_Er,_Es){return _qS(_Ed,_Er,_Es);});}),_Et=new T(function(){return _3q(_Ed);}),_Eu=new T(function(){return _30(_Ed);}),_Ev=new T(function(){return _30(_Ed);}),_Ew=new T(function(){return _30(_Ed);}),_Ex=new T(function(){return _30(_Ed);});return function(_Ey,_Ez){return A(_Ex,[new T(function(){return A(_Ep,[_Ez]);}),function(_EA){return A(_Ew,[new T(function(){var _EB=new T(function(){return E(E(_EA)[1]);}),_EC=function(_ED){var _EE=E(_ED);if(!_EE[0]){return function(_EF){return A(_Et,[[0,_g,_EF]]);};}else{var _EG=new T(function(){return _EC(_EE[2]);}),_EH=new T(function(){return A(_EE[1],[_EB]);});return function(_EI){return A(_Ev,[new T(function(){return A(_EH,[_EI]);}),function(_EJ){var _EK=new T(function(){return E(E(_EJ)[1]);});return A(_Eu,[new T(function(){return A(_EG,[new T(function(){return E(E(_EJ)[2]);})]);}),function(_EL){return A(_Et,[[0,[1,_EK,new T(function(){return E(E(_EL)[1]);})],new T(function(){return E(E(_EL)[2]);})]]);}]);}]);};}};return A(_EC,[_Ey,new T(function(){return E(E(_EA)[2]);})]);}),function(_EM){var _EN=new T(function(){var _EO=_Ei(E(_EM)[1]);return [0,_EO[1],_EO[2]];});return A(_Eo,[[0,[0,new T(function(){return E(E(_EN)[1]);}),new T(function(){var _EP=E(E(_EN)[2]);return _EP[0]==0?[0]:[1,_EP[1]];})],new T(function(){return E(E(_EM)[2]);})]]);}]);}]);};},_EQ=new T(function(){return _Ec(_2Z,_8F);}),_ER=new T(function(){return A(_EQ,[_E9]);}),_ES=function(_ET){var _EU=new T(function(){return _5c(_4S,new T(function(){return _2w(_xP,_ET,_g);}));});return function(_EV,_){return [0,[0,function(_EW,_){var _EX=_4S(_CL,_EW,_),_EY=A(_EU,[_EW,_]);return _EW;},_B7],_EV];};},_EZ=new T(function(){return _5c(_4S,_CW);}),_F0=unCStr("checkbox"),_F1=function(_F2,_F3){var _F4=new T(function(){return _ol(_F3);}),_F5=new T(function(){return _3K([0,coercionToken],_3s(_F4),function(_F6){return _qP(_F4,_F6);},function(_F7,_F8){return _qS(_F4,_F7,_F8);});}),_F9=new T(function(){return _3q(_F4);}),_Fa=new T(function(){return _3q(_F4);}),_Fb=new T(function(){return _30(_F4);}),_Fc=new T(function(){return _30(_F4);}),_Fd=new T(function(){return _3q(_F4);}),_Fe=new T(function(){return _30(_F4);}),_Ff=new T(function(){return _3q(_F4);}),_Fg=new T(function(){return _30(_F4);}),_Fh=new T(function(){return _qN(_F2);});return function(_Fi,_Fj){var _Fk=new T(function(){return !E(_Fi)?[0]:E(_Dc);});return function(_Fl){return A(_Fb,[new T(function(){return A(_F5,[_Fl]);}),function(_Fm){var _Fn=new T(function(){return E(E(_Fm)[1]);}),_Fo=new T(function(){return _op(_F3,function(_){return jsFind(toJSStr(E(_Fn)));});}),_Fp=new T(function(){return A(_Fh,[_Fn,_F0,_Fj,_Fi,_9]);});return A(_Fg,[new T(function(){var _Fq=new T(function(){return E(E(_Fm)[2]);});return A(_Ff,[[0,_Fq,_Fq]]);}),function(_Fr){return A(_Fe,[new T(function(){return A(_Fd,[[0,_A,new T(function(){var _Fs=E(E(_Fr)[1]);return [0,_Fs[1],_Fs[2],_qM,_Fs[4],_Fs[5],_Fs[6]];})]]);}),function(_Ft){return A(_Fc,[new T(function(){return A(_Fo,[new T(function(){return E(E(_Ft)[2]);})]);}),function(_Fu){return A(_Fb,[new T(function(){var _Fv=E(_Fu),_Fw=_Fv[2],_Fx=E(_Fv[1]);return _Fx[0]==0?A(_Fa,[[0,_Fk,_Fw]]):A(_op,[_F3,function(_){return _D6(_Fx[1],_6V,_);},_Fw]);}),function(_Fy){return A(_F9,[[0,[0,_Fp,[1,[0,new T(function(){return !_sq(E(_Fy)[1],_Dc)?[0]:E([1,_Fj,_g]);})]]],new T(function(){return E(E(_Fy)[2]);})]]);}]);}]);}]);}]);}]);};};},_Fz=new T(function(){return _F1(_8F,_9L);}),_FA=unCStr("Green"),_FB=new T(function(){return A(_Fz,[_0,_FA]);}),_FC=function(_FD,_){var _FE=A(_FB,[_FD,_]),_FF=E(_FE),_FG=E(_FF[1]);return [0,[0,function(_FH,_){var _FI=A(_FG[1],[_FH,_]),_FJ=A(_EZ,[_FH,_]);return _FH;},_FG[2]],_FF[2]];},_FK=new T(function(){return _t1(_FC,_vf);}),_FL=new T(function(){return _5c(_4S,_CU);}),_FM=new T(function(){return A(_Fz,[_0,_CU]);}),_FN=function(_FO,_){var _FP=A(_FM,[_FO,_]),_FQ=E(_FP),_FR=E(_FQ[1]);return [0,[0,function(_FS,_){var _FT=A(_FR[1],[_FS,_]),_FU=A(_FL,[_FS,_]);return _FS;},_FR[2]],_FQ[2]];},_FV=new T(function(){return _t1(_FN,_vf);}),_FW=new T(function(){return _5c(_4S,_CY);}),_FX=unCStr("Red"),_FY=new T(function(){return A(_Fz,[_0,_FX]);}),_FZ=function(_G0,_){var _G1=A(_FY,[_G0,_]),_G2=E(_G1),_G3=E(_G2[1]);return [0,[0,function(_G4,_){var _G5=A(_G3[1],[_G4,_]),_G6=A(_FW,[_G4,_]);return _G4;},_G3[2]],_G2[2]];},_G7=new T(function(){return _t1(_FZ,_vf);}),_G8=function(_G9,_){var _Ga=A(_G7,[_G9,_]),_Gb=E(_Ga),_Gc=E(_Gb[1]),_Gd=A(_FK,[_Gb[2],_]),_Ge=E(_Gd),_Gf=E(_Ge[1]),_Gg=A(_FV,[_Ge[2],_]),_Gh=E(_Gg),_Gi=E(_Gh[1]);return [0,[0,function(_Gj,_){var _Gk=A(_Gc[1],[_Gj,_]),_Gl=A(_Gf[1],[_Gj,_]),_Gm=A(_Gi[1],[_Gj,_]);return _Gj;},new T(function(){var _Gn=E(_Gc[2]);if(!_Gn[0]){return [0];}else{var _Go=E(_Gf[2]);if(!_Go[0]){return [0];}else{var _Gp=E(_Gi[2]);return _Gp[0]==0?[0]:[1,new T(function(){var _Gq=function(_Gr){var _Gs=E(_Gr);return _Gs[0]==0?E(new T(function(){var _Gt=function(_Gu){var _Gv=E(_Gu);return _Gv[0]==0?E(E(_Gp[1])[1]):[1,_Gv[1],new T(function(){return _Gt(_Gv[2]);})];};return _Gt(E(_Go[1])[1]);})):[1,_Gs[1],new T(function(){return _Gq(_Gs[2]);})];};return _Gq(E(_Gn[1])[1]);})];}}})],_Gh[2]];},_Gw=function(_Gx){var _Gy=new T(function(){return _5c(_4S,[1,_xH,new T(function(){return _xJ(_Gx,_CM);})]);});return function(_Gz,_){return [0,[0,function(_GA,_){var _GB=_4S(_CL,_GA,_),_GC=A(_Gy,[_GA,_]);return _GA;},_B7],_Gz];};},_GD=new T(function(){return _w0(_vM);}),_GE=function(_GF){return E(E(_GF)[11]);},_GG=function(_GH,_GI,_GJ,_GK){var _GL=new T(function(){return _ol(_GI);}),_GM=new T(function(){return _3s(_GL);}),_GN=new T(function(){return _3K([0,coercionToken],_GM,function(_GO){return _qP(_GL,_GO);},function(_GP,_GQ){return _qS(_GL,_GP,_GQ);});}),_GR=new T(function(){return _3q(_GL);}),_GS=new T(function(){return _30(_GL);}),_GT=new T(function(){return _30(_GL);}),_GU=new T(function(){return _3q(_GL);}),_GV=new T(function(){return _30(_GL);}),_GW=new T(function(){return _3q(_GL);}),_GX=new T(function(){return _30(_GL);}),_GY=new T(function(){return _30(_GL);}),_GZ=new T(function(){return _GE(_GH);});return function(_H0,_H1){return A(_GY,[new T(function(){return A(_GN,[_H1]);}),function(_H2){var _H3=new T(function(){return E(E(_H2)[1]);}),_H4=new T(function(){return _qk(_GM,function(_H5){return _op(_GI,_H5);},_GJ,_GK,_GH,_H3);});return A(_GX,[new T(function(){var _H6=new T(function(){return E(E(_H2)[2]);});return A(_GW,[[0,_H6,_H6]]);}),function(_H7){return A(_GV,[new T(function(){return A(_GU,[[0,_A,new T(function(){var _H8=E(E(_H7)[1]);return [0,_H8[1],_H8[2],_qM,_H8[4],_H8[5],_H8[6]];})]]);}),function(_H9){return A(_GT,[new T(function(){return A(_H4,[new T(function(){return E(E(_H9)[2]);})]);}),function(_Ha){return A(_GS,[new T(function(){return A(_H0,[new T(function(){return E(E(_Ha)[2]);})]);}),function(_Hb){var _Hc=E(_Hb);return A(_GR,[[0,[0,new T(function(){return A(_GZ,[_H3,E(_Hc[1])[1]]);}),new T(function(){var _Hd=E(E(_Ha)[1]);return _Hd[0]==2?[1,_Hd[1]]:[0];})],_Hc[2]]]);}]);}]);}]);}]);}]);};},_He=new T(function(){return _GG(_8F,_9L,_DT,_GD);}),_Hf=new T(function(){return _xJ(_CU,_CM);}),_Hg=new T(function(){return _xJ(_CU,_CM);}),_Hh=new T(function(){return A(_oS,[_6H]);}),_Hi=new T(function(){var _Hj=A(_DT,[_CU]),_Hk=E(_Hh),_Hl=hs_eqWord64(_Hj[1],_Hk[1]);if(!E(_Hl)){return [1,_xH,_Hf];}else{var _Hm=hs_eqWord64(_Hj[2],_Hk[2]);return E(_Hm)==0?[1,_xH,_Hg]:E(_CU);}}),_Hn=[0,_6T,_Hi],_Ho=[1,_Hn,_g],_Hp=new T(function(){return _Q(_7q,_Ho);}),_Hq=new T(function(){return _xJ(_CW,_CM);}),_Hr=new T(function(){return _xJ(_CW,_CM);}),_Hs=new T(function(){var _Ht=A(_DT,[_CW]),_Hu=E(_Hh),_Hv=hs_eqWord64(_Ht[1],_Hu[1]);if(!E(_Hv)){return [1,_xH,_Hq];}else{var _Hw=hs_eqWord64(_Ht[2],_Hu[2]);return E(_Hw)==0?[1,_xH,_Hr]:E(_CW);}}),_Hx=[0,_6T,_Hs],_Hy=[1,_Hx,_g],_Hz=new T(function(){return _Q(_7q,_Hy);}),_HA=new T(function(){return _xJ(_CY,_CM);}),_HB=new T(function(){return _xJ(_CY,_CM);}),_HC=new T(function(){var _HD=A(_DT,[_CY]),_HE=E(_Hh),_HF=hs_eqWord64(_HD[1],_HE[1]);if(!E(_HF)){return [1,_xH,_HA];}else{var _HG=hs_eqWord64(_HD[2],_HE[2]);return E(_HG)==0?[1,_xH,_HB]:E(_CY);}}),_HH=[0,_6T,_HC],_HI=[1,_HH,_g],_HJ=new T(function(){return _Q(_7q,_HI);}),_HK=function(_HL,_){var _HM=A(_HJ,[_HL,_]),_HN=_4S(_CY,_HM,_),_HO=A(_Hz,[_HL,_]),_HP=_4S(_CW,_HO,_),_HQ=A(_Hp,[_HL,_]),_HR=_4S(_CU,_HQ,_);return _HL;},_HS=[1,_CY],_HT=[0,_HK,_HS],_HU=function(_HV,_){return [0,_HT,_HV];},_HW=new T(function(){return A(_He,[_HU]);}),_HX=new T(function(){return _t1(_HW,_vf);}),_HY=function(_HZ,_){var _I0=_4f(_G8,_ES,_HZ,_),_I1=E(_I0),_I2=_4f(_ER,_CN,_I1[2],_),_I3=E(_I2),_I4=_4f(_HX,_Gw,_I3[2],_),_I5=E(_I4),_I6=E(_I5[1]);return [0,[0,function(_I7,_){var _I8=A(_CK,[_I7,_]),_I9=A(E(_I1[1])[1],[_I7,_]),_Ia=_5m(_I7,_),_Ib=_5m(_I7,_),_Ic=A(E(_I3[1])[1],[_I7,_]),_Id=_5m(_I7,_),_Ie=_5m(_I7,_),_If=A(_I6[1],[_I7,_]),_Ig=_5m(_I7,_);return _I7;},_I6[2]],_I5[2]];},_Ih=unCStr("This example draw a function of x between 10 and -10. You can define the function using javascript expressions"),_Ii=new T(function(){return _4Z(_4S,_Ih);}),_Ij=function(_Ik){var _Il=jsShow(E(_Ik)[1]);return fromJSStr(_Il);},_Im=function(_In){var _Io=new T(function(){return _Ij(_In);});return function(_ci){return _1G(_Io,_ci);};},_Ip=function(_Iq,_Ir,_Is){var _It=E(_Is);if(!_It[0]){return [0];}else{var _Iu=_It[2],_Iv=E(_It[1]);return _Iq!=_Iv[1]?[1,_Iv,new T(function(){return _Ip(_Iq,_Ir,_Iu);})]:_1G(_Ir,new T(function(){return _Ip(_Iq,_Ir,_Iu);}));}},_Iw=[0,45],_Ix=function(_Iy,_Iz,_IA){var _IB=new T(function(){return A(_Iy,[[0, -_IA]]);}),_IC=new T(function(){return E(_Iz)[1]<=6?function(_ID){return [1,_Iw,new T(function(){return A(_IB,[_ID]);})];}:function(_IE){return [1,_3E,[1,_Iw,new T(function(){return A(_IB,[[1,_3D,_IE]]);})]];};});if(_IA>=0){var _IF=isDoubleNegativeZero(_IA);return E(_IF)==0?A(_Iy,[[0,_IA]]):E(_IC);}else{return E(_IC);}},_IG=unCStr("canvas"),_IH=unCStr("id"),_II=unCStr("canvas"),_IJ=function(_IK,_IL){var _IM=new T(function(){return A(_IK,[_IL]);});return function(_IN,_){var _IO=jsCreateElem(toJSStr(E(_II))),_IP=jsAppendChild(_IO,E(_IN)[1]),_IQ=[0,_IO],_IR=A(_IM,[_IQ,_]);return _IQ;};},_IS=new T(function(){return _IJ(_uM,_2X);}),_IT=function(_IU,_){var _IV=A(_IS,[_IU,_]),_IW=A(_B,[_H,_IV,_IH,_IG,_]);return _IV;},_IX=[0,_IT,_B7],_IY=function(_IZ,_){return [0,_IX,_IZ];},_J0=unCStr("Pattern match failure in do expression at Main.hs:182:5-12"),_J1=function(_J2,_J3){while(1){var _J4=E(_J3);if(!_J4[0]){return false;}else{if(!A(_J2,[_J4[1]])){_J3=_J4[2];continue;}else{return true;}}}},_J5=unCStr("x*x+x+10;"),_J6=new T(function(){return [0,"(function(exp){ return eval(exp);})"];}),_J7=new T(function(){return _5(_J6);}),_J8=function(_J9,_){var _Ja=jsHasCtx2D(_J9);if(!E(_Ja)){return _9;}else{var _Jb=jsGetCtx2D(_J9);return [1,[0,[0,_Jb],[0,_J9]]];}},_Jc=function(_Jd,_){return _J8(E(_Jd)[1],_);},_Je=function(_Jf,_Jg){return A(_Jf,[function(_){var _Jh=jsFind(toJSStr(E(_Jg))),_Ji=E(_Jh);return _Ji[0]==0?_9:_Jc(_Ji[1],_);}]);},_Jj=new T(function(){return _Je(_H,_IG);}),_Jk=[0,-10],_Jl=[0,0],_Jm=[0,_Jk,_Jl],_Jn=[0,10],_Jo=[0,_Jn,_Jl],_Jp=[1,_Jo,_g],_Jq=[1,_Jm,_Jp],_Jr=function(_Js,_){return _A;},_Jt=function(_Ju){var _Jv=E(_Ju);if(!_Jv[0]){return E(_Jr);}else{var _Jw=E(_Jv[1]);return function(_Jx,_){var _Jy=E(_Jx)[1],_Jz=jsMoveTo(_Jy,E(_Jw[1])[1],E(_Jw[2])[1]);return (function(_JA,_){while(1){var _JB=E(_JA);if(!_JB[0]){return _A;}else{var _JC=E(_JB[1]),_JD=jsLineTo(_Jy,E(_JC[1])[1],E(_JC[2])[1]);_JA=_JB[2];continue;}}})(_Jv[2],_);};}},_JE=new T(function(){return _Jt(_Jq);}),_JF=[0,30],_JG=[0,_Jl,_JF],_JH=[0,-30],_JI=[0,_Jl,_JH],_JJ=[1,_JI,_g],_JK=[1,_JG,_JJ],_JL=new T(function(){return _Jt(_JK);}),_JM=function(_JN,_JO,_JP){while(1){var _JQ=E(_JO);if(!_JQ[0]){return true;}else{var _JR=E(_JP);if(!_JR[0]){return false;}else{if(!A(_bm,[_JN,_JQ[1],_JR[1]])){return false;}else{_JO=_JQ[2];_JP=_JR[2];continue;}}}}},_JS=unCStr("alert"),_JT=function(_JU){return _JM(_bl,_JS,_JU);},_JV=new T(function(){return [0,0/0];}),_JW=new T(function(){return [0,-1/0];}),_JX=new T(function(){return [0,1/0];}),_JY=[0,0],_JZ=function(_K0,_K1){while(1){var _K2=E(_K0);if(!_K2[0]){_K0=[1,I_fromInt(_K2[1])];continue;}else{var _K3=E(_K1);if(!_K3[0]){_K0=_K2;_K1=[1,I_fromInt(_K3[1])];continue;}else{return I_fromRat(_K2[1],_K3[1]);}}}},_K4=function(_K5,_K6){var _K7=E(_K5);if(!_K7[0]){var _K8=_K7[1],_K9=E(_K6);return _K9[0]==0?_K8==_K9[1]:I_compareInt(_K9[1],_K8)==0?true:false;}else{var _Ka=_K7[1],_Kb=E(_K6);return _Kb[0]==0?I_compareInt(_Ka,_Kb[1])==0?true:false:I_compare(_Ka,_Kb[1])==0?true:false;}},_Kc=function(_Kd,_Ke){var _Kf=E(_Kd);if(!_Kf[0]){var _Kg=_Kf[1],_Kh=E(_Ke);return _Kh[0]==0?_Kg<_Kh[1]:I_compareInt(_Kh[1],_Kg)>0;}else{var _Ki=_Kf[1],_Kj=E(_Ke);return _Kj[0]==0?I_compareInt(_Ki,_Kj[1])<0:I_compare(_Ki,_Kj[1])<0;}},_Kk=function(_Kl,_Km){return !_K4(_Km,_JY)?[0,_JZ(_Kl,_Km)]:!_K4(_Kl,_JY)?!_Kc(_Kl,_JY)?E(_JX):E(_JW):E(_JV);},_Kn=function(_Ko){var _Kp=E(_Ko);return _Kk(_Kp[1],_Kp[2]);},_Kq=function(_Kr){return [0,1/E(_Kr)[1]];},_Ks=function(_Kt){var _Ku=E(_Kt),_Kv=_Ku[1];return _Kv<0?[0, -_Kv]:E(_Ku);},_Kw=function(_Kx){var _Ky=E(_Kx);return _Ky[0]==0?_Ky[1]:I_toNumber(_Ky[1]);},_Kz=function(_KA){return [0,_Kw(_KA)];},_KB=[0,0],_KC=[0,1],_KD=[0,-1],_KE=function(_KF){var _KG=E(_KF)[1];return _KG!=0?_KG<=0?E(_KD):E(_KC):E(_KB);},_KH=function(_KI,_KJ){return [0,E(_KI)[1]-E(_KJ)[1]];},_KK=function(_KL){return [0, -E(_KL)[1]];},_KM=function(_KN,_KO){return [0,E(_KN)[1]+E(_KO)[1]];},_KP=function(_KQ,_KR){return [0,E(_KQ)[1]*E(_KR)[1]];},_KS=[0,_KM,_KP,_KH,_KK,_Ks,_KE,_Kz],_KT=function(_KU,_KV){return [0,E(_KU)[1]/E(_KV)[1]];},_KW=[0,_KS,_KT,_Kq,_Kn],_KX=function(_KY,_KZ){return E(_KY)[1]!=E(_KZ)[1]?true:false;},_L0=function(_L1,_L2){return E(_L1)[1]==E(_L2)[1];},_L3=[0,_L0,_KX],_L4=function(_L5,_L6){return E(_L5)[1]<E(_L6)[1];},_L7=function(_L8,_L9){return E(_L8)[1]<=E(_L9)[1];},_La=function(_Lb,_Lc){return E(_Lb)[1]>E(_Lc)[1];},_Ld=function(_Le,_Lf){return E(_Le)[1]>=E(_Lf)[1];},_Lg=function(_Lh,_Li){var _Lj=E(_Lh)[1],_Lk=E(_Li)[1];return _Lj>=_Lk?_Lj!=_Lk?2:1:0;},_Ll=function(_Lm,_Ln){var _Lo=E(_Lm),_Lp=E(_Ln);return _Lo[1]>_Lp[1]?E(_Lo):E(_Lp);},_Lq=function(_Lr,_Ls){var _Lt=E(_Lr),_Lu=E(_Ls);return _Lt[1]>_Lu[1]?E(_Lu):E(_Lt);},_Lv=[0,_L3,_Lg,_L4,_Ld,_La,_L7,_Ll,_Lq],_Lw=[0,1],_Lx=function(_Ly){return E(E(_Ly)[1]);},_Lz=function(_LA){return E(E(_LA)[2]);},_LB=function(_LC){return E(E(_LC)[6]);},_LD=[0,2],_LE=function(_LF,_LG){var _LH=E(_LG);return [1,_LH,new T(function(){var _LI=_Lx(_LF);return _LE(_LF,A(_LI[1],[_LH,new T(function(){return A(_LI[7],[_Lw]);})]));})];},_LJ=function(_LK,_LL){var _LM=E(_LL);if(!_LM[0]){return [0];}else{var _LN=_LM[1];return !A(_LK,[_LN])?[0]:[1,_LN,new T(function(){return _LJ(_LK,_LM[2]);})];}},_LO=function(_LP,_LQ,_LR,_LS){var _LT=new T(function(){return _LB(_LP);});return _LJ(function(_LU){return A(_LT,[_LU,new T(function(){var _LV=_Lx(_LQ),_LW=_LV[7];return A(_LV[1],[_LS,new T(function(){return A(_Lz,[_LQ,new T(function(){return A(_LW,[_Lw]);}),new T(function(){return A(_LW,[_LD]);})]);})]);})]);},_LE(_LQ,_LR));},_LX=new T(function(){return _LO(_Lv,_KW,_Jk,_Jn);}),_LY=function(_LZ){return [1,_LZ,new T(function(){var _M0=E(_LZ);return _M0[0]==0?[0]:_LY(_M0[2]);})];},_M1=function(_M2,_M3){var _M4=E(_M2);if(!_M4[0]){return [0];}else{var _M5=E(_M3);return _M5[0]==0?[0]:[1,[0,_M4[1],_M5[1]],new T(function(){return _M1(_M4[2],_M5[2]);})];}},_M6=function(_M7){var _M8=new T(function(){return !_J1(_JT,_LY(_M7))?E(_M7):E(_J5);}),_M9=function(_Ma,_){var _Mb=E(_Ma);if(!_Mb[0]){return _g;}else{var _Mc=A(_J7,[E(toJSStr(_Ip(120,new T(function(){return A(_Ix,[_Im,_oW,E(_Mb[1])[1],_g]);}),_M8))),_]),_Md=_M9(_Mb[2],_);return [1,[0,_Mc],_Md];}};return function(_ci,_yY){return _4f(_IY,function(_Me,_Be,_){return (function(_Mf,_){return [0,[0,function(_Mg,_){var _Mh=A(_Jj,[_]),_Mi=E(_Mh);if(!_Mi[0]){var _Mj=_2V(_J0,_);return _Mg;}else{var _Mk=_M9(_LX,_),_Ml=E(_Mi[1]),_Mm=jsResetCanvas(E(_Ml[2])[1]),_Mn=E(_Ml[1]),_Mo=_Mn[1],_Mp=jsPushState(_Mo),_Mq=jsScale(_Mo,3,1),_Mr=jsPushState(_Mo),_Ms=jsTranslate(_Mo,50,130),_Mt=jsPushState(_Mo),_Mu=jsRotate(_Mo,3.141592653589793),_Mv=jsBeginPath(_Mo),_Mw=A(_JE,[_Mn,_]),_Mx=A(_JL,[_Mn,_]),_My=A(_Jt,[_M1(_LX,_Mk),_Mn,_]),_Mz=jsStroke(_Mo),_MA=jsPopState(_Mo),_MB=jsPopState(_Mo),_MC=jsPopState(_Mo);return _Mg;}},_B7],_Mf];})(_Be,_);},_ci,_yY);};},_MD=[1,_J5],_ME=new T(function(){return _qY(_8F,_9L,_oS,_ye,_w2);}),_MF=new T(function(){return A(_ME,[_9,_9K,_MD]);}),_MG=new T(function(){return _t1(_MF,_9J);}),_MH=function(_MI,_){var _MJ=A(_MG,[_MI,_]),_MK=E(_MJ),_ML=E(_MK[1]);return [0,[0,function(_MM,_){var _MN=A(_ML[1],[_MM,_]),_MO=_5m(_MM,_);return _MM;},new T(function(){var _MP=E(_ML[2]);return _MP[0]==0?E(_MD):E(_MP);})],_MK[2]];},_MQ=function(_MR,_){var _MS=_4f(_MH,_M6,_MR,_),_MT=E(_MS),_MU=E(_MT[1]),_MV=new T(function(){return _v2(_uM,_MU[1]);});return [0,[0,function(_MW,_){var _MX=A(_Ii,[_MW,_]),_MY=A(_MV,[_MW,_]);return _MW;},_MU[2]],_MT[2]];},_MZ=unCStr("Do you "),_N0=new T(function(){return _5c(_4S,_MZ);}),_N1=unCStr("work?"),_N2=new T(function(){return _Dd(_8F,_9L,_DT,_DU);}),_N3=function(_N4){return E(E(_N4)[5]);},_N5=unCStr("for"),_N6=unCStr("label"),_N7=function(_N8,_N9,_Na,_Nb){var _Nc=new T(function(){return A(_N3,[_N9,_N6,_Na]);}),_Nd=new T(function(){return _92(_N9);}),_Ne=new T(function(){return _94(_Nd);}),_Nf=new T(function(){return _3q(_N8);}),_Ng=new T(function(){return _30(_N8);}),_Nh=new T(function(){return _D0(_N9);}),_Ni=new T(function(){return _3q(_N8);}),_Nj=new T(function(){return _3q(_N8);}),_Nk=new T(function(){return _30(_N8);}),_Nl=new T(function(){return _30(_N8);});return function(_Nm){return A(_Nl,[new T(function(){return A(_Nk,[new T(function(){return A(_Nj,[[0,_Nm,_Nm]]);}),function(_Nn){return A(_Ni,[[0,[1,_3y,new T(function(){var _No=E(E(_Nn)[1]);return _1G(_3F(0,E(_No[2])[1],_g),_No[1]);})],new T(function(){return E(E(_Nn)[2]);})]]);}]);}),function(_Np){var _Nq=new T(function(){return A(_Nh,[_Nc,[1,[0,_N5,new T(function(){return E(E(_Np)[1]);})],_g]]);});return A(_Ng,[new T(function(){return A(_Nb,[new T(function(){return E(E(_Np)[2]);})]);}),function(_Nr){var _Ns=E(_Nr),_Nt=E(_Ns[1]);return A(_Nf,[[0,[0,new T(function(){return A(_Ne,[_Nq,_Nt[1]]);}),_Nt[2]],_Ns[2]]]);}]);}]);};},_Nu=function(_Nv,_Nw){return _N7(_2Z,_8F,function(_Be,_){return _4S(_Nv,_Be,_);},new T(function(){return _t1(new T(function(){return A(_N2,[_Nv,_Nw]);}),_vf);}));},_Nx=function(_JU){return _Nu(_N1,_JU);},_Ny=unCStr("study?"),_Nz=function(_JU){return _Nu(_Ny,_JU);},_NA=[1,_Nz,_g],_NB=[1,_Nx,_NA],_NC=new T(function(){return A(_EQ,[_NB]);}),_ND=function(_NE,_){var _NF=A(_NC,[_NE,_]),_NG=E(_NF),_NH=E(_NG[1]);return [0,[0,function(_NI,_){var _NJ=A(_N0,[_NI,_]),_NK=A(_NH[1],[_NI,_]),_NL=_5m(_NI,_);return _NI;},_NH[2]],_NG[2]];},_NM=unCStr("do you enjoy your work? "),_NN=new T(function(){return _5c(_4S,_NM);}),_NO=function(_NP,_NQ,_){return [0,[0,_2X,[1,_NP]],_NQ];},_NR=function(_NS,_NT,_NU,_){return _4f(_NS,function(_NV){return E(_NT);},_NU,_);},_NW=function(_NX,_NY,_X,_){return _NR(_NX,_NY,_X,_);},_NZ=function(_O0){return err(_O0);},_O1=[0,_4f,_NW,_NO,_NZ],_O2=function(_O3,_O4,_O5,_O6,_O7,_O8){var _O9=new T(function(){return _94(_O3);});return A(_O4,[new T(function(){return A(_O6,[_O8]);}),function(_Oa){var _Ob=E(_Oa),_Oc=E(_Ob[1]);return A(_O4,[new T(function(){return A(_O7,[_Ob[2]]);}),function(_Od){var _Oe=E(_Od),_Of=E(_Oe[1]);return A(_O5,[[0,[0,new T(function(){return A(_O9,[_Oc[1],_Of[1]]);}),new T(function(){var _Og=E(_Oc[2]);return _Og[0]==0?E(_Of[2]):E(_Og);})],_Oe[2]]]);}]);}]);},_Oh=function(_Oi,_Oj,_Ok,_Ol,_Om,_On){var _Oo=new T(function(){return _D0(_Ok);});return A(_Oi,[new T(function(){return A(_Ol,[_On]);}),function(_Op){var _Oq=E(_Op),_Or=E(_Oq[1]);return A(_Oj,[[0,[0,new T(function(){return A(_Oo,[_Or[1],_Om]);}),_Or[2]],_Oq[2]]]);}]);},_Os=function(_Ot){return E(E(_Ot)[12]);},_Ou=function(_Ov,_Ow,_Ox,_Oy,_Oz,_OA,_OB){var _OC=new T(function(){return A(_Os,[_Ov,new T(function(){var _OD=A(_Ox,[_Oz]),_OE=E(_Hh),_OF=hs_eqWord64(_OD[1],_OE[1]);if(!E(_OF)){return A(_qW,[_Oy,_Oz]);}else{var _OG=hs_eqWord64(_OD[2],_OE[2]);return E(_OG)==0?A(_qW,[_Oy,_Oz]):E(_Oz);}}),_OA,_OB]);}),_OH=new T(function(){return _3q(_Ow);});return function(_OI){return A(_OH,[[0,[0,_OC,[1,_Oz]],_OI]]);};},_OJ=[0,_7s,_Dc],_OK=[1,_OJ,_g],_OL=[0,_7s,_Dc],_OM=[1,_OL,_g],_ON=function(_OO,_OP,_OQ,_OR){var _OS=new T(function(){return _GG(_OR,_OQ,_oS,_w2);}),_OT=new T(function(){return A(_3q,[_OO,_0]);}),_OU=new T(function(){return A(_3q,[_OO,_ec]);}),_OV=new T(function(){return _92(_OR);}),_OW=new T(function(){return _ol(_OQ);}),_OX=new T(function(){return _oC(_OR);}),_OY=new T(function(){return _30(_OO);});return function(_OZ,_P0,_P1){return A(_OY,[new T(function(){var _P2=new T(function(){return !E(_OZ)?E(_OM):[0];}),_P3=new T(function(){return _Ou(_OR,_OW,_oS,_ye,_P1,new T(function(){return A(_OX,[_P1]);}),_0);}),_P4=new T(function(){return !E(_OZ)?[0]:E(_OK);}),_P5=new T(function(){return _Ou(_OR,_OW,_oS,_ye,_P0,new T(function(){return A(_OX,[_P0]);}),_0);});return A(_OS,[function(_P6){var _P7=E(_OW);return _O2(_OV,_P7[1],_P7[3],function(_P8){var _P9=E(_OW);return _Oh(_P9[1],_P9[3],_OR,_P5,_P4,_P8);},function(_Pa){var _Pb=E(_OW);return _Oh(_Pb[1],_Pb[3],_OR,_P3,_P2,_Pa);},_P6);}]);}),function(_Pc){return !_sq(_Pc,_P0)?E(_OT):E(_OU);}]);};},_Pd=new T(function(){return _ON(_O1,_8O,_8F,_9L);}),_Pe=unCStr("yes"),_Pf=unCStr("no"),_Pg=new T(function(){return A(_Pd,[_ec,_Pe,_Pf]);}),_Ph=unCStr("ok"),_Pi=[1,_Ph],_Pj=new T(function(){return A(_yg,[_Pi]);}),_Pk=new T(function(){return _t1(_Pj,_vf);}),_Pl=function(_Pm,_){var _Pn=A(_Pg,[_Pm,_]),_Po=E(_Pn),_Pp=E(_Po[1]),_Pq=A(_Pk,[_Po[2],_]),_Pr=E(_Pq);return [0,[0,function(_Ps,_){var _Pt=A(_NN,[_Ps,_]),_Pu=A(_Pp[1],[_Ps,_]),_Pv=A(E(_Pr[1])[1],[_Ps,_]),_Pw=_5m(_Ps,_);return _Ps;},new T(function(){var _Px=E(_Pp[2]);return _Px[0]==0?[0]:[1,[0,_Px[1]]];})],_Pr[2]];},_Py=unCStr("do you study in "),_Pz=new T(function(){return _5c(_4S,_Py);}),_PA=unCStr("University"),_PB=function(_JU){return _Nu(_PA,_JU);},_PC=unCStr("High School"),_PD=function(_JU){return _Nu(_PC,_JU);},_PE=[1,_PD,_g],_PF=[1,_PB,_PE],_PG=new T(function(){return A(_EQ,[_PF]);}),_PH=function(_PI,_){var _PJ=A(_PG,[_PI,_]),_PK=E(_PJ),_PL=E(_PK[1]);return [0,[0,function(_PM,_){var _PN=A(_Pz,[_PM,_]),_PO=A(_PL[1],[_PM,_]);return _PM;},new T(function(){var _PP=E(_PL[2]);return _PP[0]==0?[0]:[1,[1,_PP[1]]];})],_PK[2]];},_PQ=new T(function(){return _aB("Main.hs:(289,11)-(296,64)|case");}),_PR=unCStr(" that you enjoy your work"),_PS=unCStr("False"),_PT=new T(function(){return _1G(_PS,_PR);}),_PU=unCStr("True"),_PV=new T(function(){return _1G(_PU,_PR);}),_PW=[0,32],_PX=function(_PY,_PZ){var _Q0=new T(function(){return _4Z(_4S,new T(function(){return unAppCStr("You are ",new T(function(){return _1G(_PY,[1,_PW,_PZ]);}));}));});return function(_ci,_yY){return _4f(_ND,function(_Q1){var _Q2=new T(function(){return !_sq(_Q1,_Ny)?!_sq(_Q1,_N1)?E(_PQ):E(_Pl):E(_PH);});return function(_ci,_yY){return _4f(_Q2,function(_Q3){return function(_Q4,_){var _Q5=A(new T(function(){var _Q6=E(_Q3);if(!_Q6[0]){var _Q7=new T(function(){return _4Z(_4S,new T(function(){return unAppCStr("You work and it is ",new T(function(){return !E(_Q6[1])?E(_PT):E(_PV);}));}));});return function(_Q8,_){return [0,[0,function(_Q9,_){var _Qa=A(_Q7,[_Q9,_]);return _Q9;},_9],_Q8];};}else{var _Qb=new T(function(){return _4Z(_4S,new T(function(){return unAppCStr("You study at the ",_Q6[1]);}));});return function(_Qc,_){return [0,[0,function(_Qd,_){var _Qe=A(_Qb,[_Qd,_]);return _Qd;},_9],_Qc];};}}),[_Q4,_]),_Qf=E(_Q5),_Qg=E(_Qf[1]);return [0,[0,function(_Qh,_){var _Qi=A(_Q0,[_Qh,_]),_Qj=A(_Qg[1],[_Qh,_]);return _Qh;},_Qg[2]],_Qf[2]];};},_ci,_yY);};},_ci,_yY);};},_Qk=function(_Ql){var _Qm=E(_Ql);return _PX(_Qm[1],_Qm[2]);},_Qn=unCStr("Who are you? "),_Qo=new T(function(){return _4Z(_4S,_Qn);}),_Qp=unCStr("name"),_Qq=unCStr("placeholder"),_Qr=[0,_Qq,_Qp],_Qs=[1,_Qr,_g],_Qt=unCStr("surname"),_Qu=[0,_Qq,_Qt],_Qv=[1,_Qu,_g],_Qw=[1,_Ph],_Qx=new T(function(){return A(_yg,[_Qw]);}),_Qy=new T(function(){return _t1(_Qx,_vf);}),_Qz=new T(function(){return A(_ME,[_9,_9K,_9]);}),_QA=new T(function(){return A(_ME,[_9,_9K,_9]);}),_QB=function(_QC,_){var _QD=A(_QA,[_QC,_]),_QE=E(_QD),_QF=E(_QE[1]),_QG=A(_Qz,[_QE[2],_]),_QH=E(_QG),_QI=E(_QH[1]),_QJ=A(_Qy,[_QH[2],_]),_QK=E(_QJ),_QL=new T(function(){return _Q(_QI[1],_Qv);}),_QM=new T(function(){return _Q(_QF[1],_Qs);});return [0,[0,function(_QN,_){var _QO=A(_Qo,[_QN,_]),_QP=A(_QM,[_QN,_]),_QQ=_5m(_QN,_),_QR=A(_QL,[_QN,_]),_QS=_5m(_QN,_),_QT=A(E(_QK[1])[1],[_QN,_]),_QU=_5m(_QN,_);return _QN;},new T(function(){var _QV=E(_QF[2]);if(!_QV[0]){return [0];}else{var _QW=E(_QI[2]);return _QW[0]==0?[0]:[1,[0,_QV[1],_QW[1]]];}})],_QK[2]];},_QX=unCStr("http://mflowdemo.herokuapp.com/noscript/monadicwidgets/combination"),_QY=unCStr("This formulary is the same than the one "),_QZ=[0,97],_R0=[1,_QZ,_g],_R1=function(_R2,_R3){var _R4=new T(function(){return A(_R2,[_R3]);});return function(_R5,_){var _R6=jsCreateElem(toJSStr(_R0)),_R7=jsAppendChild(_R6,E(_R5)[1]),_R8=[0,_R6],_R9=A(_R4,[_R8,_]);return _R8;};},_Ra=unCStr("run in the server by MFlow"),_Rb=new T(function(){return _R1(_4S,_Ra);}),_Rc=unCStr("href"),_Rd=function(_Re,_){var _Rf=_4S(_QY,_Re,_),_Rg=A(_Rb,[_Re,_]),_Rh=A(_B,[_H,_Rg,_Rc,_QX,_]);return _Re;},_Ri=new T(function(){return _4Z(_uM,_Rd);}),_Rj=unCStr("Fields of a form appear in sequence. Some of the fields trigger events instantly. Some others use a button to trigger them. It also contains option buttons, radio buttons etc"),_Rk=new T(function(){return _4Z(_4S,_Rj);}),_Rl=function(_Rm,_){var _Rn=_4f(_QB,_Qk,_Rm,_),_Ro=E(_Rn),_Rp=E(_Ro[1]);return [0,[0,function(_Rq,_){var _Rr=A(_Rk,[_Rq,_]),_Rs=A(_Ri,[_Rq,_]),_Rt=A(_Rp[1],[_Rq,_]);return _Rq;},_Rp[2]],_Ro[2]];},_Ru=unCStr("this example show a image gallery. It advances each 20 seconds and by pressing the button"),_Rv=new T(function(){return _4Z(_4S,_Ru);}),_Rw=[1,_5B],_Rx=unCStr("GalleryIndex"),_Ry=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_z7,_z8,_Rx],_Rz=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Ry,_g],_RA=function(_RB){return E(_Rz);},_RC=new T(function(){return _Bp(_13,_3x,_11,_RA);}),_RD=function(_RE,_){var _RF=A(_RC,[_RE,_]);return [0,[0,_vb,new T(function(){var _RG=E(E(_RF)[1]);return _RG[0]==0?E(_Rw):E(_RG);})],new T(function(){return E(E(_RF)[2]);})];},_RH=unCStr("100%"),_RI=[0,62],_RJ=[1,_RI,_g],_RK=[1,_RJ],_RL=new T(function(){return A(_yg,[_RK]);}),_RM=new T(function(){return _t1(_RL,_vf);}),_RN=function(_RO){return E(_RM);},_RP=unCStr("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"),_RQ=[1,_RP,_g],_RR=unCStr("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"),_RS=[1,_RR,_RQ],_RT=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"),_RU=[1,_RT,_RS],_RV=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"),_RW=[1,_RV,_RU],_RX=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"),_RY=[1,_RX,_RW],_RZ=unCStr("http://almaer.com/blog/uploads/interview-haskell.png"),_S0=[1,_RZ,_RY],_S1=unCStr("height"),_S2=unCStr("img"),_S3=function(_S4,_){var _S5=jsCreateElem(toJSStr(E(_S2))),_S6=jsAppendChild(_S5,E(_S4)[1]);return [0,_S5];},_S7=function(_S8,_S9){while(1){var _Sa=E(_S8);if(!_Sa[0]){return E(_S9);}else{_S8=_Sa[2];var _Sb=_S9+1|0;_S9=_Sb;continue;}}},_Sc=new T(function(){return [0,_S7(_S0,0)-1|0];}),_Sd=[0,_2X,_5q],_Se=unCStr("src"),_Sf=unCStr("width"),_Sg=function(_Sh){return function(_ci,_yY){return _4f(function(_Be,_){return _4f(_B5,function(_Si){return function(_Sj,_){return [0,_Sd,new T(function(){var _Sk=E(_Si);return [0,_Sk[1],_Sk[2],_Sk[3],_Sk[4],_Sk[5],new T(function(){return _AU(_Rz,new T(function(){var _Sl=E(_Sh)[1];return _Sl!=E(_Sc)[1]?[0,_Sl+1|0]:E(_5B);}),_Sk[6]);})];})];};},_Be,_);},function(_Sm,_Be,_){return (function(_Be,_){return _4f(function(_Sn,_){return [0,[0,function(_So,_){var _Sp=_S3(_So,_),_Sq=A(_B,[_H,_Sp,_Se,new T(function(){var _Sr=E(_Sh)[1];return _Sr>=0?_w7(_S0,_Sr):E(_w4);}),_]),_Ss=A(_B,[_H,_Sp,_Sf,_RH,_]),_St=A(_B,[_H,_Sp,_S1,_RH,_]),_Su=_5m(_So,_);return _So;},_B7],_Sn];},_RN,_Be,_);})(_Be,_);},_ci,_yY);};},_Sv=function(_Be,_){return _4f(_RD,_Sg,_Be,_);},_Sw=function(_Sx,_Sy,_){return _Sz(_Sy,_);},_SA=function(_Be,_){return _yz(_Sv,_Sw,_Be,_);},_SB=[0,20000],_SC=new T(function(){return _3K(_13,_3x,_11,_Y);}),_SD=function(_SE,_SF,_SG,_){var _SH=A(_SC,[_SG,_]),_SI=new T(function(){return E(E(_SH)[1]);}),_SJ=function(_){var _SK=jsSetTimeout(E(_SE)[1],function(_){var _SL=jsFind(toJSStr(E(_SI))),_SM=E(_SL);if(!_SM[0]){return _A;}else{var _SN=E(_SM[1]),_SO=E(_SN),_SP=jsClearChildren(_SN[1]),_SQ=E(_k)[1],_SR=takeMVar(_SQ),_SS=A(_SF,[_SR,_]),_ST=E(_SS),_SU=E(_ST[1]),_SV=_SU[1],_SW=_SU[2],_=putMVar(_SQ,new T(function(){var _SX=E(_ST[2]);return [0,_SX[1],_SX[2],_SX[3],_SX[4],_0,_SX[6]];}));if(!E(E(_SR)[5])){var _SY=A(_SV,[_SO,_]),_SZ=E(_SW);if(!_SZ[0]){return _SJ(_);}else{var _T0=E(_SZ[1]);return _A;}}else{var _T1=A(_7,[E(_SO[1]),_]),_T2=A(_SV,[[0,_T1],_]),_T3=E(_SW);if(!_T3[0]){return _SJ(_);}else{var _T4=E(_T3[1]);return _A;}}}});return _A;},_T5=_SJ(_);return _ym(_SI,_SF,new T(function(){return E(E(_SH)[2]);}),_);},_Sz=function(_T6,_){var _T7=_SD(_SB,_SA,_T6,_),_T8=E(_T7),_T9=E(_T8[1]);return [0,[0,function(_Ta,_){var _Tb=A(_Rv,[_Ta,_]),_Tc=A(_T9[1],[_Ta,_]);return _Ta;},_T9[2]],_T8[2]];},_Td=function(_Te){var _Tf=new T(function(){return _5c(_4S,new T(function(){return unAppCStr(" returns ",_Te);}));});return function(_Tg,_){return [0,[0,_Tf,_B7],_Tg];};},_Th=unCStr("This link say Hey!"),_Ti=function(_Be,_){return _4S(_Th,_Be,_);},_Tj=unCStr("Hey!"),_Tk=function(_){var _=0,_Tl=newMVar(),_=putMVar(_Tl,_9);return [0,_Tl];},_Tm=new T(function(){return _2(_Tk);}),_Tn=new T(function(){return _3K(_13,_3x,_11,_Y);}),_To=new T(function(){return A(_oS,[_6H]);}),_Tp=function(_Tq,_Tr,_){var _=putMVar(E(_Tq)[1],_Tr);return _A;},_Ts=function(_Tt,_Tu,_){return _s8(function(_){return _Tp(_Tm,_Tt,_);},_Tu,_);},_Tv=function(_){var _Tw=E(_Tm)[1],_Tx=takeMVar(_Tw),_=putMVar(_Tw,_Tx);return _Tx;},_Ty=function(_Tz,_TA,_TB,_TC){var _TD=new T(function(){return _R1(_uM,_TC);}),_TE=new T(function(){return unAppCStr("#/",new T(function(){var _TF=A(_TA,[_TB]),_TG=E(_To),_TH=hs_eqWord64(_TF[1],_TG[1]);if(!E(_TH)){return A(_qW,[_Tz,_TB]);}else{var _TI=hs_eqWord64(_TF[2],_TG[2]);return E(_TI)==0?A(_qW,[_Tz,_TB]):E(_TB);}}));});return function(_TJ,_){var _TK=A(_Tn,[_TJ,_]),_TL=0,_TM=function(_,_TN,_TO){var _TP=new T(function(){return E(E(_TK)[1]);}),_TQ=function(_TR,_){var _TS=A(_TD,[_TR,_]),_TT=A(_B,[_H,_TS,_Rc,_TE,_]),_TU=E(_TS),_TV=jsSetCB(_TU[1],E(_sH)[1],E([0,function(_TW,_TX,_){return (function(_){var _TY=0;if(!E(_TY)){return (function(_){var _TZ=takeMVar(E(_Tm)[1]),_U0=jsCatch(function(_){return (function(_){return [1,_TP];})();},function(_X,_){return _Ts(_TZ,_X,_);});return _Tp(_Tm,_U0,_);})();}else{var _U1=takeMVar(E(_Tm)[1]),_U2=jsCatch(function(_){return [1,_TP];},function(_X,_){return _Ts(_U1,_X,_);});return _Tp(_Tm,_U2,_);}})(_);}])[1]);return _TU;},_U3=E(_TN);return _U3[0]==0?[0,[0,_TQ,_9],_TO]:!_sq(_U3[1],_TP)?[0,[0,_TQ,_9],_TO]:[0,[0,_TQ,[1,_TB]],_TO];};if(!E(_TL)){var _U4=_Tv();return _TM(_,_U4,new T(function(){return E(E(_TK)[2]);}));}else{var _U5=E(_Tm)[1],_U6=takeMVar(_U5),_=putMVar(_U5,_U6);return _TM(_,_U6,new T(function(){return E(E(_TK)[2]);}));}};},_U7=new T(function(){return _Ty(_DU,_DT,_Tj,_Ti);}),_U8=new T(function(){return _t1(_U7,_vf);}),_U9=function(_Ua,_){var _Ub=A(_U8,[_Ua,_]),_Uc=E(_Ub),_Ud=E(_Uc[1]);return [0,[0,function(_Ue,_){var _Uf=_5m(_Ue,_),_Ug=A(_Ud[1],[_Ue,_]);return _Ue;},_Ud[2]],_Uc[2]];},_Uh=function(_){var _Ui=E(_s7)[1],_Uj=takeMVar(_Ui),_=putMVar(_Ui,_Uj);return _Uj;},_Uk=function(_Ul,_){var _Um=0;if(!E(_Um)){var _Un=_Uh();return [0,[0,_2X,[1,_Un]],_Ul];}else{var _Uo=E(_s7)[1],_Up=takeMVar(_Uo),_=putMVar(_Uo,_Up);return [0,[0,_2X,[1,_Up]],_Ul];}},_Uq=function(_Ur,_Us,_Ut){return A(_Ur,[[1,_2t,new T(function(){return A(_Us,[_Ut]);})]]);},_Uu=unCStr("Key "),_Uv=unCStr("Mouse "),_Uw=unCStr("Click "),_Ux=unCStr("NoData"),_Uy=function(_Uz){return _1G(_Ux,_Uz);},_UA=unCStr(": empty list"),_UB=unCStr("Prelude."),_UC=function(_UD){return err(_1G(_UB,new T(function(){return _1G(_UD,_UA);})));},_UE=unCStr("foldr1"),_UF=new T(function(){return _UC(_UE);}),_UG=function(_UH,_UI){var _UJ=E(_UI);if(!_UJ[0]){return E(_UF);}else{var _UK=_UJ[1],_UL=E(_UJ[2]);return _UL[0]==0?E(_UK):A(_UH,[_UK,new T(function(){return _UG(_UH,_UL);})]);}},_UM=[0,32],_UN=function(_UO,_UP){var _UQ=E(_UP);switch(_UQ[0]){case 0:return E(_Uy);case 1:var _UR=function(_US){return _3F(11,E(_UQ[1])[1],[1,_UM,new T(function(){var _UT=E(_UQ[2]);return [1,_3E,new T(function(){return A(_UG,[_Uq,[1,function(_UU){return _3F(0,E(_UT[1])[1],_UU);},[1,function(_UV){return _3F(0,E(_UT[2])[1],_UV);},_g]],[1,_3D,_US]]);})];})]);};return E(_UO)[1]<11?function(_UW){return _1G(_Uw,new T(function(){return _UR(_UW);}));}:function(_UX){return [1,_3E,new T(function(){return _1G(_Uw,new T(function(){return _UR([1,_3D,_UX]);}));})];};case 2:var _UY=function(_UZ){return _1G(_Uv,new T(function(){var _V0=E(_UQ[1]);return [1,_3E,new T(function(){return A(_UG,[_Uq,[1,function(_V1){return _3F(0,E(_V0[1])[1],_V1);},[1,function(_V2){return _3F(0,E(_V0[2])[1],_V2);},_g]],[1,_3D,_UZ]]);})];}));};return E(_UO)[1]<11?E(_UY):function(_V3){return [1,_3E,new T(function(){return _UY([1,_3D,_V3]);})];};default:var _V4=_UQ[1];return E(_UO)[1]<11?function(_V5){return _1G(_Uu,new T(function(){return _3F(11,E(_V4)[1],_V5);}));}:function(_V6){return [1,_3E,new T(function(){return _1G(_Uu,new T(function(){return _3F(11,E(_V4)[1],[1,_3D,_V6]);}));})];};}},_V7=function(_V8){var _V9=new T(function(){return _4Z(_4S,new T(function(){var _Va=E(_V8);return _1G(_Va[1],[1,_PW,new T(function(){return A(_UN,[_oW,_Va[2],_g]);})]);}));});return function(_Vb,_){return [0,[0,_V9,_B7],_Vb];};},_Vc=function(_Be,_){return _4f(_Uk,_V7,_Be,_);},_Vd=function(_Ve){return E(_Vc);},_Vf=[14,coercionToken],_Vg=[12,coercionToken],_Vh=[9,coercionToken],_Vi=[11,coercionToken],_Vj=[5,coercionToken],_Vk=[10,coercionToken],_Vl=[6,coercionToken],_Vm=[7,coercionToken],_Vn=unCStr("height:100px;background-color:lightgreen;position:relative"),_Vo=unCStr("div"),_Vp=function(_Vq,_Vr){var _Vs=new T(function(){return A(_Vq,[_Vr]);});return function(_Vt,_){var _Vu=jsCreateElem(toJSStr(E(_Vo))),_Vv=jsAppendChild(_Vu,E(_Vt)[1]),_Vw=[0,_Vu],_Vx=A(_Vs,[_Vw,_]);return _Vw;};},_Vy=unCStr("h1"),_Vz=function(_VA,_VB){var _VC=new T(function(){return A(_VA,[_VB]);});return function(_VD,_){var _VE=jsCreateElem(toJSStr(E(_Vy))),_VF=jsAppendChild(_VE,E(_VD)[1]),_VG=[0,_VE],_VH=A(_VC,[_VG,_]);return _VG;};},_VI=unCStr("Mouse events here"),_VJ=new T(function(){return _Vz(_4S,_VI);}),_VK=new T(function(){return _Vp(_uM,_VJ);}),_VL=function(_VM,_){var _VN=A(_VK,[_VM,_]),_VO=A(_B,[_H,_VN,_Cn,_Vn,_]);return _VN;},_VP=[0,_VL,_B7],_VQ=function(_VR,_){return [0,_VP,_VR];},_VS=new T(function(){return _t1(_VQ,_Vm);}),_VT=new T(function(){return _t1(_VS,_Vl);}),_VU=new T(function(){return _t1(_VT,_Vk);}),_VV=new T(function(){return _t1(_VU,_Vj);}),_VW=new T(function(){return _t1(_VV,_Vi);}),_VX=new T(function(){return _t1(_VW,_vf);}),_VY=new T(function(){return _t1(_VX,_Vh);}),_VZ=new T(function(){return _t1(_VY,_Vg);}),_W0=new T(function(){return _t1(_VZ,_Vf);}),_W1=new T(function(){return _t1(_W0,_9J);}),_W2=unCStr("http://todomvc.com"),_W3=unCStr("Work in progress for a todo application to be added to "),_W4=unCStr("todomvc.com"),_W5=new T(function(){return _R1(_4S,_W4);}),_W6=function(_W7,_){var _W8=_4S(_W3,_W7,_),_W9=A(_W5,[_W7,_]),_Wa=A(_B,[_H,_W9,_Rc,_W2,_]);return _W7;},_Wb=new T(function(){return _4Z(_uM,_W6);}),_Wc=unCStr("Tasks"),_Wd=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_z7,_z8,_Wc],_We=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_Wd,_g],_Wf=2,_Wg=function(_Wh,_Wi,_Wj,_Wk,_){var _Wl=A(_Wj,[_Wk,_]),_Wm=E(_Wl),_Wn=E(_Wm[1]),_Wo=_Wn[1];return [0,[0,function(_Wp,_){var _Wq=jsFind(toJSStr(E(_Wh))),_Wr=E(_Wq);if(!_Wr[0]){return _Wp;}else{var _Ws=_Wr[1];switch(E(_Wi)){case 0:var _Wt=A(_Wo,[_Ws,_]);return _Wp;case 1:var _Wu=E(_Ws),_Wv=_Wu[1],_Ww=jsGetChildren(_Wv),_Wx=E(_Ww);if(!_Wx[0]){var _Wy=A(_Wo,[_Wu,_]);return _Wp;}else{var _Wz=jsCreateElem(toJSStr(E(_3X))),_WA=jsAddChildBefore(_Wz,_Wv,E(_Wx[1])[1]),_WB=A(_Wo,[[0,_Wz],_]);return _Wp;}break;default:var _WC=E(_Ws),_WD=jsClearChildren(_WC[1]),_WE=A(_Wo,[_WC,_]);return _Wp;}}},_Wn[2]],_Wm[2]];},_WF=[0,_2X,_5q],_WG=function(_WH,_){return [0,_WF,_WH];},_WI=unCStr("Pattern match failure in do expression at Main.hs:345:7-25"),_WJ=new T(function(){return _NZ(_WI);}),_WK=function(_WL,_WM,_WN,_WO){return A(_WL,[new T(function(){return function(_){var _WP=jsSet(E(_WM)[1],toJSStr(E(_WN)),toJSStr(E(_WO)));return _A;};})]);},_WQ=unCStr("text"),_WR=unCStr("value"),_WS=new T(function(){return _6I(_oL,_oQ);}),_WT=new T(function(){return A(_WS,[_6H]);}),_WU=new T(function(){return A(_WS,[_6H]);}),_WV=unCStr("Prelude.read: ambiguous parse"),_WW=unCStr("Prelude.read: no parse"),_WX=function(_WY){return [1,function(_WZ){return A(_kx,[_WZ,function(_X0){return E([3,_WY,_bO]);}]);}];},_X1=function(_X2){while(1){var _X3=(function(_X4){var _X5=E(_X4);if(!_X5[0]){return [0];}else{var _X6=_X5[2],_X7=E(_X5[1]);if(!E(_X7[2])[0]){return [1,_X7[1],new T(function(){return _X1(_X6);})];}else{_X2=_X6;return null;}}})(_X2);if(_X3!=null){return _X3;}}},_X8=function(_X9,_Xa){var _Xb=_X1(_aE(A(E(_X9)[3],[_mO,_WX]),_Xa));return _Xb[0]==0?err(_WW):E(_Xb[2])[0]==0?E(_Xb[1]):err(_WV);},_Xc=function(_Xd,_Xe,_Xf,_Xg){var _Xh=new T(function(){return _qW(_Xe);}),_Xi=new T(function(){return _qY(_8F,_9L,_Xf,_Xe,_Xd);});return [0,function(_Xj){return A(_Xi,[[1,_Xg],_WQ,_Xj]);},function(_Xk,_){var _Xl=E(_Xg),_Xm=jsFind(toJSStr(_Xl)),_Xn=E(_Xm);return _Xn[0]==0?_45(_Xl):A(_WK,[_H,_Xn[1],_WR,new T(function(){var _Xo=A(_Xf,[_Xk]),_Xp=E(_WT),_Xq=hs_eqWord64(_Xo[1],_Xp[1]);if(!E(_Xq)){return A(_Xh,[_Xk]);}else{var _Xr=hs_eqWord64(_Xo[2],_Xp[2]);return E(_Xr)==0?A(_Xh,[_Xk]):E(_Xk);}}),_]);},function(_){var _Xs=E(_Xg),_Xt=jsFind(toJSStr(_Xs)),_Xu=E(_Xt);if(!_Xu[0]){return _45(_Xs);}else{var _Xv=_D2(E(_Xu[1])[1],_WR,_);return new T(function(){var _Xw=A(_WS,[_Xv]),_Xx=E(_WU),_Xy=hs_eqWord64(_Xw[1],_Xx[1]);if(!E(_Xy)){return _X8(_Xd,_Xv);}else{var _Xz=hs_eqWord64(_Xw[2],_Xx[2]);return E(_Xz)==0?_X8(_Xd,_Xv):E(_Xv);}});}}];},_XA=unCStr("todo"),_XB=new T(function(){var _XC=_Xc(_GD,_DU,_DT,_XA);return [0,_XC[1],_XC[2],_XC[3]];}),_XD=new T(function(){var _XE=A(E(_XB)[2],[_g]);return function(_XF,_){var _XG=A(_XE,[_]);return [0,[0,_2X,[1,_XG]],_XF];};}),_XH=[1,_g],_XI=function(_XJ){return E(_We);},_XK=new T(function(){return _Bp(_13,_3x,_11,_XI);}),_XL=function(_XM,_){var _XN=A(_XK,[_XM,_]);return [0,[0,_vb,new T(function(){var _XO=E(E(_XN)[1]);return _XO[0]==0?E(_XH):E(_XO);})],new T(function(){return E(E(_XN)[2]);})];},_XP=[0,_2X,_5q],_XQ=[0,_2X,_5q],_XR=function(_XS,_XT,_){return [0,_XQ,_XT];},_XU=[0,_2X,_5q],_XV=function(_XW,_){return [0,_XU,_XW];},_XX=unCStr("list"),_XY=unCStr("check"),_XZ=new T(function(){return A(_Fz,[_0,_XY]);}),_Y0=new T(function(){return _t1(_XZ,_vf);}),_Y1=function(_Y2,_){var _Y3=A(_Y0,[_Y2,_]),_Y4=E(_Y3),_Y5=E(_Y4[1]);return [0,[0,_Y5[1],new T(function(){var _Y6=E(_Y5[2]);return _Y6[0]==0?[0]:[1,E(_Y6[1])[1]];})],_Y4[2]];},_Y7=unCStr("text-decoration:line-through;"),_Y8=unCStr("li"),_Y9=function(_Ya,_Yb){var _Yc=new T(function(){return A(_Ya,[_Yb]);});return function(_Yd,_){var _Ye=jsCreateElem(toJSStr(E(_Y8))),_Yf=jsAppendChild(_Ye,E(_Yd)[1]),_Yg=[0,_Ye],_Yh=A(_Yc,[_Yg,_]);return _Yg;};},_Yi=function(_Yj){var _Yk=E(_Yj);if(!_Yk[0]){return [0];}else{var _Yl=new T(function(){return _5c(_4S,_Yk[1]);});return [1,function(_Ym,_){var _Yn=_4f(_Y1,function(_Yo){var _Yp=E(_Yo);return _Yp[0]==0?function(_Yq,_){return [0,[0,_Yl,_B7],_Yq];}:!_sq(_Yp[1],_XY)?function(_Yr,_){return [0,[0,_Yl,_B7],_Yr];}:E(_Yp[2])[0]==0?function(_Ys,_){return [0,[0,function(_Yt,_){var _Yu=A(_Yl,[_Yt,_]),_Yv=A(_B,[_H,_Yu,_Cn,_Y7,_]);return _Yu;},_B7],_Ys];}:function(_Yw,_){return [0,[0,_Yl,_B7],_Yw];};},_Ym,_),_Yx=E(_Yn),_Yy=E(_Yx[1]);return [0,[0,new T(function(){return _Y9(_uM,_Yy[1]);}),_Yy[2]],_Yx[2]];},new T(function(){return _Yi(_Yk[2]);})];}},_Yz=function(_YA,_YB){while(1){var _YC=(function(_YD,_YE){var _YF=E(_YE);if(!_YF[0]){return E(_YD);}else{_YA=function(_YG,_){var _YH=A(_YD,[_YG,_]),_YI=E(_YH),_YJ=E(_YI[1]),_YK=A(_YF[1],[_YI[2],_]),_YL=E(_YK),_YM=E(_YL[1]);return [0,[0,function(_YN,_){var _YO=A(_YJ[1],[_YN,_]),_YP=A(_YM[1],[_YN,_]);return _YN;},new T(function(){var _YQ=E(_YJ[2]);return _YQ[0]==0?E(_YM[2]):E(_YQ);})],_YL[2]];};_YB=_YF[2];return null;}})(_YA,_YB);if(_YC!=null){return _YC;}}},_YR=function(_YS,_YT,_){return _4f(_Uk,function(_YU){var _YV=E(E(_YU)[2]);return _YV[0]==3?E(E(_YV[1])[1])==13?function(_Be,_){return _4f(_XD,function(_YW){return function(_Be,_){return _4f(_XL,function(_YX){var _YY=new T(function(){return _Yz(_XV,_Yi([1,_YS,_YX]));});return function(_ci,_yY){return _4f(function(_Be,_){return _4f(_B5,function(_YZ){return function(_Z0,_){return [0,_XP,new T(function(){var _Z1=E(_YZ);return [0,_Z1[1],_Z1[2],_Z1[3],_Z1[4],_Z1[5],new T(function(){return _AU(_We,[1,_YS,_YX],_Z1[6]);})];})];};},_Be,_);},function(_Z2,_Be,_){return (function(_Be,_){return _4f(function(_Be,_){return _Wg(_XX,_Wf,_YY,_Be,_);},_XR,_Be,_);})(_Be,_);},_ci,_yY);};},_Be,_);};},_Be,_);}:E(_WG):E(_WJ);},_YT,_);},_Z3=new T(function(){return A(E(_XB)[1],[_9]);}),_Z4=new T(function(){return _t1(_Z3,_9J);}),_Z5=unCStr("todos"),_Z6=new T(function(){return _Vz(_4S,_Z5);}),_Z7=new T(function(){return _Vp(_uM,_2X);}),_Z8=function(_Z9,_){var _Za=_4f(_Z4,_YR,_Z9,_),_Zb=E(_Za),_Zc=E(_Zb[1]),_Zd=new T(function(){return _v2(_uM,function(_Ze,_){var _Zf=A(_Z6,[_Ze,_]),_Zg=A(_Zc[1],[_Ze,_]);return _Ze;});});return [0,[0,function(_Zh,_){var _Zi=A(_Zd,[_Zh,_]),_Zj=A(_Z7,[_Zh,_]),_Zk=A(_B,[_H,_Zj,_IH,_XX,_]);return _Zh;},new T(function(){var _Zl=E(_Zc[2]);return _Zl[0]==0?E(_B7):E(_Zl);})],_Zb[2]];},_Zm=function(_Zn,_Zo,_){return [0,[0,_2X,[1,[1,_Zn]]],_Zo];},_Zp=unCStr("revEntry"),_Zq=new T(function(){var _Zr=_Xc(_GD,_DU,_DT,_Zp);return [0,_Zr[1],_Zr[2],_Zr[3]];}),_Zs=new T(function(){return A(E(_Zq)[1],[_9]);}),_Zt=new T(function(){return _t1(_Zs,_9J);}),_Zu=function(_Zv,_Zw,_){return [0,[0,_2X,[1,[0,_Zv]]],_Zw];},_Zx=unCStr("entry"),_Zy=new T(function(){var _Zz=_Xc(_GD,_DU,_DT,_Zx);return [0,_Zz[1],_Zz[2],_Zz[3]];}),_ZA=new T(function(){return A(E(_Zy)[1],[_9]);}),_ZB=new T(function(){return _t1(_ZA,_9J);}),_ZC=function(_ZD,_){var _ZE=_4f(_ZB,_Zu,_ZD,_),_ZF=E(_ZE),_ZG=E(_ZF[1]),_ZH=_4f(_Zt,_Zm,_ZF[2],_),_ZI=E(_ZH),_ZJ=E(_ZI[1]);return [0,[0,new T(function(){return _v2(_uM,function(_ZK,_){var _ZL=A(_ZG[1],[_ZK,_]),_ZM=_5m(_ZK,_),_ZN=A(_ZJ[1],[_ZK,_]);return _ZK;});}),new T(function(){var _ZO=E(_ZG[2]);return _ZO[0]==0?E(_ZJ[2]):E(_ZO);})],_ZI[2]];},_ZP=unCStr("To search palindromes: one box present the other\'s reversed. It is also an example of cell usage"),_ZQ=new T(function(){return _4Z(_4S,_ZP);}),_ZR=function(_ZS){var _ZT=A(E(_Zq)[2],[_ZS]);return function(_ZU,_){var _ZV=A(_ZT,[_]);return [0,[0,_2X,[1,_ZV]],_ZU];};},_ZW=function(_ZX,_ZY){while(1){var _ZZ=E(_ZX);if(!_ZZ[0]){return E(_ZY);}else{_ZX=_ZZ[2];var _100=[1,_ZZ[1],_ZY];_ZY=_100;continue;}}},_101=function(_102){var _103=new T(function(){return _ZW(_102,_g);});return function(_104,_){return [0,[0,_2X,[1,_103]],_104];};},_105=new T(function(){var _106=E(E(_Zy)[3]);return function(_107,_){var _108=A(_106,[_]);return [0,[0,_2X,[1,_108]],_107];};}),_109=function(_Be,_){return _4f(_105,_101,_Be,_);},_10a=function(_Be,_){return _4f(_109,_ZR,_Be,_);},_10b=function(_10c){var _10d=A(E(_Zy)[2],[_10c]);return function(_10e,_){var _10f=A(_10d,[_]);return [0,[0,_2X,[1,_10f]],_10e];};},_10g=new T(function(){var _10h=E(E(_Zq)[3]);return function(_10i,_){var _10j=A(_10h,[_]);return [0,[0,_2X,[1,_10j]],_10i];};}),_10k=function(_10l){var _10m=new T(function(){return _ZW(_10l,_g);});return function(_10n,_){return [0,[0,_2X,[1,_10m]],_10n];};},_10o=function(_Be,_){return _4f(_10g,_10k,_Be,_);},_10p=function(_Be,_){return _4f(_10o,_10b,_Be,_);},_10q=function(_10r){return E(_10r)[0]==0?E(_10a):E(_10p);},_10s=function(_10t,_){var _10u=_4f(_ZC,_10q,_10t,_),_10v=E(_10u),_10w=E(_10v[1]);return [0,[0,function(_10x,_){var _10y=A(_ZQ,[_10x,_]),_10z=A(_10w[1],[_10x,_]);return _10x;},_10w[2]],_10v[2]];},_10A=unCStr("This widget sum recursively n numbers, but remember the previos entries when one entry is edited"),_10B=new T(function(){return _4Z(_4S,_10A);}),_10C=[0,_2X,_9],_10D=function(_10E,_){return [0,_10C,_10E];},_10F=function(_10G,_10H,_10I){var _10J=E(_10I);if(!_10J[0]){var _10K=_10J[3],_10L=_10J[4],_10M=_10J[5],_10N=E(_10J[2]),_10O=_10N[1];return _10G>=_10O?_10G!=_10O?_Af(_10N,_10K,_10L,_10F(_10G,_10H,_10M)):[0,_10J[1],E([0,_10G]),_10H,E(_10L),E(_10M)]:_zA(_10N,_10K,_10F(_10G,_10H,_10L),_10M);}else{return [0,1,E([0,_10G]),_10H,E(_f),E(_f)];}},_10P=function(_10Q,_10R,_10S){var _10T=E(_10Q),_10U=_10T[1],_10V=E(_10S);if(!_10V[0]){var _10W=_10V[3],_10X=_10V[4],_10Y=_10V[5],_10Z=E(_10V[2]),_110=_10Z[1];return _10U>=_110?_10U!=_110?_Af(_10Z,_10W,_10X,_10F(_10U,_10R,_10Y)):[0,_10V[1],E(_10T),_10R,E(_10X),E(_10Y)]:_zA(_10Z,_10W,_10F(_10U,_10R,_10X),_10Y);}else{return [0,1,E(_10T),_10R,E(_f),E(_f)];}},_111=function(_112,_113){while(1){var _114=E(_113);if(!_114[0]){var _115=E(_114[2])[1];if(_112>=_115){if(_112!=_115){_113=_114[5];continue;}else{return [1,_114[3]];}}else{_113=_114[4];continue;}}else{return [0];}}},_116=unCStr("containers-0.5.5.1"),_117=unCStr("Data.Map.Base"),_118=unCStr("Map"),_119=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_116,_117,_118],_11a=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_119,_g],_11b=function(_11c){return E(_11a);},_11d=function(_11e){var _11f=E(_11e);if(!_11f[0]){return [0];}else{var _11g=E(_11f[1]);return [1,[0,_11g[1],_11g[2]],new T(function(){return _11d(_11f[2]);})];}},_11h=function(_11i,_11j){return function(_11k){return E(new T(function(){var _11l=A(_11i,[_6H]),_11m=E(_11l[3]),_11n=_11m[1],_11o=_11m[2],_11p=_1G(_11l[4],[1,new T(function(){return A(_11j,[_6H]);}),_g]);if(!_11p[0]){return [0,_11n,_11o,_11m,_g];}else{var _11q=_6c(new T(function(){return _60(_6o(_6z,[1,[0,_11n,_11o],new T(function(){return _11d(_11p);})]));}));return [0,_11q[1],_11q[2],_11m,_11p];}}));};},_11r=new T(function(){return _11h(_11b,_oj);}),_11s=new T(function(){return _6I(_11r,_oj);}),_11t=new T(function(){return _Bp(_13,_3x,_11,_11s);}),_11u=function(_11v,_){var _11w=A(_11t,[_11v,_]);return [0,[0,_2X,new T(function(){return E(E(_11w)[1]);})],new T(function(){return E(E(_11w)[2]);})];},_11x=new T(function(){return _6I(_11r,_oj);}),_11y=[1,_f],_11z=new T(function(){return _Bp(_13,_3x,_11,_11x);}),_11A=function(_11B,_){var _11C=A(_11z,[_11B,_]);return [0,[0,_vb,new T(function(){var _11D=E(E(_11C)[1]);return _11D[0]==0?E(_11y):E(_11D);})],new T(function(){return E(E(_11C)[2]);})];},_11E=[0,_2X,_5q],_11F=[1,_9],_11G=function(_11H,_11I){var _11J=new T(function(){return [0,E(_11H)[1]+1|0];});return function(_ci,_yY){return _4f(function(_Be,_){return _4f(function(_11K,_){var _11L=_4f(_11u,function(_11M){var _11N=_111(E(_11H)[1],_11M);return _11N[0]==0?E(_10D):function(_11O,_){return [0,[0,_2X,_11N],_11O];};},_11K,_),_11P=E(_11L),_11Q=E(_11P[1]);return [0,[0,function(_11R,_){var _11S=A(_11Q[1],[_11R,_]);return _11R;},new T(function(){var _11T=E(_11Q[2]);return _11T[0]==0?E(_11F):[1,_11T];})],_11P[2]];},function(_11U){var _11V=new T(function(){return _t1(new T(function(){return A(_rJ,[_9,_9K,_11U]);}),_9J);});return function(_ci,_yY){return _4f(function(_11W,_){var _11X=A(_11V,[_11W,_]),_11Y=E(_11X),_11Z=_11Y[2],_120=E(_11Y[1]),_121=_120[1],_122=_120[2],_123=E(_11U);return _123[0]==0?[0,[0,function(_124,_){var _125=A(_121,[_124,_]);return _124;},_122],_11Z]:[0,[0,function(_126,_){var _127=A(_121,[_126,_]);return _126;},new T(function(){var _128=E(_122);return _128[0]==0?E(_123):E(_128);})],_11Z];},function(_129,_12a,_){return _4f(function(_Be,_){return _4f(_11A,function(_12b){var _12c=new T(function(){return _10P(_11H,_129,_12b);}),_12d=new T(function(){return A(_11x,[_12c]);});return function(_ci,_yY){return _4f(_B5,function(_12e){return function(_12f,_){return [0,_11E,new T(function(){var _12g=E(_12e);return [0,_12g[1],_12g[2],_12g[3],_12g[4],_12g[5],new T(function(){return _AU(_12d,_12c,_12g[6]);})];})];};},_ci,_yY);};},_Be,_);},function(_12h,_Be,_){return (function(_12i,_){return [0,[0,_2X,[1,_129]],_12i];})(_Be,_);},_12a,_);},_ci,_yY);};},_Be,_);},function(_12j){var _12k=new T(function(){return _11G(_11J,new T(function(){return _8U(_11I,_12j);}));}),_12l=new T(function(){return _5c(_4S,new T(function(){return _3F(0,E(_11I)[1]+E(_12j)[1]|0,_g);}));});return function(_ci,_yY){return _4f(function(_12m,_){return [0,[0,function(_12n,_){var _12o=A(_12l,[_12n,_]),_12p=_5m(_12n,_);return _12n;},_5q],_12m];},function(_12q){return E(_12k);},_ci,_yY);};},_ci,_yY);};},_12r=new T(function(){return _11G(_5B,_5B);}),_12s=unCStr("This widget sum recursively n numbers. When enters 0, present the result"),_12t=new T(function(){return _4Z(_4S,_12s);}),_12u=new T(function(){return A(_rJ,[_9,_9K,_9]);}),_12v=new T(function(){return _t1(_12u,_9J);}),_12w=function(_12x){var _12y=new T(function(){return _5c(_4S,new T(function(){return _58(_12x);}));});return function(_ci,_yY){return _4f(_12v,function(_12z){var _12A=E(E(_12z)[1]);if(!_12A){return function(_12B,_){return [0,[0,function(_12C,_){var _12D=_5m(_12C,_),_12E=_4S(_5r,_12C,_),_12F=A(_12y,[_12C,_]);return _12C;},_9],_12B];};}else{var _12G=new T(function(){return _12w(new T(function(){return [0,E(_12x)[1]+_12A|0];}));}),_12H=new T(function(){return _5c(_4S,new T(function(){return _3F(0,E(_12x)[1]+_12A|0,_g);}));});return function(_ci,_yY){return _4f(function(_12I,_){return [0,[0,function(_12J,_){var _12K=A(_12H,[_12J,_]),_12L=_5m(_12J,_);return _12J;},_5q],_12I];},function(_12M){return E(_12G);},_ci,_yY);};}},_ci,_yY);};},_12N=new T(function(){return _12w(_5B);}),_12O=unCStr("This widget sum two numbers and append the result. Using applicative and monadic expressions"),_12P=new T(function(){return _4Z(_4S,_12O);}),_12Q=function(_12R){return function(_12S,_){return [0,[0,new T(function(){var _12T=new T(function(){return _5c(_4S,new T(function(){return _58(_12R);}));});return _4Z(_uM,function(_12U,_){var _12V=_4S(_5r,_12U,_),_12W=A(_12T,[_12U,_]);return _12U;});}),_5q],_12S];};},_12X=new T(function(){return A(_rJ,[_9,_9K,_9]);}),_12Y=new T(function(){return _t1(_12X,_9J);}),_12Z=unCStr("second number "),_130=unCStr("first number"),_131=new T(function(){return A(_rJ,[_9,_9K,_9]);}),_132=new T(function(){return _t1(_131,_9J);}),_133=function(_134,_){var _135=A(_12Y,[_134,_]),_136=E(_135),_137=E(_136[1]),_138=A(_132,[_136[2],_]),_139=E(_138),_13a=E(_139[1]);return [0,[0,function(_13b,_){var _13c=_4S(_130,_13b,_),_13d=_5m(_13b,_),_13e=A(_137[1],[_13b,_]),_13f=_5m(_13b,_),_13g=_4S(_12Z,_13b,_),_13h=_5m(_13b,_),_13i=A(_13a[1],[_13b,_]),_13j=_5m(_13b,_);return _13b;},new T(function(){var _13k=E(_137[2]);if(!_13k[0]){return [0];}else{var _13l=E(_13a[2]);return _13l[0]==0?[0]:[1,new T(function(){return _8U(_13k[1],_13l[1]);})];}})],_139[2]];},_13m=function(_13n,_){var _13o=_4f(_133,_12Q,_13n,_),_13p=E(_13o),_13q=E(_13p[1]),_13r=new T(function(){return _4Z(_uM,_13q[1]);});return [0,[0,function(_13s,_){var _13t=A(_12P,[_13s,_]),_13u=A(_13r,[_13s,_]);return _13s;},_13q[2]],_13p[2]];},_13v=unCStr("table"),_13w=function(_13x,_13y){var _13z=new T(function(){return A(_13x,[_13y]);});return function(_13A,_){var _13B=jsCreateElem(toJSStr(E(_13v))),_13C=jsAppendChild(_13B,E(_13A)[1]),_13D=[0,_13B],_13E=A(_13z,[_13D,_]);return _13D;};},_13F=unCStr("hplayground examples"),_13G=new T(function(){return _Vz(_4S,_13F);}),_13H=unCStr("td"),_13I=function(_13J,_13K){var _13L=new T(function(){return A(_13J,[_13K]);});return function(_13M,_){var _13N=jsCreateElem(toJSStr(E(_13H))),_13O=jsAppendChild(_13N,E(_13M)[1]),_13P=[0,_13N],_13Q=A(_13L,[_13P,_]);return _13P;};},_13R=unCStr("tr"),_13S=function(_13T,_13U){var _13V=new T(function(){return A(_13T,[_13U]);});return function(_13W,_){var _13X=jsCreateElem(toJSStr(E(_13R))),_13Y=jsAppendChild(_13X,E(_13W)[1]),_13Z=[0,_13X],_140=A(_13V,[_13Z,_]);return _13Z;};},_141=unCStr("bottom of the page"),_142=new T(function(){return _5c(_4S,_141);}),_143=unCStr("h3"),_144=function(_145,_146){var _147=new T(function(){return A(_145,[_146]);});return function(_148,_){var _149=jsCreateElem(toJSStr(E(_143))),_14a=jsAppendChild(_149,E(_148)[1]),_14b=[0,_149],_14c=A(_147,[_14b,_]);return _14b;};},_14d=unCStr("https://github.com/agocorona/hplayground"),_14e=unCStr("   "),_14f=unCStr("https://github.com/agocorona/hplayground/blob/master/src/Main.hs"),_14g=unCStr("haskell-web.blogspot.com.es/2014/07/hplayground-translate-your-console.html"),_14h=unCStr("Git repository"),_14i=new T(function(){return _R1(_4S,_14h);}),_14j=unCStr("Examples code"),_14k=new T(function(){return _R1(_4S,_14j);}),_14l=unCStr("Article"),_14m=new T(function(){return _R1(_4S,_14l);}),_14n=function(_14o,_){var _14p=A(_14i,[_14o,_]),_14q=A(_B,[_H,_14p,_Rc,_14d,_]),_14r=_4S(_14e,_14o,_),_14s=A(_14k,[_14o,_]),_14t=A(_B,[_H,_14s,_Rc,_14f,_]),_14u=_4S(_14e,_14o,_),_14v=A(_14m,[_14o,_]),_14w=A(_B,[_H,_14v,_Rc,_14g,_]);return _14o;},_14x=new T(function(){return _v2(_uM,_14n);}),_14y=new T(function(){return _144(_uM,_14x);}),_14z=function(_14A,_){var _14B=_13m(_14A,_),_14C=E(_14B),_14D=E(_14C[1]),_14E=A(_v0,[_14C[2],_]),_14F=E(_14E),_14G=E(_14F[1]),_14H=A(_12N,[_14F[2],_]),_14I=E(_14H),_14J=E(_14I[1]),_14K=A(_C1,[_14I[2],_]),_14L=E(_14K),_14M=E(_14L[1]),_14N=_HY(_14L[2],_),_14O=E(_14N),_14P=E(_14O[1]),_14Q=_4f(_U9,_Td,_14O[2],_),_14R=E(_14Q),_14S=E(_14R[1]),_14T=A(_12r,[_14R[2],_]),_14U=E(_14T),_14V=E(_14U[1]),_14W=A(_CI,[_14U[2],_]),_14X=E(_14W),_14Y=E(_14X[1]),_14Z=_Rl(_14X[2],_),_150=E(_14Z),_151=E(_150[1]),_152=_10s(_150[2],_),_153=E(_152),_154=E(_153[1]),_155=_Z8(_153[2],_),_156=E(_155),_157=E(_156[1]),_158=_MQ(_156[2],_),_159=E(_158),_15a=E(_159[1]),_15b=_Sz(_159[2],_),_15c=E(_15b),_15d=E(_15c[1]),_15e=_4f(_W1,_Vd,_15c[2],_),_15f=E(_15e),_15g=E(_15f[1]),_15h=new T(function(){return _13w(_uM,function(_15i,_){var _15j=A(new T(function(){var _15k=new T(function(){return _13I(_uM,function(_15l,_){var _15m=A(_12t,[_15l,_]),_15n=A(_14J[1],[_15l,_]);return _15l;});}),_15o=new T(function(){return _13I(_uM,_14G[1]);}),_15p=new T(function(){return _13I(_uM,_14D[1]);});return _13S(_uM,function(_15q,_){var _15r=A(_15p,[_15q,_]),_15s=A(_B,[_H,_15r,_Cn,_4O,_]),_15t=A(_15o,[_15q,_]),_15u=A(_B,[_H,_15t,_Cn,_4O,_]),_15v=A(_15k,[_15q,_]),_15w=A(_B,[_H,_15v,_Cn,_4O,_]);return _15q;});}),[_15i,_]),_15x=A(_B,[_H,_15j,_Cn,_4Q,_]),_15y=A(new T(function(){var _15z=new T(function(){return _13I(_uM,_14Y[1]);}),_15A=new T(function(){return _13I(_uM,function(_15B,_){var _15C=A(_10B,[_15B,_]),_15D=A(_14V[1],[_15B,_]);return _15B;});}),_15E=new T(function(){return _13I(_uM,function(_15F,_){var _15G=A(_14M[1],[_15F,_]),_15H=A(_14P[1],[_15F,_]),_15I=A(_14S[1],[_15F,_]);return _15F;});});return _13S(_uM,function(_15J,_){var _15K=A(_15E,[_15J,_]),_15L=A(_B,[_H,_15K,_Cn,_4O,_]),_15M=A(_15A,[_15J,_]),_15N=A(_B,[_H,_15M,_Cn,_4O,_]),_15O=A(_15z,[_15J,_]),_15P=A(_B,[_H,_15O,_Cn,_4O,_]);return _15J;});}),[_15i,_]),_15Q=A(_B,[_H,_15y,_Cn,_4Q,_]),_15R=A(new T(function(){var _15S=new T(function(){return _13I(_uM,function(_15T,_){var _15U=A(_Wb,[_15T,_]),_15V=A(_157[1],[_15T,_]);return _15T;});}),_15W=new T(function(){return _13I(_uM,_154[1]);}),_15X=new T(function(){return _13I(_uM,new T(function(){return _v2(_uM,_151[1]);}));});return _13S(_uM,function(_15Y,_){var _15Z=A(_15X,[_15Y,_]),_160=A(_B,[_H,_15Z,_Cn,_4O,_]),_161=A(_15W,[_15Y,_]),_162=A(_B,[_H,_161,_Cn,_4O,_]),_163=A(_15S,[_15Y,_]),_164=A(_B,[_H,_163,_Cn,_4O,_]);return _15Y;});}),[_15i,_]),_165=A(_B,[_H,_15R,_Cn,_4Q,_]),_166=A(new T(function(){var _167=new T(function(){return _13I(_uM,_15g[1]);}),_168=new T(function(){return _13I(_uM,_15d[1]);}),_169=new T(function(){return _13I(_uM,_15a[1]);});return _13S(_uM,function(_16a,_){var _16b=A(_169,[_16a,_]),_16c=A(_B,[_H,_16b,_Cn,_4O,_]),_16d=A(_168,[_16a,_]),_16e=A(_B,[_H,_16d,_Cn,_4O,_]),_16f=A(_167,[_16a,_]),_16g=A(_B,[_H,_16f,_Cn,_4O,_]);return _16a;});}),[_15i,_]),_16h=A(_B,[_H,_166,_Cn,_4Q,_]);return _15i;});});return [0,[0,function(_16i,_){var _16j=A(_13G,[_16i,_]),_16k=A(_B,[_H,_16j,_Cn,_Cm,_]),_16l=A(_14y,[_16i,_]),_16m=A(_15h,[_16i,_]),_16n=A(_B,[_H,_16m,_Cn,_4P,_]),_16o=A(_142,[_16i,_]);return _16i;},new T(function(){var _16p=E(_14D[2]);if(!_16p[0]){var _16q=E(_14G[2]);if(!_16q[0]){var _16r=E(_14J[2]);if(!_16r[0]){var _16s=E(_14M[2]);if(!_16s[0]){var _16t=E(_14P[2]);if(!_16t[0]){var _16u=E(_14S[2]);if(!_16u[0]){var _16v=E(_14V[2]);if(!_16v[0]){var _16w=E(_14Y[2]);if(!_16w[0]){var _16x=E(_151[2]);if(!_16x[0]){var _16y=E(_154[2]);if(!_16y[0]){var _16z=E(_157[2]);if(!_16z[0]){var _16A=E(_15a[2]);if(!_16A[0]){var _16B=E(_15d[2]);return _16B[0]==0?E(_15g[2]):E(_16B);}else{return E(_16A);}}else{return E(_16z);}}else{return E(_16y);}}else{return E(_16x);}}else{return E(_16w);}}else{return E(_16v);}}else{return E(_16u);}}else{return E(_16t);}}else{return E(_16s);}}else{return E(_16r);}}else{return E(_16q);}}else{return E(_16p);}})],_15f[2]];},_16C=unCStr("idelem"),_16D=function(_){var _16E=E(_16C),_16F=jsFind(toJSStr(_16E)),_16G=E(_16F);return _16G[0]==0?_45(_16E):_l(_14z,_16G[1],_);},_16H=function(_){return _16D(_);};
var hasteMain = function() {A(_16H, [0]);};window.onload = hasteMain;