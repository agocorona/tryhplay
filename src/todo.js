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

var _0=false,_1=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_2=function(_3){var _4=A(_3,[_]);return E(_4);},_5=function(_6){return _2(function(_){var _=0;return eval(E(_6)[1]);});},_7=new T(function(){return _5(_1);}),_8=2,_9=[1],_a=[0],_b=[0],_c=function(_d,_){return _b;},_e=function(_){return _b;},_f=[0,_e,_c],_g=[0,0],_h=[0,_a,_g,_8,_f,_0,_9],_i=function(_){var _=0,_j=newMVar(),_=putMVar(_j,_h);return [0,_j];},_k=new T(function(){return _2(_i);}),_l=function(_m,_n,_){var _o=E(_k)[1],_p=takeMVar(_o),_q=A(_m,[_p,_]),_r=E(_q),_s=E(_r[1]),_t=_s[1],_u=_s[2],_=putMVar(_o,new T(function(){var _v=E(_r[2]);return [0,_v[1],_v[2],_v[3],_v[4],_0,_v[6]];}));if(!E(E(_p)[5])){var _w=A(_t,[_n,_]);return _u;}else{var _x=A(_7,[E(E(_n)[1]),_]),_y=A(_t,[[0,_x],_]);return _u;}},_z=unCStr("main"),_A=0,_B=function(_C,_D,_E,_F){return A(_C,[new T(function(){return function(_){var _G=jsSetAttr(E(_D)[1],toJSStr(E(_E)),toJSStr(E(_F)));return _A;};})]);},_H=2,_I=1,_J=unCStr("span"),_K=function(_L,_M,_N,_O,_){var _P=A(_N,[_O,_]),_Q=E(_P),_R=E(_Q[1]),_S=_R[1];return [0,[0,function(_T,_){var _U=jsFind(toJSStr(E(_L))),_V=E(_U);if(!_V[0]){return _T;}else{var _W=_V[1];switch(E(_M)){case 0:var _X=A(_S,[_W,_]);return _T;case 1:var _Y=E(_W),_Z=_Y[1],_10=jsGetChildren(_Z),_11=E(_10);if(!_11[0]){var _12=A(_S,[_Y,_]);return _T;}else{var _13=jsCreateElem(toJSStr(E(_J))),_14=jsAddChildBefore(_13,_Z,E(_11[1])[1]),_15=A(_S,[[0,_13],_]);return _T;}break;default:var _16=E(_W),_17=jsClearChildren(_16[1]),_18=A(_S,[_16,_]);return _T;}}},_R[2]],_Q[2]];},_19=function(_1a){return E(_1a);},_1b=unCStr("id"),_1c=unCStr("todoapp"),_1d=unCStr("clear-holder"),_1e=unCStr("filters"),_1f=unCStr("todo-count"),_1g=unCStr("header"),_1h=unCStr("id"),_1i=function(_1j,_1k,_1l,_){var _1m=E(_1k),_1n=A(_1j,[_1l,_]),_1o=A(_B,[_19,_1n,_1m[1],_1m[2],_]);return _1n;},_1p=function(_1q,_1r){while(1){var _1s=(function(_1t,_1u){var _1v=E(_1u);if(!_1v[0]){return E(_1t);}else{_1q=function(_1w,_){return _1i(_1t,_1v[1],_1w,_);};_1r=_1v[2];return null;}})(_1q,_1r);if(_1s!=null){return _1s;}}},_1x=function(_1y,_1z,_){var _1A=jsCreateElem(toJSStr(E(_1y))),_1B=jsAppendChild(_1A,E(_1z)[1]);return [0,_1A];},_1C=function(_1w,_){return _1x(_J,_1w,_);},_1D=[0,coercionToken],_1E=function(_1F,_1G,_){return [0,_A,_1F];},_1H=function(_1I,_){return [0,_1I,_1I];},_1J=function(_1K,_1L,_){var _1M=A(_1K,[_]);return A(_1L,[_]);},_1N=function(_1O,_1P,_){return _1J(_1O,_1P,_);},_1Q=function(_1R,_1S,_){var _1T=A(_1R,[_]);return A(_1S,[_1T,_]);},_1U=unCStr("base"),_1V=unCStr("GHC.IO.Exception"),_1W=unCStr("IOException"),_1X=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1U,_1V,_1W],_1Y=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1X,_a],_1Z=function(_20){return E(_1Y);},_21=function(_22){return E(E(_22)[1]);},_23=unCStr("Maybe.fromJust: Nothing"),_24=new T(function(){return err(_23);}),_25=function(_26,_27,_28){var _29=new T(function(){var _2a=A(_26,[_28]),_2b=A(_27,[new T(function(){var _2c=E(_29);return _2c[0]==0?E(_24):E(_2c[1]);})]),_2d=hs_eqWord64(_2a[1],_2b[1]);if(!E(_2d)){return [0];}else{var _2e=hs_eqWord64(_2a[2],_2b[2]);return E(_2e)==0?[0]:[1,_28];}});return E(_29);},_2f=function(_2g){var _2h=E(_2g);return _25(_21(_2h[1]),_1Z,_2h[2]);},_2i=unCStr(": "),_2j=[0,41],_2k=unCStr(" ("),_2l=function(_2m,_2n){var _2o=E(_2m);return _2o[0]==0?E(_2n):[1,_2o[1],new T(function(){return _2l(_2o[2],_2n);})];},_2p=unCStr("already exists"),_2q=unCStr("does not exist"),_2r=unCStr("protocol error"),_2s=unCStr("failed"),_2t=unCStr("invalid argument"),_2u=unCStr("inappropriate type"),_2v=unCStr("hardware fault"),_2w=unCStr("unsupported operation"),_2x=unCStr("timeout"),_2y=unCStr("resource vanished"),_2z=unCStr("interrupted"),_2A=unCStr("resource busy"),_2B=unCStr("resource exhausted"),_2C=unCStr("end of file"),_2D=unCStr("illegal operation"),_2E=unCStr("permission denied"),_2F=unCStr("user error"),_2G=unCStr("unsatisified constraints"),_2H=unCStr("system error"),_2I=function(_2J,_2K){switch(E(_2J)){case 0:return _2l(_2p,_2K);case 1:return _2l(_2q,_2K);case 2:return _2l(_2A,_2K);case 3:return _2l(_2B,_2K);case 4:return _2l(_2C,_2K);case 5:return _2l(_2D,_2K);case 6:return _2l(_2E,_2K);case 7:return _2l(_2F,_2K);case 8:return _2l(_2G,_2K);case 9:return _2l(_2H,_2K);case 10:return _2l(_2r,_2K);case 11:return _2l(_2s,_2K);case 12:return _2l(_2t,_2K);case 13:return _2l(_2u,_2K);case 14:return _2l(_2v,_2K);case 15:return _2l(_2w,_2K);case 16:return _2l(_2x,_2K);case 17:return _2l(_2y,_2K);default:return _2l(_2z,_2K);}},_2L=[0,125],_2M=unCStr("{handle: "),_2N=function(_2O,_2P,_2Q,_2R,_2S,_2T){var _2U=new T(function(){var _2V=new T(function(){return _2I(_2P,new T(function(){var _2W=E(_2R);return _2W[0]==0?E(_2T):_2l(_2k,new T(function(){return _2l(_2W,[1,_2j,_2T]);}));}));}),_2X=E(_2Q);return _2X[0]==0?E(_2V):_2l(_2X,new T(function(){return _2l(_2i,_2V);}));}),_2Y=E(_2S);if(!_2Y[0]){var _2Z=E(_2O);if(!_2Z[0]){return E(_2U);}else{var _30=E(_2Z[1]);return _30[0]==0?_2l(_2M,new T(function(){return _2l(_30[1],[1,_2L,new T(function(){return _2l(_2i,_2U);})]);})):_2l(_2M,new T(function(){return _2l(_30[1],[1,_2L,new T(function(){return _2l(_2i,_2U);})]);}));}}else{return _2l(_2Y[1],new T(function(){return _2l(_2i,_2U);}));}},_31=function(_32){var _33=E(_32);return _2N(_33[1],_33[2],_33[3],_33[4],_33[6],_a);},_34=function(_35,_36){var _37=E(_35);return _2N(_37[1],_37[2],_37[3],_37[4],_37[6],_36);},_38=[0,44],_39=[0,93],_3a=[0,91],_3b=function(_3c,_3d,_3e){var _3f=E(_3d);return _3f[0]==0?unAppCStr("[]",_3e):[1,_3a,new T(function(){return A(_3c,[_3f[1],new T(function(){var _3g=function(_3h){var _3i=E(_3h);return _3i[0]==0?E([1,_39,_3e]):[1,_38,new T(function(){return A(_3c,[_3i[1],new T(function(){return _3g(_3i[2]);})]);})];};return _3g(_3f[2]);})]);})];},_3j=function(_3k,_3l){return _3b(_34,_3k,_3l);},_3m=function(_3n,_3o,_3p){var _3q=E(_3o);return _2N(_3q[1],_3q[2],_3q[3],_3q[4],_3q[6],_3p);},_3r=[0,_3m,_31,_3j],_3s=new T(function(){return [0,_1Z,_3r,_3t,_2f];}),_3t=function(_3u){return [0,_3s,_3u];},_3v=7,_3w=function(_3x){return [0,_b,_3v,_a,_3x,_b,_b];},_3y=function(_3z,_){return die(new T(function(){return _3t(new T(function(){return _3w(_3z);}));}));},_3A=function(_3B,_){return _3y(_3B,_);},_3C=function(_3D,_){return _3D;},_3E=[0,_1Q,_1N,_3C,_3A],_3F=function(_3G){return E(E(_3G)[1]);},_3H=function(_3I,_3J,_3K,_3L){return A(_3F,[_3I,new T(function(){return A(_3J,[_3L]);}),function(_3M){return A(_3K,[new T(function(){return E(E(_3M)[1]);}),new T(function(){return E(E(_3M)[2]);})]);}]);},_3N=function(_3O,_3P,_3Q,_3R){return A(_3F,[_3O,new T(function(){return A(_3P,[_3R]);}),function(_3S){return A(_3Q,[new T(function(){return E(E(_3S)[2]);})]);}]);},_3T=function(_3U,_3V,_3W,_3X){return _3N(_3U,_3V,_3W,_3X);},_3Y=function(_3Z){return E(E(_3Z)[4]);},_40=function(_41,_42){var _43=new T(function(){return A(_3Y,[_41,_42]);});return function(_44){return E(_43);};},_45=function(_46){return E(E(_46)[3]);},_47=function(_48){var _49=new T(function(){return _45(_48);});return [0,function(_3V,_3W,_3X){return _3H(_48,_3V,_3W,_3X);},function(_3V,_3W,_3X){return _3T(_48,_3V,_3W,_3X);},function(_4a,_4b){return A(_49,[[0,_4a,_4b]]);},function(_3X){return _40(_48,_3X);}];},_4c=new T(function(){return _47(_3E);}),_4d=[0,112],_4e=function(_4f,_4g){var _4h=jsShowI(_4f);return _2l(fromJSStr(_4h),_4g);},_4i=[0,41],_4j=[0,40],_4k=function(_4l,_4m,_4n){return _4m>=0?_4e(_4m,_4n):_4l<=6?_4e(_4m,_4n):[1,_4j,new T(function(){var _4o=jsShowI(_4m);return _2l(fromJSStr(_4o),[1,_4i,_4n]);})];},_4p=function(_4q,_4r,_4s,_4t){var _4u=E(_4r);return A(_4u[1],[new T(function(){var _4v=E(_4q);return E(_4s);}),function(_4w){var _4x=new T(function(){return E(E(_4w)[2]);});return A(_4u[2],[new T(function(){return A(_4t,[new T(function(){var _4y=E(new T(function(){var _4z=E(_4q);return [0,coercionToken];})),_4A=E(_4w);return [0,_4A[1],new T(function(){return [0,E(_4x)[1]+1|0];}),_4A[3],_4A[4],_4A[5],_4A[6]];})]);}),new T(function(){return A(_4u[3],[[1,_4d,new T(function(){return _2l(_4k(0,E(_4x)[1],_a),new T(function(){return E(E(_4w)[1]);}));})]]);})]);}]);},_4B=new T(function(){return _4p(_1D,_4c,_1H,_1E);}),_4C=unCStr(" could be found!"),_4D=function(_4E){return err(unAppCStr("No element with ID ",new T(function(){return _2l(_4E,_4C);})));},_4F=function(_4G,_4H,_){var _4I=E(_4H),_4J=jsFind(toJSStr(_4I)),_4K=E(_4J);if(!_4K[0]){return _4D(_4I);}else{var _4L=E(_4K[1]),_4M=jsClearChildren(_4L[1]);return _l(_4G,_4L,_);}},_4N=function(_4O,_4P,_4Q,_4R,_4S,_4T,_4U,_4V,_4W,_){var _4X=E(_4U);return [0,_4X,[0,_4R,_4S,_4T,[0,function(_){return _4F(function(_4Y,_){var _4Z=A(_4O,[new T(function(){var _50=E(_4Y);return [0,_50[1],_4S,_50[3],_50[4],_50[5],_50[6]];}),_]);return [0,[0,_3C,E(E(_4Z)[1])[2]],_4Y];},_4Q,_);},function(_51,_){var _52=_4F(new T(function(){return A(_4P,[_51]);}),_4Q,_),_53=E(_52);return _53[0]==0?_b:A(_4X[2],[_53[1],_]);}],_4V,_4W]];},_54=function(_55,_56,_57,_){var _58=A(_4B,[_57,_]),_59=E(_58),_5a=_59[1],_5b=E(_59[2]),_5c=_4N(_55,_56,_5a,_5b[1],_5b[2],_5b[3],_5b[4],_5b[5],_5b[6],_),_5d=A(_55,[new T(function(){return E(E(_5c)[2]);}),_]),_5e=E(_5d),_5f=_5e[2],_5g=E(_5e[1]),_5h=_5g[1],_5i=new T(function(){return _1p(_1C,[1,[0,_1h,_5a],_a]);}),_5j=E(_5g[2]);if(!_5j[0]){return [0,[0,function(_5k,_){var _5l=A(_5h,[_5k,_]),_5m=A(_5i,[_5k,_]);return _5k;},_b],new T(function(){var _5n=E(_5f);return [0,_5n[1],_5n[2],_5n[3],new T(function(){return E(E(_5c)[1]);}),_5n[5],_5n[6]];})];}else{var _5o=A(_56,[_5j[1],new T(function(){var _5p=E(_5f);return [0,_5p[1],_5p[2],_5p[3],new T(function(){return E(E(_5c)[1]);}),_5p[5],_5p[6]];}),_]),_5q=E(_5o),_5r=E(_5q[1]);return [0,[0,function(_5s,_){var _5t=A(_5h,[_5s,_]),_5u=A(_5i,[_5s,_]),_5v=A(_5r[1],[_5u,_]);return _5s;},_5r[2]],_5q[2]];}},_5w=function(_5x){var _5y=E(_5x);switch(_5y[0]){case 0:return _5w(_5y[3])+_5w(_5y[4])|0;case 1:return 1;default:return 0;}},_5z=true,_5A=function(_5B,_5C){while(1){var _5D=E(_5C);switch(_5D[0]){case 0:var _5E=_5D[3],_5F=_5A(_5B,_5D[4]);if(_5F[0]==2){_5C=_5E;continue;}else{var _5G=_5A(_5B,_5E);return _5G[0]==2?E(_5F):[0,_5D[1],_5D[2],E(_5G),E(_5F)];}break;case 1:return !A(_5B,[[0,_5D[1]],_5D[2]])?[2]:E(_5D);default:return [2];}}},_5H=function(_5I){return _5I>0;},_5J=new T(function(){return [0,"(function(x) {return x === null;})"];}),_5K=new T(function(){return _5(_5J);}),_5L=unCStr("No such value"),_5M=[0,_5L],_5N=unCStr("Invalid JSON!"),_5O=[0,_5N],_5P=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_5Q=function(_5R){return E(E(_5R)[3]);},_5S=function(_5T,_5U,_){var _5V=A(_5,[_5P,E(toJSStr(E(_5U))),_]);return new T(function(){if(!_2(function(_){var _=0,_5W=A(_5K,[E(_5V),_]);return new T(function(){return _5H(_5W);});})){var _5X=String(_5V),_5Y=jsParseJSON(_5X),_5Z=E(_5Y);return _5Z[0]==0?E(_5O):A(_5Q,[_5T,_5Z[1]]);}else{return E(_5M);}});},_60=[8,coercionToken],_61=unCStr("clear-completed"),_62=function(_63,_64,_){var _65=jsCreateTextNode(toJSStr(E(_63))),_66=jsAppendChild(_65,E(_64)[1]);return [0,_65];},_67=unCStr("button"),_68=function(_69,_6a){var _6b=new T(function(){return A(_69,[_6a]);});return function(_6c,_){var _6d=jsCreateElem(toJSStr(E(_67))),_6e=jsAppendChild(_6d,E(_6c)[1]),_6f=[0,_6d],_6g=A(_6b,[_6f,_]);return _6f;};},_6h=unCStr("Clear completed"),_6i=new T(function(){return _68(_62,_6h);}),_6j=function(_6k,_){var _6l=A(_6i,[_6k,_]),_6m=A(_B,[_19,_6l,_1b,_61,_]);return _6l;},_6n=unCStr("keydown"),_6o=unCStr("mousemove"),_6p=unCStr("blur"),_6q=unCStr("focus"),_6r=unCStr("change"),_6s=unCStr("unload"),_6t=unCStr("load"),_6u=unCStr("keyup"),_6v=unCStr("keypress"),_6w=unCStr("mouseup"),_6x=unCStr("mousedown"),_6y=unCStr("dblclick"),_6z=unCStr("click"),_6A=unCStr("mouseout"),_6B=unCStr("mouseover"),_6C=function(_6D){switch(E(_6D)[0]){case 0:return E(_6t);case 1:return E(_6s);case 2:return E(_6r);case 3:return E(_6q);case 4:return E(_6p);case 5:return E(_6o);case 6:return E(_6B);case 7:return E(_6A);case 8:return E(_6z);case 9:return E(_6y);case 10:return E(_6x);case 11:return E(_6w);case 12:return E(_6v);case 13:return E(_6u);default:return E(_6n);}},_6E=function(_6F,_6G){while(1){var _6H=E(_6F);if(!_6H[0]){return E(_6G)[0]==0?true:false;}else{var _6I=E(_6G);if(!_6I[0]){return false;}else{if(E(_6H[1])[1]!=E(_6I[1])[1]){return false;}else{_6F=_6H[2];_6G=_6I[2];continue;}}}}},_6J=[0],_6K=unCStr("Onload"),_6L=[0,_6K,_6J],_6M=unCStr("OnLoad"),_6N=[0,_6M,_6J],_6O=function(_){var _=0,_6P=newMVar(),_=putMVar(_6P,_6N);return [0,_6P];},_6Q=new T(function(){return _2(_6O);}),_6R=function(_6S,_6T,_){var _6U=A(_6S,[_]);return die(_6T);},_6V=function(_6W,_6X,_6Y,_){return _6R(function(_){var _=putMVar(_6W,_6X);return _A;},_6Y,_);},_6Z=function(_70,_){var _71=0;if(!E(_71)){return (function(_){var _72=E(_6Q)[1],_73=takeMVar(_72),_74=jsCatch(function(_){return (function(_){return _70;})();},function(_1w,_){return _6V(_72,_73,_1w,_);}),_=putMVar(_72,_74);return _A;})();}else{var _75=E(_6Q)[1],_76=takeMVar(_75),_77=jsCatch(function(_){return _70;},function(_1w,_){return _6V(_75,_76,_1w,_);}),_=putMVar(_75,_77);return _A;}},_78=function(_79,_){var _7a=_6Z(_6L,_);return [0,[0,_3C,[1,_7a]],new T(function(){var _7b=E(_79);return [0,_7b[1],_7b[2],_7b[3],_7b[4],_5z,_7b[6]];})];},_7c=function(_){var _7d=E(_6Q)[1],_7e=takeMVar(_7d),_=putMVar(_7d,_7e);return _7e;},_7f=function(_7g,_){var _7h=0;if(!E(_7h)){var _7i=_7c();return [0,[0,_3C,[1,_7i]],new T(function(){var _7j=E(_7g);return [0,_7j[1],_7j[2],_7j[3],_7j[4],_5z,_7j[6]];})];}else{var _7k=E(_6Q)[1],_7l=takeMVar(_7k),_=putMVar(_7k,_7l);return [0,[0,_3C,[1,_7l]],new T(function(){var _7m=E(_7g);return [0,_7m[1],_7m[2],_7m[3],_7m[4],_5z,_7m[6]];})];}},_7n=[0,_3C,_b],_7o=function(_7p,_){var _7q=0;if(!E(_7q)){return (function(_){var _7r=E(_6Q)[1],_7s=takeMVar(_7r),_7t=jsCatch(function(_){return (function(_){return _7p;})();},function(_1w,_){return _6V(_7r,_7s,_1w,_);}),_=putMVar(_7r,_7t);return _A;})();}else{var _7u=E(_6Q)[1],_7v=takeMVar(_7u),_7w=jsCatch(function(_){return _7p;},function(_1w,_){return _6V(_7u,_7v,_1w,_);}),_=putMVar(_7u,_7w);return _A;}},_7x=unCStr("true"),_7y=new T(function(){return [0,"keydown"];}),_7z=new T(function(){return [0,"mousemove"];}),_7A=new T(function(){return [0,"blur"];}),_7B=new T(function(){return [0,"focus"];}),_7C=new T(function(){return [0,"change"];}),_7D=new T(function(){return [0,"unload"];}),_7E=new T(function(){return [0,"load"];}),_7F=new T(function(){return [0,"keyup"];}),_7G=new T(function(){return [0,"keypress"];}),_7H=new T(function(){return [0,"mouseup"];}),_7I=new T(function(){return [0,"mousedown"];}),_7J=new T(function(){return [0,"dblclick"];}),_7K=new T(function(){return [0,"click"];}),_7L=new T(function(){return [0,"mouseout"];}),_7M=new T(function(){return [0,"mouseover"];}),_7N=function(_7O){switch(E(_7O)[0]){case 0:return E(_7E);case 1:return E(_7D);case 2:return E(_7C);case 3:return E(_7B);case 4:return E(_7A);case 5:return E(_7z);case 6:return E(_7M);case 7:return E(_7L);case 8:return E(_7K);case 9:return E(_7J);case 10:return E(_7I);case 11:return E(_7H);case 12:return E(_7G);case 13:return E(_7F);default:return E(_7y);}},_7P=function(_7Q,_7R,_7S){var _7T=new T(function(){return _6C(_7R);}),_7U=new T(function(){return _7N(_7R);});return function(_7V,_){var _7W=A(_7Q,[_7V,_]),_7X=E(_7W),_7Y=_7X[1],_7Z=E(_7T),_80=jsGetAttr(_7Y,toJSStr(_7Z));if(!_6E(fromJSStr(_80),_7x)){var _81=E(_7S),_82=jsSetCB(_7Y,E(_7U)[1],E([0,_7S])[1]),_83=A(_B,[_19,_7X,_7Z,_7x,_]);return _7X;}else{return _7X;}};},_84=function(_85,_86){var _87=new T(function(){return _6C(_86);}),_88=[0,_87,_6J];return function(_89,_){var _8a=E(_89),_8b=E(_8a[4]),_8c=_8b[1],_8d=_8b[2],_8e=A(_85,[_8a,_]),_8f=E(_8e),_8g=E(_8f[1]),_8h=_8g[1];return [0,[0,new T(function(){var _8i=E(_86);switch(_8i[0]){case 0:return _7P(_8h,_8i,function(_){var _8j=_7o(_88,_),_8k=A(_8c,[_]),_8l=E(_8k);if(!_8l[0]){return _A;}else{var _8m=A(_8d,[_8l[1],_]);return _A;}});case 1:return _7P(_8h,_8i,function(_){var _8n=_7o(_88,_),_8o=A(_8c,[_]),_8p=E(_8o);if(!_8p[0]){return _A;}else{var _8q=A(_8d,[_8p[1],_]);return _A;}});case 2:return _7P(_8h,_8i,function(_){var _8r=_7o(_88,_),_8s=A(_8c,[_]),_8t=E(_8s);if(!_8t[0]){return _A;}else{var _8u=A(_8d,[_8t[1],_]);return _A;}});case 3:return _7P(_8h,_8i,function(_){var _8v=_7o(_88,_),_8w=A(_8c,[_]),_8x=E(_8w);if(!_8x[0]){return _A;}else{var _8y=A(_8d,[_8x[1],_]);return _A;}});case 4:return _7P(_8h,_8i,function(_){var _8z=_7o(_88,_),_8A=A(_8c,[_]),_8B=E(_8A);if(!_8B[0]){return _A;}else{var _8C=A(_8d,[_8B[1],_]);return _A;}});case 5:return _7P(_8h,_8i,function(_8D,_){var _8E=_7o([0,_87,[2,E(_8D)]],_),_8F=A(_8c,[_]),_8G=E(_8F);if(!_8G[0]){return _A;}else{var _8H=A(_8d,[_8G[1],_]);return _A;}});case 6:return _7P(_8h,_8i,function(_8I,_){var _8J=_7o([0,_87,[2,E(_8I)]],_),_8K=A(_8c,[_]),_8L=E(_8K);if(!_8L[0]){return _A;}else{var _8M=A(_8d,[_8L[1],_]);return _A;}});case 7:return _7P(_8h,_8i,function(_){var _8N=A(_8c,[_]),_8O=E(_8N);if(!_8O[0]){return _A;}else{var _8P=A(_8d,[_8O[1],_]);return _A;}});case 8:return _7P(_8h,_8i,function(_8Q,_8R,_){var _8S=_7o([0,_87,[1,_8Q,E(_8R)]],_),_8T=A(_8c,[_]),_8U=E(_8T);if(!_8U[0]){return _A;}else{var _8V=A(_8d,[_8U[1],_]);return _A;}});case 9:return _7P(_8h,_8i,function(_8W,_8X,_){var _8Y=_7o([0,_87,[1,_8W,E(_8X)]],_),_8Z=A(_8c,[_]),_90=E(_8Z);if(!_90[0]){return _A;}else{var _91=A(_8d,[_90[1],_]);return _A;}});case 10:return _7P(_8h,_8i,function(_92,_93,_){var _94=_7o([0,_87,[1,_92,E(_93)]],_),_95=A(_8c,[_]),_96=E(_95);if(!_96[0]){return _A;}else{var _97=A(_8d,[_96[1],_]);return _A;}});case 11:return _7P(_8h,_8i,function(_98,_99,_){var _9a=_7o([0,_87,[1,_98,E(_99)]],_),_9b=A(_8c,[_]),_9c=E(_9b);if(!_9c[0]){return _A;}else{var _9d=A(_8d,[_9c[1],_]);return _A;}});case 12:return _7P(_8h,_8i,function(_9e,_){var _9f=_7o([0,_87,[3,_9e]],_),_9g=A(_8c,[_]),_9h=E(_9g);if(!_9h[0]){return _A;}else{var _9i=A(_8d,[_9h[1],_]);return _A;}});case 13:return _7P(_8h,_8i,function(_9j,_){var _9k=_7o([0,_87,[3,_9j]],_),_9l=A(_8c,[_]),_9m=E(_9l);if(!_9m[0]){return _A;}else{var _9n=A(_8d,[_9m[1],_]);return _A;}});default:return _7P(_8h,_8i,function(_9o,_){var _9p=_7o([0,_87,[3,_9o]],_),_9q=A(_8c,[_]),_9r=E(_9q);if(!_9r[0]){return _A;}else{var _9s=A(_8d,[_9r[1],_]);return _A;}});}}),_8g[2]],_8f[2]];};},_9t=[1,_A],_9u=function(_9v,_9w){var _9x=new T(function(){return _6C(_9w);}),_9y=new T(function(){return _84(function(_9z,_){return [0,[0,_9v,_9t],_9z];},_9w);});return function(_9A,_9B){return _54(_78,function(_9C,_1w,_){return (function(_1w,_){return _54(_9y,function(_9D){return function(_1w,_){return _54(_7f,function(_9E){var _9F=E(_9E);return (function(_9G,_9H){var _9I=new T(function(){return _6E(_9x,_9G);});return function(_9J,_){return !E(_9I)?[0,_7n,_9J]:[0,[0,_3C,[1,[0,_9G,_9H]]],_9J];};})(_9F[1],_9F[2]);},_1w,_);};},_1w,_);})(_1w,_);},_9A,_9B);};},_9K=new T(function(){return _9u(_6j,_60);}),_9L=[1,_A],_9M=unCStr("todo-list"),_9N=[0,_3C,_9L],_9O=function(_9P,_){return [0,_9N,_9P];},_9Q=function(_9R,_9S){while(1){var _9T=E(_9S);switch(_9T[0]){case 0:var _9U=_9T[1],_9V=_9T[2],_9W=_9T[3],_9X=_9T[4],_9Y=_9R>>>0,_9Z=_9V>>>0;if(((_9Y&((_9Z-1>>>0^4.294967295e9)>>>0^_9Z)>>>0)>>>0&4.294967295e9)==_9U){if((_9Y&_9Z)>>>0!=0){var _a0=_9Q(_9R,_9X);if(_a0[0]==2){return E(_9W);}else{var _a1=E(_9W);return _a1[0]==2?E(_a0):[0,_9U,_9V,E(_a1),E(_a0)];}}else{var _a2=E(_9X);if(_a2[0]==2){_9S=_9W;continue;}else{var _a3=_9Q(_9R,_9W);return _a3[0]==2?E(_a2):[0,_9U,_9V,E(_a3),E(_a2)];}}}else{return E(_9T);}break;case 1:return _9R!=_9T[1]?E(_9T):[2];default:return [2];}}},_a4=function(_a5,_a6){return (function(_a7){while(1){var _a8=E(_a7);switch(_a8[0]){case 0:var _a9=_a5>>>0,_aa=_a8[2]>>>0;if(((_a9&((_aa-1>>>0^4.294967295e9)>>>0^_aa)>>>0)>>>0&4.294967295e9)==_a8[1]){if((_a9&_aa)>>>0!=0){_a7=_a8[4];continue;}else{_a7=_a8[3];continue;}}else{return [0];}break;case 1:return _a5!=_a8[1]?[0]:[1,_a8[2]];default:return [0];}}})(_a6);},_ab=1,_ac=0,_ad=function(_ae,_af,_ag){var _ah=E(_ag);switch(_ah[0]){case 0:var _ai=_ah[1],_aj=_ah[2],_ak=_ah[3],_al=_ah[4],_am=_ae>>>0,_an=_aj>>>0;if(((_am&((_an-1>>>0^4.294967295e9)>>>0^_an)>>>0)>>>0&4.294967295e9)==_ai){return (_am&_an)>>>0!=0?[0,_ai,_aj,E(_ak),E(_ad(_ae,_af,_al))]:[0,_ai,_aj,E(_ad(_ae,_af,_ak)),E(_al)];}else{var _ao=(_am^_ai>>>0)>>>0,_ap=(_ao|_ao>>>1)>>>0,_aq=(_ap|_ap>>>2)>>>0,_ar=(_aq|_aq>>>4)>>>0,_as=(_ar|_ar>>>8)>>>0,_at=(_as|_as>>>16)>>>0,_au=(_at^_at>>>1)>>>0&4.294967295e9,_av=_au>>>0,_aw=(_am&((_av-1>>>0^4.294967295e9)>>>0^_av)>>>0)>>>0&4.294967295e9;return (_am&_av)>>>0!=0?[0,_aw,_au,E(_ah),E([1,_ae,_af])]:[0,_aw,_au,E([1,_ae,_af]),E(_ah)];}break;case 1:var _ax=_ah[1];if(_ae!=_ax){var _ay=_ae>>>0,_az=(_ay^_ax>>>0)>>>0,_aA=(_az|_az>>>1)>>>0,_aB=(_aA|_aA>>>2)>>>0,_aC=(_aB|_aB>>>4)>>>0,_aD=(_aC|_aC>>>8)>>>0,_aE=(_aD|_aD>>>16)>>>0,_aF=(_aE^_aE>>>1)>>>0&4.294967295e9,_aG=_aF>>>0,_aH=(_ay&((_aG-1>>>0^4.294967295e9)>>>0^_aG)>>>0)>>>0&4.294967295e9;return (_ay&_aG)>>>0!=0?[0,_aH,_aF,E(_ah),E([1,_ae,_af])]:[0,_aH,_aF,E([1,_ae,_af]),E(_ah)];}else{return [1,_ae,_af];}break;default:return [1,_ae,_af];}},_aI=function(_aJ){return E(E(_aJ)[1]);},_aK=function(_aL,_aM,_aN){var _aO=E(_aN);return [3,[1,new T(function(){return A(_aI,[_aL,_aO[1]]);}),[1,new T(function(){return A(_aI,[_aM,_aO[2]]);}),_a]]];},_aP=function(_aQ,_aR){var _aS=E(_aR);return _aS[0]==0?[0]:[1,new T(function(){return A(_aQ,[_aS[1]]);}),new T(function(){return _aP(_aQ,_aS[2]);})];},_aT=function(_aU,_aV,_aW){return [3,new T(function(){return _aP(function(_aX){return _aK(_aU,_aV,_aX);},_aW);})];},_aY=unCStr("Tried to deserialize a non-array into a pair!"),_aZ=[0,_aY],_b0=function(_b1,_b2,_b3){var _b4=E(_b3);if(_b4[0]==3){var _b5=E(_b4[1]);if(!_b5[0]){return E(_aZ);}else{var _b6=E(_b5[2]);if(!_b6[0]){return E(_aZ);}else{if(!E(_b6[2])[0]){var _b7=A(_5Q,[_b1,_b5[1]]);if(!_b7[0]){return [0,_b7[1]];}else{var _b8=A(_5Q,[_b2,_b6[1]]);return _b8[0]==0?[0,_b8[1]]:[1,[0,_b7[1],_b8[1]]];}}else{return E(_aZ);}}}}else{return E(_aZ);}},_b9=[1,_a],_ba=unCStr("Tried to deserialie a non-array to a list!"),_bb=[0,_ba],_bc=function(_bd,_be,_bf){var _bg=E(_bf);if(_bg[0]==3){var _bh=function(_bi){var _bj=E(_bi);if(!_bj[0]){return E(_b9);}else{var _bk=_b0(_bd,_be,_bj[1]);if(!_bk[0]){return [0,_bk[1]];}else{var _bl=_bh(_bj[2]);return _bl[0]==0?[0,_bl[1]]:[1,[1,_bk[1],_bl[1]]];}}};return _bh(_bg[1]);}else{return E(_bb);}},_bm=function(_bn,_bo){return [0,function(_aX){return _aK(_bn,_bo,_aX);},function(_aX){return _aT(_bn,_bo,_aX);},function(_aX){return _b0(_bn,_bo,_aX);},function(_aX){return _bc(_bn,_bo,_aX);}];},_bp=unCStr("Completed"),_bq=unCStr("Active"),_br=function(_bs){return [1,new T(function(){return E(_bs)==0?[0,toJSStr(E(_bp))]:[0,toJSStr(E(_bq))];})];},_bt=function(_bu){return [3,new T(function(){return _aP(_br,_bu);})];},_bv=unCStr("Prelude.read: ambiguous parse"),_bw=new T(function(){return err(_bv);}),_bx=unCStr("Prelude.read: no parse"),_by=new T(function(){return err(_bx);}),_bz=function(_bA,_bB){return A(_bB,[_ab]);},_bC=[0,_bq,_bz],_bD=[1,_bC,_a],_bE=function(_bF,_bG){return A(_bG,[_ac]);},_bH=[0,_bp,_bE],_bI=[1,_bH,_bD],_bJ=unCStr("base"),_bK=unCStr("Control.Exception.Base"),_bL=unCStr("PatternMatchFail"),_bM=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_bJ,_bK,_bL],_bN=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_bM,_a],_bO=function(_bP){return E(_bN);},_bQ=function(_bR){var _bS=E(_bR);return _25(_21(_bS[1]),_bO,_bS[2]);},_bT=function(_bU){return E(E(_bU)[1]);},_bV=function(_bW,_bX){return _2l(E(_bW)[1],_bX);},_bY=function(_bZ,_c0){return _3b(_bV,_bZ,_c0);},_c1=function(_c2,_c3,_c4){return _2l(E(_c3)[1],_c4);},_c5=[0,_c1,_bT,_bY],_c6=new T(function(){return [0,_bO,_c5,_c7,_bQ];}),_c7=function(_c8){return [0,_c6,_c8];},_c9=unCStr("Non-exhaustive patterns in"),_ca=function(_cb,_cc){return die(new T(function(){return A(_cc,[_cb]);}));},_cd=function(_ce,_cf){var _cg=E(_cf);if(!_cg[0]){return [0,_a,_a];}else{var _ch=_cg[1];if(!A(_ce,[_ch])){return [0,_a,_cg];}else{var _ci=new T(function(){var _cj=_cd(_ce,_cg[2]);return [0,_cj[1],_cj[2]];});return [0,[1,_ch,new T(function(){return E(E(_ci)[1]);})],new T(function(){return E(E(_ci)[2]);})];}}},_ck=[0,32],_cl=[0,10],_cm=[1,_cl,_a],_cn=function(_co){return E(E(_co)[1])==124?false:true;},_cp=function(_cq,_cr){var _cs=_cd(_cn,unCStr(_cq)),_ct=_cs[1],_cu=function(_cv,_cw){return _2l(_cv,new T(function(){return unAppCStr(": ",new T(function(){return _2l(_cr,new T(function(){return _2l(_cw,_cm);}));}));}));},_cx=E(_cs[2]);return _cx[0]==0?_cu(_ct,_a):E(E(_cx[1])[1])==124?_cu(_ct,[1,_ck,_cx[2]]):_cu(_ct,_a);},_cy=function(_cz){return _ca([0,new T(function(){return _cp(_cz,_c9);})],_c7);},_cA=new T(function(){return _cy("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_cB=function(_cC,_cD){while(1){var _cE=(function(_cF,_cG){var _cH=E(_cF);switch(_cH[0]){case 0:var _cI=E(_cG);if(!_cI[0]){return [0];}else{_cC=A(_cH[1],[_cI[1]]);_cD=_cI[2];return null;}break;case 1:var _cJ=A(_cH[1],[_cG]),_cK=_cG;_cC=_cJ;_cD=_cK;return null;case 2:return [0];case 3:return [1,[0,_cH[1],_cG],new T(function(){return _cB(_cH[2],_cG);})];default:return E(_cH[1]);}})(_cC,_cD);if(_cE!=null){return _cE;}}},_cL=function(_cM,_cN){var _cO=new T(function(){var _cP=E(_cN);if(_cP[0]==3){return [3,_cP[1],new T(function(){return _cL(_cM,_cP[2]);})];}else{var _cQ=E(_cM);if(_cQ[0]==2){return E(_cP);}else{var _cR=E(_cP);if(_cR[0]==2){return E(_cQ);}else{var _cS=new T(function(){var _cT=E(_cR);if(_cT[0]==4){return [1,function(_cU){return [4,new T(function(){return _2l(_cB(_cQ,_cU),_cT[1]);})];}];}else{var _cV=E(_cQ);if(_cV[0]==1){var _cW=_cV[1],_cX=E(_cT);return _cX[0]==0?[1,function(_cY){return _cL(A(_cW,[_cY]),_cX);}]:[1,function(_cZ){return _cL(A(_cW,[_cZ]),new T(function(){return A(_cX[1],[_cZ]);}));}];}else{var _d0=E(_cT);return _d0[0]==0?E(_cA):[1,function(_d1){return _cL(_cV,new T(function(){return A(_d0[1],[_d1]);}));}];}}}),_d2=E(_cQ);switch(_d2[0]){case 1:var _d3=E(_cR);return _d3[0]==4?[1,function(_d4){return [4,new T(function(){return _2l(_cB(A(_d2[1],[_d4]),_d4),_d3[1]);})];}]:E(_cS);case 4:var _d5=_d2[1],_d6=E(_cR);switch(_d6[0]){case 0:return [1,function(_d7){return [4,new T(function(){return _2l(_d5,new T(function(){return _cB(_d6,_d7);}));})];}];case 1:return [1,function(_d8){return [4,new T(function(){return _2l(_d5,new T(function(){return _cB(A(_d6[1],[_d8]),_d8);}));})];}];default:return [4,new T(function(){return _2l(_d5,_d6[1]);})];}break;default:return E(_cS);}}}}}),_d9=E(_cM);switch(_d9[0]){case 0:var _da=E(_cN);return _da[0]==0?[0,function(_db){return _cL(A(_d9[1],[_db]),new T(function(){return A(_da[1],[_db]);}));}]:E(_cO);case 3:return [3,_d9[1],new T(function(){return _cL(_d9[2],_cN);})];default:return E(_cO);}},_dc=function(_dd,_de){return E(_dd)[1]!=E(_de)[1];},_df=function(_dg,_dh){return E(_dg)[1]==E(_dh)[1];},_di=[0,_df,_dc],_dj=function(_dk){return E(E(_dk)[1]);},_dl=function(_dm,_dn,_do){while(1){var _dp=E(_dn);if(!_dp[0]){return E(_do)[0]==0?true:false;}else{var _dq=E(_do);if(!_dq[0]){return false;}else{if(!A(_dj,[_dm,_dp[1],_dq[1]])){return false;}else{_dn=_dp[2];_do=_dq[2];continue;}}}}},_dr=function(_ds,_dt,_du){return !_dl(_ds,_dt,_du)?true:false;},_dv=function(_dw){return [0,function(_dx,_dy){return _dl(_dw,_dx,_dy);},function(_dx,_dy){return _dr(_dw,_dx,_dy);}];},_dz=new T(function(){return _dv(_di);}),_dA=function(_dB,_dC){var _dD=E(_dB);switch(_dD[0]){case 0:return [0,function(_dE){return _dA(A(_dD[1],[_dE]),_dC);}];case 1:return [1,function(_dF){return _dA(A(_dD[1],[_dF]),_dC);}];case 2:return [2];case 3:return _cL(A(_dC,[_dD[1]]),new T(function(){return _dA(_dD[2],_dC);}));default:var _dG=function(_dH){var _dI=E(_dH);if(!_dI[0]){return [0];}else{var _dJ=E(_dI[1]);return _2l(_cB(A(_dC,[_dJ[1]]),_dJ[2]),new T(function(){return _dG(_dI[2]);}));}},_dK=_dG(_dD[1]);return _dK[0]==0?[2]:[4,_dK];}},_dL=[2],_dM=function(_dN){return [3,_dN,_dL];},_dO=function(_dP,_dQ){var _dR=E(_dP);if(!_dR){return A(_dQ,[_A]);}else{var _dS=new T(function(){return _dO(_dR-1|0,_dQ);});return [0,function(_dT){return E(_dS);}];}},_dU=function(_dV,_dW,_dX){var _dY=new T(function(){return A(_dV,[_dM]);});return [1,function(_dZ){return A(function(_e0,_e1,_e2){while(1){var _e3=(function(_e4,_e5,_e6){var _e7=E(_e4);switch(_e7[0]){case 0:var _e8=E(_e5);if(!_e8[0]){return E(_dW);}else{_e0=A(_e7[1],[_e8[1]]);_e1=_e8[2];var _e9=_e6+1|0;_e2=_e9;return null;}break;case 1:var _ea=A(_e7[1],[_e5]),_eb=_e5,_e9=_e6;_e0=_ea;_e1=_eb;_e2=_e9;return null;case 2:return E(_dW);case 3:return function(_ec){var _ed=new T(function(){return _dA(_e7,_ec);});return _dO(_e6,function(_ee){return E(_ed);});};default:return function(_9A){return _dA(_e7,_9A);};}})(_e0,_e1,_e2);if(_e3!=null){return _e3;}}},[_dY,_dZ,0,_dX]);}];},_ef=[6],_eg=unCStr("valDig: Bad base"),_eh=new T(function(){return err(_eg);}),_ei=function(_ej,_ek){var _el=function(_em,_en){var _eo=E(_em);if(!_eo[0]){var _ep=new T(function(){return A(_en,[_a]);});return function(_eq){return A(_eq,[_ep]);};}else{var _er=E(_eo[1])[1],_es=function(_et){var _eu=new T(function(){return _el(_eo[2],function(_ev){return A(_en,[[1,_et,_ev]]);});});return function(_ew){var _ex=new T(function(){return A(_eu,[_ew]);});return [0,function(_ey){return E(_ex);}];};};switch(E(E(_ej)[1])){case 8:if(48>_er){var _ez=new T(function(){return A(_en,[_a]);});return function(_eA){return A(_eA,[_ez]);};}else{if(_er>55){var _eB=new T(function(){return A(_en,[_a]);});return function(_eC){return A(_eC,[_eB]);};}else{return _es([0,_er-48|0]);}}break;case 10:if(48>_er){var _eD=new T(function(){return A(_en,[_a]);});return function(_eE){return A(_eE,[_eD]);};}else{if(_er>57){var _eF=new T(function(){return A(_en,[_a]);});return function(_eG){return A(_eG,[_eF]);};}else{return _es([0,_er-48|0]);}}break;case 16:var _eH=new T(function(){return 97>_er?65>_er?[0]:_er>70?[0]:[1,[0,(_er-65|0)+10|0]]:_er>102?65>_er?[0]:_er>70?[0]:[1,[0,(_er-65|0)+10|0]]:[1,[0,(_er-97|0)+10|0]];});if(48>_er){var _eI=E(_eH);if(!_eI[0]){var _eJ=new T(function(){return A(_en,[_a]);});return function(_eK){return A(_eK,[_eJ]);};}else{return _es(_eI[1]);}}else{if(_er>57){var _eL=E(_eH);if(!_eL[0]){var _eM=new T(function(){return A(_en,[_a]);});return function(_eN){return A(_eN,[_eM]);};}else{return _es(_eL[1]);}}else{return _es([0,_er-48|0]);}}break;default:return E(_eh);}}};return [1,function(_eO){return A(_el,[_eO,_19,function(_eP){var _eQ=E(_eP);return _eQ[0]==0?[2]:A(_ek,[_eQ]);}]);}];},_eR=[0,10],_eS=[0,1],_eT=[0,2147483647],_eU=function(_eV,_eW){while(1){var _eX=E(_eV);if(!_eX[0]){var _eY=_eX[1],_eZ=E(_eW);if(!_eZ[0]){var _f0=_eZ[1],_f1=addC(_eY,_f0);if(!E(_f1[2])){return [0,_f1[1]];}else{_eV=[1,I_fromInt(_eY)];_eW=[1,I_fromInt(_f0)];continue;}}else{_eV=[1,I_fromInt(_eY)];_eW=_eZ;continue;}}else{var _f2=E(_eW);if(!_f2[0]){_eV=_eX;_eW=[1,I_fromInt(_f2[1])];continue;}else{return [1,I_add(_eX[1],_f2[1])];}}}},_f3=new T(function(){return _eU(_eT,_eS);}),_f4=function(_f5){var _f6=E(_f5);if(!_f6[0]){var _f7=E(_f6[1]);return _f7==(-2147483648)?E(_f3):[0, -_f7];}else{return [1,I_negate(_f6[1])];}},_f8=[0,10],_f9=[0,0],_fa=function(_fb,_fc){while(1){var _fd=E(_fb);if(!_fd[0]){var _fe=_fd[1],_ff=E(_fc);if(!_ff[0]){var _fg=_ff[1];if(!(imul(_fe,_fg)|0)){return [0,imul(_fe,_fg)|0];}else{_fb=[1,I_fromInt(_fe)];_fc=[1,I_fromInt(_fg)];continue;}}else{_fb=[1,I_fromInt(_fe)];_fc=_ff;continue;}}else{var _fh=E(_fc);if(!_fh[0]){_fb=_fd;_fc=[1,I_fromInt(_fh[1])];continue;}else{return [1,I_mul(_fd[1],_fh[1])];}}}},_fi=function(_fj,_fk,_fl){while(1){var _fm=E(_fl);if(!_fm[0]){return E(_fk);}else{var _fn=_eU(_fa(_fk,_fj),_fm[1]);_fl=_fm[2];_fk=_fn;continue;}}},_fo=function(_fp){var _fq=new T(function(){return _cL(_cL([0,function(_fr){return E(E(_fr)[1])==45?_ei(_eR,function(_fs){return A(_fp,[[1,new T(function(){return _f4(_fi(_f8,_f9,_fs));})]]);}):[2];}],[0,function(_ft){return E(E(_ft)[1])==43?_ei(_eR,function(_fu){return A(_fp,[[1,new T(function(){return _fi(_f8,_f9,_fu);})]]);}):[2];}]),new T(function(){return _ei(_eR,function(_fv){return A(_fp,[[1,new T(function(){return _fi(_f8,_f9,_fv);})]]);});}));});return _cL([0,function(_fw){return E(E(_fw)[1])==101?E(_fq):[2];}],[0,function(_fx){return E(E(_fx)[1])==69?E(_fq):[2];}]);},_fy=function(_fz){return A(_fz,[_b]);},_fA=function(_fB){return A(_fB,[_b]);},_fC=function(_fD){var _fE=new T(function(){return _ei(_eR,function(_fF){return A(_fD,[[1,_fF]]);});});return [0,function(_fG){return E(E(_fG)[1])==46?E(_fE):[2];}];},_fH=function(_fI){return _ei(_eR,function(_fJ){return _dU(_fC,_fy,function(_fK){return _dU(_fo,_fA,function(_fL){return A(_fI,[[5,[1,_fJ,_fK,_fL]]]);});});});},_fM=function(_fN,_fO,_fP){while(1){var _fQ=E(_fP);if(!_fQ[0]){return false;}else{if(!A(_dj,[_fN,_fO,_fQ[1]])){_fP=_fQ[2];continue;}else{return true;}}}},_fR=unCStr("!@#$%&*+./<=>?\\^|:-~"),_fS=function(_fT){return _fM(_di,_fT,_fR);},_fU=[0,8],_fV=[0,16],_fW=function(_fX){var _fY=new T(function(){return _ei(_fV,function(_fZ){return A(_fX,[[5,[0,_fV,_fZ]]]);});}),_g0=new T(function(){return _ei(_fU,function(_g1){return A(_fX,[[5,[0,_fU,_g1]]]);});}),_g2=new T(function(){return _ei(_fV,function(_g3){return A(_fX,[[5,[0,_fV,_g3]]]);});}),_g4=new T(function(){return _ei(_fU,function(_g5){return A(_fX,[[5,[0,_fU,_g5]]]);});});return [0,function(_g6){return E(E(_g6)[1])==48?E([0,function(_g7){switch(E(E(_g7)[1])){case 79:return E(_g4);case 88:return E(_g2);case 111:return E(_g0);case 120:return E(_fY);default:return [2];}}]):[2];}];},_g8=function(_g9){var _ga=new T(function(){return A(_g9,[_fV]);}),_gb=new T(function(){return A(_g9,[_fU]);}),_gc=new T(function(){return A(_g9,[_fV]);}),_gd=new T(function(){return A(_g9,[_fU]);});return [0,function(_ge){switch(E(E(_ge)[1])){case 79:return E(_gd);case 88:return E(_gc);case 111:return E(_gb);case 120:return E(_ga);default:return [2];}}];},_gf=function(_gg){return A(_gg,[_eR]);},_gh=function(_gi){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _4k(9,_gi,_a);})));},_gj=function(_gk){var _gl=E(_gk);return _gl[0]==0?E(_gl[1]):I_toInt(_gl[1]);},_gm=function(_gn,_go){var _gp=E(_gn);if(!_gp[0]){var _gq=_gp[1],_gr=E(_go);return _gr[0]==0?_gq<=_gr[1]:I_compareInt(_gr[1],_gq)>=0;}else{var _gs=_gp[1],_gt=E(_go);return _gt[0]==0?I_compareInt(_gs,_gt[1])<=0:I_compare(_gs,_gt[1])<=0;}},_gu=function(_gv){return [2];},_gw=function(_gx){var _gy=E(_gx);if(!_gy[0]){return E(_gu);}else{var _gz=_gy[1],_gA=E(_gy[2]);if(!_gA[0]){return E(_gz);}else{var _gB=new T(function(){return _gw(_gA);});return function(_gC){return _cL(A(_gz,[_gC]),new T(function(){return A(_gB,[_gC]);}));};}}},_gD=unCStr("NUL"),_gE=function(_gF){return [2];},_gG=function(_gH){return _gE(_gH);},_gI=function(_gJ,_gK){var _gL=function(_gM,_gN){var _gO=E(_gM);if(!_gO[0]){return function(_gP){return A(_gP,[_gJ]);};}else{var _gQ=E(_gN);if(!_gQ[0]){return E(_gE);}else{if(E(_gO[1])[1]!=E(_gQ[1])[1]){return E(_gG);}else{var _gR=new T(function(){return _gL(_gO[2],_gQ[2]);});return function(_gS){var _gT=new T(function(){return A(_gR,[_gS]);});return [0,function(_gU){return E(_gT);}];};}}}};return [1,function(_gV){return A(_gL,[_gJ,_gV,_gK]);}];},_gW=[0,0],_gX=function(_gY){var _gZ=new T(function(){return A(_gY,[_gW]);});return _gI(_gD,function(_h0){return E(_gZ);});},_h1=unCStr("STX"),_h2=[0,2],_h3=function(_h4){var _h5=new T(function(){return A(_h4,[_h2]);});return _gI(_h1,function(_h6){return E(_h5);});},_h7=unCStr("ETX"),_h8=[0,3],_h9=function(_ha){var _hb=new T(function(){return A(_ha,[_h8]);});return _gI(_h7,function(_hc){return E(_hb);});},_hd=unCStr("EOT"),_he=[0,4],_hf=function(_hg){var _hh=new T(function(){return A(_hg,[_he]);});return _gI(_hd,function(_hi){return E(_hh);});},_hj=unCStr("ENQ"),_hk=[0,5],_hl=function(_hm){var _hn=new T(function(){return A(_hm,[_hk]);});return _gI(_hj,function(_ho){return E(_hn);});},_hp=unCStr("ACK"),_hq=[0,6],_hr=function(_hs){var _ht=new T(function(){return A(_hs,[_hq]);});return _gI(_hp,function(_hu){return E(_ht);});},_hv=unCStr("BEL"),_hw=[0,7],_hx=function(_hy){var _hz=new T(function(){return A(_hy,[_hw]);});return _gI(_hv,function(_hA){return E(_hz);});},_hB=unCStr("BS"),_hC=[0,8],_hD=function(_hE){var _hF=new T(function(){return A(_hE,[_hC]);});return _gI(_hB,function(_hG){return E(_hF);});},_hH=unCStr("HT"),_hI=[0,9],_hJ=function(_hK){var _hL=new T(function(){return A(_hK,[_hI]);});return _gI(_hH,function(_hM){return E(_hL);});},_hN=unCStr("LF"),_hO=[0,10],_hP=function(_hQ){var _hR=new T(function(){return A(_hQ,[_hO]);});return _gI(_hN,function(_hS){return E(_hR);});},_hT=unCStr("VT"),_hU=[0,11],_hV=function(_hW){var _hX=new T(function(){return A(_hW,[_hU]);});return _gI(_hT,function(_hY){return E(_hX);});},_hZ=unCStr("FF"),_i0=[0,12],_i1=function(_i2){var _i3=new T(function(){return A(_i2,[_i0]);});return _gI(_hZ,function(_i4){return E(_i3);});},_i5=unCStr("CR"),_i6=[0,13],_i7=function(_i8){var _i9=new T(function(){return A(_i8,[_i6]);});return _gI(_i5,function(_ia){return E(_i9);});},_ib=unCStr("SI"),_ic=[0,15],_id=function(_ie){var _if=new T(function(){return A(_ie,[_ic]);});return _gI(_ib,function(_ig){return E(_if);});},_ih=unCStr("DLE"),_ii=[0,16],_ij=function(_ik){var _il=new T(function(){return A(_ik,[_ii]);});return _gI(_ih,function(_im){return E(_il);});},_in=unCStr("DC1"),_io=[0,17],_ip=function(_iq){var _ir=new T(function(){return A(_iq,[_io]);});return _gI(_in,function(_is){return E(_ir);});},_it=unCStr("DC2"),_iu=[0,18],_iv=function(_iw){var _ix=new T(function(){return A(_iw,[_iu]);});return _gI(_it,function(_iy){return E(_ix);});},_iz=unCStr("DC3"),_iA=[0,19],_iB=function(_iC){var _iD=new T(function(){return A(_iC,[_iA]);});return _gI(_iz,function(_iE){return E(_iD);});},_iF=unCStr("DC4"),_iG=[0,20],_iH=function(_iI){var _iJ=new T(function(){return A(_iI,[_iG]);});return _gI(_iF,function(_iK){return E(_iJ);});},_iL=unCStr("NAK"),_iM=[0,21],_iN=function(_iO){var _iP=new T(function(){return A(_iO,[_iM]);});return _gI(_iL,function(_iQ){return E(_iP);});},_iR=unCStr("SYN"),_iS=[0,22],_iT=function(_iU){var _iV=new T(function(){return A(_iU,[_iS]);});return _gI(_iR,function(_iW){return E(_iV);});},_iX=unCStr("ETB"),_iY=[0,23],_iZ=function(_j0){var _j1=new T(function(){return A(_j0,[_iY]);});return _gI(_iX,function(_j2){return E(_j1);});},_j3=unCStr("CAN"),_j4=[0,24],_j5=function(_j6){var _j7=new T(function(){return A(_j6,[_j4]);});return _gI(_j3,function(_j8){return E(_j7);});},_j9=unCStr("EM"),_ja=[0,25],_jb=function(_jc){var _jd=new T(function(){return A(_jc,[_ja]);});return _gI(_j9,function(_je){return E(_jd);});},_jf=unCStr("SUB"),_jg=[0,26],_jh=function(_ji){var _jj=new T(function(){return A(_ji,[_jg]);});return _gI(_jf,function(_jk){return E(_jj);});},_jl=unCStr("ESC"),_jm=[0,27],_jn=function(_jo){var _jp=new T(function(){return A(_jo,[_jm]);});return _gI(_jl,function(_jq){return E(_jp);});},_jr=unCStr("FS"),_js=[0,28],_jt=function(_ju){var _jv=new T(function(){return A(_ju,[_js]);});return _gI(_jr,function(_jw){return E(_jv);});},_jx=unCStr("GS"),_jy=[0,29],_jz=function(_jA){var _jB=new T(function(){return A(_jA,[_jy]);});return _gI(_jx,function(_jC){return E(_jB);});},_jD=unCStr("RS"),_jE=[0,30],_jF=function(_jG){var _jH=new T(function(){return A(_jG,[_jE]);});return _gI(_jD,function(_jI){return E(_jH);});},_jJ=unCStr("US"),_jK=[0,31],_jL=function(_jM){var _jN=new T(function(){return A(_jM,[_jK]);});return _gI(_jJ,function(_jO){return E(_jN);});},_jP=unCStr("SP"),_jQ=[0,32],_jR=function(_jS){var _jT=new T(function(){return A(_jS,[_jQ]);});return _gI(_jP,function(_jU){return E(_jT);});},_jV=unCStr("DEL"),_jW=[0,127],_jX=function(_jY){var _jZ=new T(function(){return A(_jY,[_jW]);});return _gI(_jV,function(_k0){return E(_jZ);});},_k1=[1,_jX,_a],_k2=[1,_jR,_k1],_k3=[1,_jL,_k2],_k4=[1,_jF,_k3],_k5=[1,_jz,_k4],_k6=[1,_jt,_k5],_k7=[1,_jn,_k6],_k8=[1,_jh,_k7],_k9=[1,_jb,_k8],_ka=[1,_j5,_k9],_kb=[1,_iZ,_ka],_kc=[1,_iT,_kb],_kd=[1,_iN,_kc],_ke=[1,_iH,_kd],_kf=[1,_iB,_ke],_kg=[1,_iv,_kf],_kh=[1,_ip,_kg],_ki=[1,_ij,_kh],_kj=[1,_id,_ki],_kk=[1,_i7,_kj],_kl=[1,_i1,_kk],_km=[1,_hV,_kl],_kn=[1,_hP,_km],_ko=[1,_hJ,_kn],_kp=[1,_hD,_ko],_kq=[1,_hx,_kp],_kr=[1,_hr,_kq],_ks=[1,_hl,_kr],_kt=[1,_hf,_ks],_ku=[1,_h9,_kt],_kv=[1,_h3,_ku],_kw=[1,_gX,_kv],_kx=unCStr("SOH"),_ky=[0,1],_kz=function(_kA){var _kB=new T(function(){return A(_kA,[_ky]);});return _gI(_kx,function(_kC){return E(_kB);});},_kD=unCStr("SO"),_kE=[0,14],_kF=function(_kG){var _kH=new T(function(){return A(_kG,[_kE]);});return _gI(_kD,function(_kI){return E(_kH);});},_kJ=function(_kK){return _dU(_kz,_kF,_kK);},_kL=[1,_kJ,_kw],_kM=new T(function(){return _gw(_kL);}),_kN=[0,1114111],_kO=[0,34],_kP=[0,_kO,_5z],_kQ=[0,39],_kR=[0,_kQ,_5z],_kS=[0,92],_kT=[0,_kS,_5z],_kU=[0,_hw,_5z],_kV=[0,_hC,_5z],_kW=[0,_i0,_5z],_kX=[0,_hO,_5z],_kY=[0,_i6,_5z],_kZ=[0,_hI,_5z],_l0=[0,_hU,_5z],_l1=[0,_gW,_5z],_l2=[0,_ky,_5z],_l3=[0,_h2,_5z],_l4=[0,_h8,_5z],_l5=[0,_he,_5z],_l6=[0,_hk,_5z],_l7=[0,_hq,_5z],_l8=[0,_hw,_5z],_l9=[0,_hC,_5z],_la=[0,_hI,_5z],_lb=[0,_hO,_5z],_lc=[0,_hU,_5z],_ld=[0,_i0,_5z],_le=[0,_i6,_5z],_lf=[0,_kE,_5z],_lg=[0,_ic,_5z],_lh=[0,_ii,_5z],_li=[0,_io,_5z],_lj=[0,_iu,_5z],_lk=[0,_iA,_5z],_ll=[0,_iG,_5z],_lm=[0,_iM,_5z],_ln=[0,_iS,_5z],_lo=[0,_iY,_5z],_lp=[0,_j4,_5z],_lq=[0,_ja,_5z],_lr=[0,_jg,_5z],_ls=[0,_jm,_5z],_lt=[0,_js,_5z],_lu=[0,_jy,_5z],_lv=[0,_jE,_5z],_lw=[0,_jK,_5z],_lx=function(_ly){return [0,_ly];},_lz=function(_lA){var _lB=new T(function(){return A(_lA,[_l0]);}),_lC=new T(function(){return A(_lA,[_kZ]);}),_lD=new T(function(){return A(_lA,[_kY]);}),_lE=new T(function(){return A(_lA,[_kX]);}),_lF=new T(function(){return A(_lA,[_kW]);}),_lG=new T(function(){return A(_lA,[_kV]);}),_lH=new T(function(){return A(_lA,[_kU]);}),_lI=new T(function(){return A(_lA,[_kT]);}),_lJ=new T(function(){return A(_lA,[_kR]);}),_lK=new T(function(){return A(_lA,[_kP]);});return _cL([0,function(_lL){switch(E(E(_lL)[1])){case 34:return E(_lK);case 39:return E(_lJ);case 92:return E(_lI);case 97:return E(_lH);case 98:return E(_lG);case 102:return E(_lF);case 110:return E(_lE);case 114:return E(_lD);case 116:return E(_lC);case 118:return E(_lB);default:return [2];}}],new T(function(){return _cL(_dU(_g8,_gf,function(_lM){var _lN=new T(function(){return _lx(E(_lM)[1]);});return _ei(_lM,function(_lO){var _lP=_fi(_lN,_f9,_lO);return !_gm(_lP,_kN)?[2]:A(_lA,[[0,new T(function(){var _lQ=_gj(_lP);return _lQ>>>0>1114111?_gh(_lQ):[0,_lQ];}),_5z]]);});}),new T(function(){var _lR=new T(function(){return A(_lA,[_lw]);}),_lS=new T(function(){return A(_lA,[_lv]);}),_lT=new T(function(){return A(_lA,[_lu]);}),_lU=new T(function(){return A(_lA,[_lt]);}),_lV=new T(function(){return A(_lA,[_ls]);}),_lW=new T(function(){return A(_lA,[_lr]);}),_lX=new T(function(){return A(_lA,[_lq]);}),_lY=new T(function(){return A(_lA,[_lp]);}),_lZ=new T(function(){return A(_lA,[_lo]);}),_m0=new T(function(){return A(_lA,[_ln]);}),_m1=new T(function(){return A(_lA,[_lm]);}),_m2=new T(function(){return A(_lA,[_ll]);}),_m3=new T(function(){return A(_lA,[_lk]);}),_m4=new T(function(){return A(_lA,[_lj]);}),_m5=new T(function(){return A(_lA,[_li]);}),_m6=new T(function(){return A(_lA,[_lh]);}),_m7=new T(function(){return A(_lA,[_lg]);}),_m8=new T(function(){return A(_lA,[_lf]);}),_m9=new T(function(){return A(_lA,[_le]);}),_ma=new T(function(){return A(_lA,[_ld]);}),_mb=new T(function(){return A(_lA,[_lc]);}),_mc=new T(function(){return A(_lA,[_lb]);}),_md=new T(function(){return A(_lA,[_la]);}),_me=new T(function(){return A(_lA,[_l9]);}),_mf=new T(function(){return A(_lA,[_l8]);}),_mg=new T(function(){return A(_lA,[_l7]);}),_mh=new T(function(){return A(_lA,[_l6]);}),_mi=new T(function(){return A(_lA,[_l5]);}),_mj=new T(function(){return A(_lA,[_l4]);}),_mk=new T(function(){return A(_lA,[_l3]);}),_ml=new T(function(){return A(_lA,[_l2]);}),_mm=new T(function(){return A(_lA,[_l1]);});return _cL([0,function(_mn){return E(E(_mn)[1])==94?E([0,function(_mo){switch(E(E(_mo)[1])){case 64:return E(_mm);case 65:return E(_ml);case 66:return E(_mk);case 67:return E(_mj);case 68:return E(_mi);case 69:return E(_mh);case 70:return E(_mg);case 71:return E(_mf);case 72:return E(_me);case 73:return E(_md);case 74:return E(_mc);case 75:return E(_mb);case 76:return E(_ma);case 77:return E(_m9);case 78:return E(_m8);case 79:return E(_m7);case 80:return E(_m6);case 81:return E(_m5);case 82:return E(_m4);case 83:return E(_m3);case 84:return E(_m2);case 85:return E(_m1);case 86:return E(_m0);case 87:return E(_lZ);case 88:return E(_lY);case 89:return E(_lX);case 90:return E(_lW);case 91:return E(_lV);case 92:return E(_lU);case 93:return E(_lT);case 94:return E(_lS);case 95:return E(_lR);default:return [2];}}]):[2];}],new T(function(){return A(_kM,[function(_mp){return A(_lA,[[0,_mp,_5z]]);}]);}));}));}));},_mq=function(_mr){return A(_mr,[_A]);},_ms=function(_mt){var _mu=E(_mt);if(!_mu[0]){return E(_mq);}else{var _mv=_mu[2],_mw=E(E(_mu[1])[1]);switch(_mw){case 9:var _mx=new T(function(){return _ms(_mv);});return function(_my){var _mz=new T(function(){return A(_mx,[_my]);});return [0,function(_mA){return E(_mz);}];};case 10:var _mB=new T(function(){return _ms(_mv);});return function(_mC){var _mD=new T(function(){return A(_mB,[_mC]);});return [0,function(_mE){return E(_mD);}];};case 11:var _mF=new T(function(){return _ms(_mv);});return function(_mG){var _mH=new T(function(){return A(_mF,[_mG]);});return [0,function(_mI){return E(_mH);}];};case 12:var _mJ=new T(function(){return _ms(_mv);});return function(_mK){var _mL=new T(function(){return A(_mJ,[_mK]);});return [0,function(_mM){return E(_mL);}];};case 13:var _mN=new T(function(){return _ms(_mv);});return function(_mO){var _mP=new T(function(){return A(_mN,[_mO]);});return [0,function(_mQ){return E(_mP);}];};case 32:var _mR=new T(function(){return _ms(_mv);});return function(_mS){var _mT=new T(function(){return A(_mR,[_mS]);});return [0,function(_mU){return E(_mT);}];};case 160:var _mV=new T(function(){return _ms(_mv);});return function(_mW){var _mX=new T(function(){return A(_mV,[_mW]);});return [0,function(_mY){return E(_mX);}];};default:var _mZ=u_iswspace(_mw);if(!E(_mZ)){return E(_mq);}else{var _n0=new T(function(){return _ms(_mv);});return function(_n1){var _n2=new T(function(){return A(_n0,[_n1]);});return [0,function(_n3){return E(_n2);}];};}}}},_n4=function(_n5){var _n6=new T(function(){return _lz(_n5);}),_n7=new T(function(){return _n4(_n5);}),_n8=[1,function(_n9){return A(_ms,[_n9,function(_na){return E([0,function(_nb){return E(E(_nb)[1])==92?E(_n7):[2];}]);}]);}];return _cL([0,function(_nc){return E(E(_nc)[1])==92?E([0,function(_nd){var _ne=E(E(_nd)[1]);switch(_ne){case 9:return E(_n8);case 10:return E(_n8);case 11:return E(_n8);case 12:return E(_n8);case 13:return E(_n8);case 32:return E(_n8);case 38:return E(_n7);case 160:return E(_n8);default:var _nf=u_iswspace(_ne);return E(_nf)==0?[2]:E(_n8);}}]):[2];}],[0,function(_ng){var _nh=E(_ng);return E(_nh[1])==92?E(_n6):A(_n5,[[0,_nh,_0]]);}]);},_ni=function(_nj,_nk){var _nl=new T(function(){return A(_nk,[[1,new T(function(){return A(_nj,[_a]);})]]);});return _n4(function(_nm){var _nn=E(_nm),_no=E(_nn[1]);return E(_no[1])==34?!E(_nn[2])?E(_nl):_ni(function(_np){return A(_nj,[[1,_no,_np]]);},_nk):_ni(function(_nq){return A(_nj,[[1,_no,_nq]]);},_nk);});},_nr=unCStr("_\'"),_ns=function(_nt){var _nu=u_iswalnum(_nt);return E(_nu)==0?_fM(_di,[0,_nt],_nr):true;},_nv=function(_nw){return _ns(E(_nw)[1]);},_nx=unCStr(",;()[]{}`"),_ny=function(_nz){return A(_nz,[_a]);},_nA=function(_nB,_nC){var _nD=function(_nE){var _nF=E(_nE);if(!_nF[0]){return E(_ny);}else{var _nG=_nF[1];if(!A(_nB,[_nG])){return E(_ny);}else{var _nH=new T(function(){return _nD(_nF[2]);});return function(_nI){var _nJ=new T(function(){return A(_nH,[function(_nK){return A(_nI,[[1,_nG,_nK]]);}]);});return [0,function(_nL){return E(_nJ);}];};}}};return [1,function(_nM){return A(_nD,[_nM,_nC]);}];},_nN=unCStr(".."),_nO=unCStr("::"),_nP=unCStr("->"),_nQ=[0,64],_nR=[1,_nQ,_a],_nS=[0,126],_nT=[1,_nS,_a],_nU=unCStr("=>"),_nV=[1,_nU,_a],_nW=[1,_nT,_nV],_nX=[1,_nR,_nW],_nY=[1,_nP,_nX],_nZ=unCStr("<-"),_o0=[1,_nZ,_nY],_o1=[0,124],_o2=[1,_o1,_a],_o3=[1,_o2,_o0],_o4=[1,_kS,_a],_o5=[1,_o4,_o3],_o6=[0,61],_o7=[1,_o6,_a],_o8=[1,_o7,_o5],_o9=[1,_nO,_o8],_oa=[1,_nN,_o9],_ob=function(_oc){var _od=new T(function(){return A(_oc,[_ef]);});return _cL([1,function(_oe){return E(_oe)[0]==0?E(_od):[2];}],new T(function(){var _of=new T(function(){return _lz(function(_og){var _oh=E(_og);return (function(_oi,_oj){var _ok=new T(function(){return A(_oc,[[0,_oi]]);});return !E(_oj)?E(E(_oi)[1])==39?[2]:[0,function(_ol){return E(E(_ol)[1])==39?E(_ok):[2];}]:[0,function(_om){return E(E(_om)[1])==39?E(_ok):[2];}];})(_oh[1],_oh[2]);});});return _cL([0,function(_on){return E(E(_on)[1])==39?E([0,function(_oo){var _op=E(_oo);switch(E(_op[1])){case 39:return [2];case 92:return E(_of);default:var _oq=new T(function(){return A(_oc,[[0,_op]]);});return [0,function(_or){return E(E(_or)[1])==39?E(_oq):[2];}];}}]):[2];}],new T(function(){var _os=new T(function(){return _ni(_19,_oc);});return _cL([0,function(_ot){return E(E(_ot)[1])==34?E(_os):[2];}],new T(function(){return _cL([0,function(_ou){return !_fM(_di,_ou,_nx)?[2]:A(_oc,[[2,[1,_ou,_a]]]);}],new T(function(){return _cL([0,function(_ov){return !_fM(_di,_ov,_fR)?[2]:_nA(_fS,function(_ow){var _ox=[1,_ov,_ow];return !_fM(_dz,_ox,_oa)?A(_oc,[[4,_ox]]):A(_oc,[[2,_ox]]);});}],new T(function(){return _cL([0,function(_oy){var _oz=E(_oy),_oA=_oz[1],_oB=u_iswalpha(_oA);return E(_oB)==0?E(_oA)==95?_nA(_nv,function(_oC){return A(_oc,[[3,[1,_oz,_oC]]]);}):[2]:_nA(_nv,function(_oD){return A(_oc,[[3,[1,_oz,_oD]]]);});}],new T(function(){return _dU(_fW,_fH,_oc);}));}));}));}));}));}));},_oE=function(_oF){var _oG=new T(function(){return _ob(_oF);});return [1,function(_oH){return A(_ms,[_oH,function(_oI){return E(_oG);}]);}];},_oJ=function(_oK,_oL,_oM){var _oN=E(_oK);if(!_oN[0]){return [2];}else{var _oO=E(_oN[1]),_oP=_oO[1],_oQ=new T(function(){return A(_oO[2],[_oL,_oM]);});return _cL(_oE(function(_oR){var _oS=E(_oR);switch(_oS[0]){case 3:return !_6E(_oP,_oS[1])?[2]:E(_oQ);case 4:return !_6E(_oP,_oS[1])?[2]:E(_oQ);default:return [2];}}),new T(function(){return _oJ(_oN[2],_oL,_oM);}));}},_oT=function(_oU,_oV){return _oJ(_bI,_oU,_oV);},_oW=[0,0],_oX=function(_oY,_oZ){var _p0=new T(function(){return A(_oY,[_oW,function(_p1){var _p2=new T(function(){return A(_oZ,[_p1]);});return _oE(function(_p3){var _p4=E(_p3);if(_p4[0]==2){var _p5=E(_p4[1]);return _p5[0]==0?[2]:E(E(_p5[1])[1])==41?E(_p5[2])[0]==0?E(_p2):[2]:[2];}else{return [2];}});}]);});return _oE(function(_p6){var _p7=E(_p6);if(_p7[0]==2){var _p8=E(_p7[1]);return _p8[0]==0?[2]:E(E(_p8[1])[1])==40?E(_p8[2])[0]==0?E(_p0):[2]:[2];}else{return [2];}});},_p9=function(_pa,_pb){var _pc=function(_pd){var _pe=new T(function(){return A(_pa,[_pd]);});return function(_pf){return _cL(A(_pe,[_pf]),new T(function(){return _oX(_pc,_pf);}));};};return _pc(_pb);},_pg=function(_ph){return [1,function(_pi){return A(_ms,[_pi,function(_pj){return E([3,_ph,_dL]);}]);}];},_pk=new T(function(){return A(_p9,[_oT,_oW,_pg]);}),_pl=new T(function(){return _cy("todo.hs:26:3-54|function parseJSON");}),_pm=function(_pn){return fromJSStr(E(_pn)[1]);},_po=function(_pp){while(1){var _pq=(function(_pr){var _ps=E(_pr);if(!_ps[0]){return [0];}else{var _pt=_ps[2],_pu=E(_ps[1]);if(!E(_pu[2])[0]){return [1,_pu[1],new T(function(){return _po(_pt);})];}else{_pp=_pt;return null;}}})(_pp);if(_pq!=null){return _pq;}}},_pv=function(_pw){var _px=E(_pw);return _px[0]==1?[1,new T(function(){var _py=_po(_cB(_pk,new T(function(){return _pm(_px[1]);})));return _py[0]==0?E(_by):E(_py[2])[0]==0?E(_py[1]):E(_bw);})]:E(_pl);},_pz=unCStr("Tried to deserialie a non-array to a list!"),_pA=[0,_pz],_pB=[1,_a],_pC=function(_pD){var _pE=E(_pD);if(!_pE[0]){return E(_pB);}else{var _pF=_pv(_pE[1]);if(!_pF[0]){return [0,_pF[1]];}else{var _pG=_pC(_pE[2]);return _pG[0]==0?[0,_pG[1]]:[1,[1,_pF[1],_pG[1]]];}}},_pH=function(_pI){var _pJ=E(_pI);return _pJ[0]==3?_pC(_pJ[1]):E(_pA);},_pK=[0,_br,_bt,_pv,_pH],_pL=function(_pM){return [0,toJSStr(E(_pM))];},_pN=function(_pO){return [1,new T(function(){return _pL(_pO);})];},_pP=unCStr("Tried to deserialize a non-string to a Char"),_pQ=[0,_pP],_pR=unCStr("Tried to deserialize long string to a Char"),_pS=[0,_pR],_pT=function(_pU){var _pV=E(_pU);if(_pV[0]==1){var _pW=fromJSStr(E(_pV[1])[1]);return _pW[0]==0?E(_pS):E(_pW[2])[0]==0?[1,_pW[1]]:E(_pS);}else{return E(_pQ);}},_pX=unCStr("Tried to deserialize a non-JSString to a JSString"),_pY=[0,_pX],_pZ=function(_q0){var _q1=E(_q0);return _q1[0]==1?[1,new T(function(){return _pm(_q1[1]);})]:E(_pY);},_q2=function(_q3){return [1,new T(function(){return [0,toJSStr([1,_q3,_a])];})];},_q4=[0,_q2,_pN,_pT,_pZ],_q5=function(_q6){return E(E(_q6)[2]);},_q7=function(_q8,_q9){return [3,new T(function(){return _aP(new T(function(){return _q5(_q8);}),_q9);})];},_qa=[1,_a],_qb=[0,_ba],_qc=function(_qd){return E(E(_qd)[4]);},_qe=function(_qf,_qg){var _qh=E(_qg);if(_qh[0]==3){var _qi=new T(function(){return _qc(_qf);}),_qj=function(_qk){var _ql=E(_qk);if(!_ql[0]){return E(_qa);}else{var _qm=A(_qi,[_ql[1]]);if(!_qm[0]){return [0,_qm[1]];}else{var _qn=_qj(_ql[2]);return _qn[0]==0?[0,_qn[1]]:[1,[1,_qm[1],_qn[1]]];}}};return _qj(_qh[1]);}else{return E(_qb);}},_qo=function(_qp){return [0,new T(function(){return _q5(_qp);}),function(_aX){return _q7(_qp,_aX);},new T(function(){return _qc(_qp);}),function(_aX){return _qe(_qp,_aX);}];},_qq=new T(function(){return _qo(_q4);}),_qr=new T(function(){return _bm(_qq,_pK);}),_qs=function(_qt){return [0,_qt];},_qu=function(_qv){return [3,new T(function(){return _aP(_qs,_qv);})];},_qw=unCStr("The given Number can\'t be represented as an Int"),_qx=[0,_qw],_qy=unCStr("Tried to deserialize a non-Number to an Int"),_qz=[0,_qy],_qA=function(_qB){var _qC=E(_qB);if(!_qC[0]){var _qD=E(_qC[1])[1],_qE=[0,_qD&4.294967295e9];return _qE[1]!=_qD?E(_qx):[1,_qE];}else{return E(_qz);}},_qF=[0,_ba],_qG=[1,_a],_qH=[0,_qw],_qI=[0,_qy],_qJ=function(_qK){var _qL=E(_qK);if(!_qL[0]){return E(_qG);}else{var _qM=E(_qL[1]);if(!_qM[0]){var _qN=E(_qM[1])[1],_qO=[0,_qN&4.294967295e9];if(_qO[1]!=_qN){return E(_qH);}else{var _qP=_qJ(_qL[2]);return _qP[0]==0?[0,_qP[1]]:[1,[1,_qO,_qP[1]]];}}else{return E(_qI);}}},_qQ=function(_qR){var _qS=E(_qR);return _qS[0]==3?_qJ(_qS[1]):E(_qF);},_qT=[0,_qs,_qu,_qA,_qQ],_qU=new T(function(){return _bm(_qT,_qr);}),_qV=new T(function(){return _qo(_qU);}),_qW=new T(function(){return _bm(_qV,_qT);}),_qX=unCStr("tasks"),_qY=new T(function(){return [0,toJSStr(_a)];}),_qZ=[0,93],_r0=[1,_qZ,_a],_r1=new T(function(){return [0,toJSStr(_r0)];}),_r2=[0,125],_r3=[1,_r2,_a],_r4=new T(function(){return [0,toJSStr(_r3)];}),_r5=[0,58],_r6=[1,_r5,_a],_r7=new T(function(){return [0,toJSStr(_r6)];}),_r8=[0,44],_r9=[1,_r8,_a],_ra=new T(function(){return [0,toJSStr(_r9)];}),_rb=new T(function(){return [0,"false"];}),_rc=function(_rd){var _re=jsShow(E(_rd)[1]);return [0,_re];},_rf=function(_rg){var _rh=jsStringify(E(_rg)[1]);return [0,_rh];},_ri=[0,91],_rj=[1,_ri,_a],_rk=new T(function(){return [0,toJSStr(_rj)];}),_rl=[0,123],_rm=[1,_rl,_a],_rn=new T(function(){return [0,toJSStr(_rm)];}),_ro=[0,34],_rp=[1,_ro,_a],_rq=new T(function(){return [0,toJSStr(_rp)];}),_rr=new T(function(){return [0,"true"];}),_rs=function(_rt,_ru){var _rv=E(_ru);switch(_rv[0]){case 0:return [1,new T(function(){return _rc(_rv[1]);}),_rt];case 1:return [1,new T(function(){return _rf(_rv[1]);}),_rt];case 2:return !E(_rv[1])?[1,_rb,_rt]:[1,_rr,_rt];case 3:var _rw=E(_rv[1]);return _rw[0]==0?[1,_rk,[1,_r1,_rt]]:[1,_rk,new T(function(){return _rs(new T(function(){var _rx=function(_ry){var _rz=E(_ry);return _rz[0]==0?E([1,_r1,_rt]):[1,_ra,new T(function(){return _rs(new T(function(){return _rx(_rz[2]);}),_rz[1]);})];};return _rx(_rw[2]);}),_rw[1]);})];default:var _rA=E(_rv[1]);if(!_rA[0]){return [1,_rn,[1,_r4,_rt]];}else{var _rB=E(_rA[1]);return [1,_rn,[1,new T(function(){return _rf(_rB[1]);}),[1,_r7,new T(function(){return _rs(new T(function(){var _rC=function(_rD){var _rE=E(_rD);if(!_rE[0]){return E([1,_r4,_rt]);}else{var _rF=E(_rE[1]);return [1,_ra,[1,_rq,[1,_rF[1],[1,_rq,[1,_r7,new T(function(){return _rs(new T(function(){return _rC(_rE[2]);}),_rF[2]);})]]]]];}};return _rC(_rA[2]);}),_rB[2]);})]]];}}},_rG=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_rH=function(_rI,_rJ){var _rK=new T(function(){return A(_5,[_rG,E(toJSStr(E(_rJ)))]);}),_rL=new T(function(){return _aI(_rI);});return function(_rM,_){var _rN=jsCat(new T(function(){return _rs(_a,A(_rL,[_rM]));}),E(_qY)[1]),_rO=A(_rK,[E(_rN),_]);return _A;};},_rP=new T(function(){return _rH(_qW,_qX);}),_rQ=[2],_rR=function(_rS,_rT){while(1){var _rU=E(_rT);if(!_rU[0]){return E(_rS);}else{var _rV=E(_rU[1]),_rW=_ad(E(_rV[1])[1],_rV[2],_rS);_rT=_rU[2];_rS=_rW;continue;}}},_rX=function(_rY){return _rR(_rQ,_rY);},_rZ=[0,0],_s0=function(_s1,_s2){while(1){var _s3=(function(_s4,_s5){var _s6=E(_s5);switch(_s6[0]){case 0:_s1=new T(function(){return _s0(_s4,_s6[4]);});_s2=_s6[3];return null;case 1:return [1,[0,[0,_s6[1]],_s6[2]],_s4];default:return E(_s4);}})(_s1,_s2);if(_s3!=null){return _s3;}}},_s7=function(_s8){var _s9=E(_s8);if(!_s9[0]){var _sa=_s9[3],_sb=_s9[4];return _s9[2]>=0?_s0(new T(function(){return _s0(_a,_sb);}),_sa):_s0(new T(function(){return _s0(_a,_sa);}),_sb);}else{return _s0(_a,_s9);}},_sc=new T(function(){return _s7(_rQ);}),_sd=[0,_sc,_rZ],_se=new T(function(){return _bm(_qq,_pK);}),_sf=new T(function(){return _bm(_qT,_se);}),_sg=new T(function(){return _qo(_sf);}),_sh=new T(function(){return _bm(_sg,_qT);}),_si=new T(function(){return _rH(_sh,_qX);}),_sj=function(_sk,_){while(1){var _sl=(function(_sm,_){var _sn=E(_sm);if(!_sn[0]){var _so=A(_si,[_sd,_]),_sp=_5S(_sh,_qX,_);_sk=_sp;return null;}else{var _sq=E(_sn[1]);return [0,new T(function(){return _rX(_sq[1]);}),_sq[2]];}})(_sk,_);if(_sl!=null){return _sl;}}},_sr=function(_ss,_st,_su,_sv){return A(_ss,[function(_){var _sw=_5S(_sh,_qX,_),_sx=_sj(_sw,_),_sy=E(_sx),_sz=new T(function(){return _ad(E(_st)[1],[0,_su,_sv],_sy[1]);}),_sA=A(_rP,[[0,new T(function(){return _s7(_sz);}),_sy[2]],_]);return _sz;}]);},_sB=[9,coercionToken],_sC=[13,coercionToken],_sD=[0,_3C,_b],_sE=function(_sF,_){var _sG=0;if(!E(_sG)){var _sH=_7c();return [0,[0,_3C,[1,_sH]],new T(function(){var _sI=E(_sF);return [0,_sI[1],_sI[2],_sI[3],_sI[4],_5z,_sI[6]];})];}else{var _sJ=E(_6Q)[1],_sK=takeMVar(_sJ),_=putMVar(_sJ,_sK);return [0,[0,_3C,[1,_sK]],new T(function(){var _sL=E(_sF);return [0,_sL[1],_sL[2],_sL[3],_sL[4],_5z,_sL[6]];})];}},_sM=function(_sN){return err(_sN);},_sO=unCStr("Pattern match failure in do expression at todo.hs:174:13-31"),_sP=new T(function(){return _sM(_sO);}),_sQ=function(_sR,_sS,_){return _54(_sE,function(_sT){var _sU=E(E(_sT)[2]);return _sU[0]==3?function(_sV,_){return E(E(_sU[1])[1])==13?[0,[0,_3C,[1,_sR]],_sV]:[0,_sD,_sV];}:E(_sP);},_sS,_);},_sW=unCStr("edit"),_sX=unCStr("completed"),_sY=unCStr("text"),_sZ=unCStr("label"),_t0=function(_t1,_t2){var _t3=new T(function(){return A(_t1,[_t2]);});return function(_t4,_){var _t5=jsCreateElem(toJSStr(E(_sZ))),_t6=jsAppendChild(_t5,E(_t4)[1]),_t7=[0,_t5],_t8=A(_t3,[_t7,_]);return _t7;};},_t9=function(_ta,_tb,_tc,_){var _td=_1x(_ta,_tc,_),_te=A(_tb,[_td,_]);return _td;},_tf=unCStr("()"),_tg=unCStr("GHC.Tuple"),_th=unCStr("ghc-prim"),_ti=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_th,_tg,_tf],_tj=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_ti,_a],_tk=function(_tl){return E(_tj);},_tm=unCStr("haste-perch-0.1.0.1"),_tn=unCStr("Haste.Perch"),_to=unCStr("PerchM"),_tp=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_tm,_tn,_to],_tq=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_tp,_a],_tr=function(_ts){return E(_tq);},_tt=function(_tu){var _tv=E(_tu);return _tv[0]==0?[0]:_2l(_tv[1],new T(function(){return _tt(_tv[2]);}));},_tw=function(_tx,_ty){var _tz=E(_tx);if(!_tz){return [0,_a,_ty];}else{var _tA=E(_ty);if(!_tA[0]){return [0,_a,_a];}else{var _tB=new T(function(){var _tC=_tw(_tz-1|0,_tA[2]);return [0,_tC[1],_tC[2]];});return [0,[1,_tA[1],new T(function(){return E(E(_tB)[1]);})],new T(function(){return E(E(_tB)[2]);})];}}},_tD=[0,120],_tE=[0,48],_tF=function(_tG){var _tH=new T(function(){var _tI=_tw(8,new T(function(){var _tJ=md5(toJSStr(E(_tG)));return fromJSStr(_tJ);}));return [0,_tI[1],_tI[2]];}),_tK=parseInt([0,toJSStr([1,_tE,[1,_tD,new T(function(){return E(E(_tH)[1]);})]])]),_tL=new T(function(){var _tM=_tw(8,new T(function(){return E(E(_tH)[2]);}));return [0,_tM[1],_tM[2]];}),_tN=parseInt([0,toJSStr([1,_tE,[1,_tD,new T(function(){return E(E(_tL)[1]);})]])]),_tO=hs_mkWord64(_tK,_tN),_tP=parseInt([0,toJSStr([1,_tE,[1,_tD,new T(function(){return E(_tw(8,new T(function(){return E(E(_tL)[2]);}))[1]);})]])]),_tQ=hs_mkWord64(_tP,_tP);return [0,_tO,_tQ];},_tR=function(_tS,_tT){var _tU=jsShowI(_tS),_tV=md5(_tU);return _2l(fromJSStr(_tV),new T(function(){var _tW=jsShowI(_tT),_tX=md5(_tW);return fromJSStr(_tX);}));},_tY=function(_tZ){var _u0=E(_tZ);return _tR(_u0[1],_u0[2]);},_u1=function(_u2){var _u3=E(_u2);if(!_u3[0]){return [0];}else{var _u4=E(_u3[1]);return [1,[0,_u4[1],_u4[2]],new T(function(){return _u1(_u3[2]);})];}},_u5=unCStr("Prelude.undefined"),_u6=new T(function(){return err(_u5);}),_u7=function(_u8,_u9){return function(_ua){return E(new T(function(){var _ub=A(_u8,[_u6]),_uc=E(_ub[3]),_ud=_uc[1],_ue=_uc[2],_uf=_2l(_ub[4],[1,new T(function(){return A(_u9,[_u6]);}),_a]);if(!_uf[0]){return [0,_ud,_ue,_uc,_a];}else{var _ug=_tF(new T(function(){return _tt(_aP(_tY,[1,[0,_ud,_ue],new T(function(){return _u1(_uf);})]));}));return [0,_ug[1],_ug[2],_uc,_uf];}}));};},_uh=new T(function(){return _u7(_tr,_tk);}),_ui=unCStr("value"),_uj=unCStr("onclick"),_uk=unCStr("checked"),_ul=[0,_uk,_a],_um=[1,_ul,_a],_un=unCStr("type"),_uo=unCStr("input"),_up=function(_uq,_){return _1x(_uo,_uq,_);},_ur=function(_us,_ut,_uu,_uv,_uw){var _ux=new T(function(){var _uy=new T(function(){return _1p(_up,[1,[0,_un,_ut],[1,[0,_1h,_us],[1,[0,_ui,_uu],_a]]]);});return !E(_uv)?E(_uy):_1p(_uy,_um);}),_uz=E(_uw);return _uz[0]==0?E(_ux):_1p(_ux,[1,[0,_uj,_uz[1]],_a]);},_uA=unCStr("href"),_uB=[0,97],_uC=[1,_uB,_a],_uD=function(_uE,_){return _1x(_uC,_uE,_);},_uF=function(_uG,_uH){var _uI=new T(function(){return _1p(_uD,[1,[0,_uA,_uG],_a]);});return function(_uJ,_){var _uK=A(_uI,[_uJ,_]),_uL=A(_uH,[_uK,_]);return _uK;};},_uM=function(_uN){return _uF(_uN,function(_1w,_){return _62(_uN,_1w,_);});},_uO=unCStr("option"),_uP=function(_uQ,_){return _1x(_uO,_uQ,_);},_uR=unCStr("selected"),_uS=[0,_uR,_a],_uT=[1,_uS,_a],_uU=function(_uV,_uW,_uX){var _uY=new T(function(){return _1p(_uP,[1,[0,_ui,_uV],_a]);}),_uZ=function(_v0,_){var _v1=A(_uY,[_v0,_]),_v2=A(_uW,[_v1,_]);return _v1;};return !E(_uX)?E(_uZ):_1p(_uZ,_uT);},_v3=function(_v4,_v5){return _uU(_v4,function(_1w,_){return _62(_v4,_1w,_);},_v5);},_v6=unCStr("method"),_v7=unCStr("action"),_v8=unCStr("UTF-8"),_v9=unCStr("acceptCharset"),_va=[0,_v9,_v8],_vb=unCStr("form"),_vc=function(_vd,_){return _1x(_vb,_vd,_);},_ve=function(_vf,_vg,_vh){var _vi=new T(function(){return _1p(_vc,[1,_va,[1,[0,_v7,_vf],[1,[0,_v6,_vg],_a]]]);});return function(_vj,_){var _vk=A(_vi,[_vj,_]),_vl=A(_vh,[_vk,_]);return _vk;};},_vm=unCStr("select"),_vn=function(_vo,_){return _1x(_vm,_vo,_);},_vp=function(_vq,_vr){var _vs=new T(function(){return _1p(_vn,[1,[0,_1h,_vq],_a]);});return function(_vt,_){var _vu=A(_vs,[_vt,_]),_vv=A(_vr,[_vu,_]);return _vu;};},_vw=unCStr("textarea"),_vx=function(_vy,_){return _1x(_vw,_vy,_);},_vz=function(_vA,_vB){var _vC=new T(function(){return _1p(_vx,[1,[0,_1h,_vA],_a]);});return function(_vD,_){var _vE=A(_vC,[_vD,_]),_vF=_62(_vB,_vE,_);return _vE;};},_vG=unCStr("color:red"),_vH=unCStr("style"),_vI=[0,_vH,_vG],_vJ=[1,_vI,_a],_vK=[0,98],_vL=[1,_vK,_a],_vM=function(_vN){return _1p(function(_vO,_){var _vP=_1x(_vL,_vO,_),_vQ=A(_vN,[_vP,_]);return _vP;},_vJ);},_vR=function(_vS,_vT,_){var _vU=E(_vS);if(!_vU[0]){return _vT;}else{var _vV=A(_vU[1],[_vT,_]),_vW=_vR(_vU[2],_vT,_);return _vT;}},_vX=function(_vY,_vZ,_w0,_){var _w1=A(_vY,[_w0,_]),_w2=A(_vZ,[_w0,_]);return _w0;},_w3=[0,_3C,_vX,_vR],_w4=[0,_w3,_uh,_62,_62,_t9,_vM,_uF,_uM,_ur,_vz,_vp,_uU,_v3,_ve,_1p],_w5=[0,_3E,_19],_w6=function(_w7){return E(E(_w7)[1]);},_w8=function(_w9){return E(E(_w9)[2]);},_wa=function(_wb,_wc){var _wd=new T(function(){return A(_w8,[_wb,_wc]);}),_we=new T(function(){return _w6(_wb);}),_wf=new T(function(){return _45(_we);}),_wg=new T(function(){return _3F(_we);});return function(_wh){return A(_wg,[_wd,function(_wi){return A(_wf,[[0,_wi,_wh]]);}]);};},_wj=function(_wk,_wl){return [0,_wk,function(_wm){return _wa(_wl,_wm);}];},_wn=function(_wo,_wp){return A(_45,[_wo,[0,_wp,_wp]]);},_wq=function(_wr,_ws,_wt){return A(_45,[_wr,[0,_A,_ws]]);},_wu=function(_wv,_ww){return [0,_wv,function(_wx){return _wn(_ww,_wx);},function(_wy,_wz){return _wq(_ww,_wy,_wz);}];},_wA=function(_wB){return E(E(_wB)[1]);},_wC=function(_wD){return E(E(_wD)[1]);},_wE=function(_wF,_wG){return A(_wF,[function(_){return jsFind(toJSStr(E(_wG)));}]);},_wH=function(_wI){return E(E(_wI)[3]);},_wJ=unCStr("GHC.Types"),_wK=unCStr("[]"),_wL=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_th,_wJ,_wK],_wM=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_wL,_a],_wN=function(_wO){return E(_wM);},_wP=unCStr("Char"),_wQ=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_th,_wJ,_wP],_wR=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_wQ,_a],_wS=function(_wT){return E(_wR);},_wU=new T(function(){return _u7(_wN,_wS);}),_wV=new T(function(){return A(_wU,[_u6]);}),_wW=new T(function(){return E(_u6);}),_wX=function(_wY){return E(E(_wY)[6]);},_wZ=function(_x0){return E(E(_x0)[1]);},_x1=[0,0],_x2=[0,32],_x3=[0,10],_x4=function(_x5){var _x6=E(_x5);if(!_x6[0]){return E(_19);}else{var _x7=_x6[1],_x8=E(_x6[2]);if(!_x8[0]){return _x9(_x3,_x7);}else{var _xa=new T(function(){return _x4(_x8);}),_xb=new T(function(){return _x9(_x3,_x7);});return function(_xc){return A(_xb,[[1,_x2,new T(function(){return A(_xa,[_xc]);})]]);};}}},_xd=unCStr("->"),_xe=[1,_xd,_a],_xf=[1,_wJ,_xe],_xg=[1,_th,_xf],_xh=[0,32],_xi=function(_xj){var _xk=E(_xj);if(!_xk[0]){return [0];}else{var _xl=_xk[1],_xm=E(_xk[2]);return _xm[0]==0?E(_xl):_2l(_xl,[1,_xh,new T(function(){return _xi(_xm);})]);}},_xn=new T(function(){return _xi(_xg);}),_xo=new T(function(){var _xp=_tF(_xn);return [0,_xp[1],_xp[2],_th,_wJ,_xd];}),_xq=function(_xr,_xs){var _xt=E(_xr);return _xt[0]==0?E(_xs):A(_xt[1],[new T(function(){return _xq(_xt[2],_xs);})]);},_xu=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_xv=[1,_tj,_a],_xw=function(_xx){var _xy=E(_xx);if(!_xy[0]){return [0];}else{var _xz=E(_xy[1]);return [1,[0,_xz[1],_xz[2]],new T(function(){return _xw(_xy[2]);})];}},_xA=new T(function(){var _xB=_2l(_a,_xv);if(!_xB[0]){return E(_wL);}else{var _xC=_tF(new T(function(){return _tt(_aP(_tY,[1,_xu,new T(function(){return _xw(_xB);})]));}));return E(_wL);}}),_xD=[0,40],_xE=function(_xF){return _x9(_x3,_xF);},_xG=[0,8],_xH=unCStr(" -> "),_xI=[0,9],_xJ=[0,93],_xK=[0,91],_xL=[0,41],_xM=[0,44],_xN=function(_xF){return [1,_xM,_xF];},_xO=function(_xP,_xQ){var _xR=E(_xQ);return _xR[0]==0?[0]:[1,_xP,[1,_xR[1],new T(function(){return _xO(_xP,_xR[2]);})]];},_x9=function(_xS,_xT){var _xU=E(_xT),_xV=_xU[3],_xW=E(_xU[4]);if(!_xW[0]){return function(_xX){return _2l(E(_xV)[5],_xX);};}else{var _xY=_xW[1],_xZ=new T(function(){var _y0=E(_xV)[5],_y1=new T(function(){return _x4(_xW);}),_y2=new T(function(){return E(_xS)[1]<=9?function(_y3){return _2l(_y0,[1,_x2,new T(function(){return A(_y1,[_y3]);})]);}:function(_y4){return [1,_4j,new T(function(){return _2l(_y0,[1,_x2,new T(function(){return A(_y1,[[1,_4i,_y4]]);})]);})];};}),_y5=E(_y0);if(!_y5[0]){return E(_y2);}else{if(E(E(_y5[1])[1])==40){var _y6=E(_y5[2]);return _y6[0]==0?E(_y2):E(E(_y6[1])[1])==44?function(_y7){return [1,_xD,new T(function(){return A(new T(function(){var _y8=_aP(_xE,_xW);if(!_y8[0]){return E(_19);}else{var _y9=new T(function(){return _xO(_xN,_y8[2]);});return function(_9A){return _xq([1,_y8[1],_y9],_9A);};}}),[[1,_xL,_y7]]);})];}:E(_y2);}else{return E(_y2);}}}),_ya=E(_xW[2]);if(!_ya[0]){var _yb=E(_xV),_yc=E(_xA),_yd=hs_eqWord64(_yb[1],_yc[1]);if(!E(_yd)){return E(_xZ);}else{var _ye=hs_eqWord64(_yb[2],_yc[2]);if(!E(_ye)){return E(_xZ);}else{var _yf=new T(function(){return _x9(_x1,_xY);});return function(_yg){return [1,_xK,new T(function(){return A(_yf,[[1,_xJ,_yg]]);})];};}}}else{if(!E(_ya[2])[0]){var _yh=E(_xV),_yi=E(_xo),_yj=hs_eqWord64(_yh[1],_yi[1]);if(!E(_yj)){return E(_xZ);}else{var _yk=hs_eqWord64(_yh[2],_yi[2]);if(!E(_yk)){return E(_xZ);}else{var _yl=new T(function(){return _x9(_xG,_ya[1]);}),_ym=new T(function(){return _x9(_xI,_xY);});return E(_xS)[1]<=8?function(_yn){return A(_ym,[new T(function(){return _2l(_xH,new T(function(){return A(_yl,[_yn]);}));})]);}:function(_yo){return [1,_4j,new T(function(){return A(_ym,[new T(function(){return _2l(_xH,new T(function(){return A(_yl,[[1,_4i,_yo]]);}));})]);})];};}}}else{return E(_xZ);}}}},_yp=function(_yq,_yr,_ys,_yt){var _yu=new T(function(){return _45(_yq);}),_yv=new T(function(){return _wH(_yt);}),_yw=new T(function(){return _wX(_yt);}),_yx=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_x9,[_x1,A(_yr,[_wW]),_a]);}));}),_yy=new T(function(){return A(_wZ,[_ys,_g]);});return function(_yz){if(!E(new T(function(){var _yA=A(_yr,[_wW]),_yB=E(_wV),_yC=hs_eqWord64(_yA[1],_yB[1]);if(!E(_yC)){return false;}else{var _yD=hs_eqWord64(_yA[2],_yB[2]);return E(_yD)==0?false:true;}}))){var _yE=new T(function(){return A(_yu,[[1,_yz,new T(function(){return A(_yw,[new T(function(){return A(_yv,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _2l(_yz,_yx);}));})]);})]);})]]);}),_yF=A(_yy,[_yz]);if(!_yF[0]){return E(_yE);}else{var _yG=E(_yF[1]);return E(_yG[2])[0]==0?E(_yF[2])[0]==0?A(_yu,[[2,_yG[1]]]):E(_yE):E(_yE);}}else{return A(_yu,[[2,_yz]]);}};},_yH=[0],_yI=new T(function(){return [0,"value"];}),_yJ=function(_yK,_yL,_yM,_yN,_yO,_yP){var _yQ=E(_yK),_yR=_yQ[1],_yS=new T(function(){return A(_yQ[3],[_yH]);}),_yT=new T(function(){return _yp(_yQ,_yM,_yN,_yO);});return A(_yR,[new T(function(){return _wE(_yL,_yP);}),function(_yU){var _yV=E(_yU);return _yV[0]==0?E(_yS):A(_yR,[new T(function(){return A(_yL,[function(_){var _yW=jsGet(E(_yV[1])[1],E(_yI)[1]);return [1,new T(function(){return fromJSStr(_yW);})];}]);}),function(_yX){var _yY=E(_yX);return _yY[0]==0?E(_yS):A(_yT,[_yY[1]]);}]);}]);},_yZ=1,_z0=function(_z1){return E(E(_z1)[9]);},_z2=function(_z3){return E(E(_z3)[2]);},_z4=function(_z5){return E(E(_z5)[2]);},_z6=function(_z7){return E(E(_z7)[3]);},_z8=function(_z9){return E(E(_z9)[2]);},_za=function(_zb,_zc,_zd,_ze,_zf,_zg,_zh,_zi,_zj,_zk,_zl,_zm){var _zn=_wC(_zg),_zo=_zn[1],_zp=_zn[3],_zq=new T(function(){return _wA(_zi);}),_zr=new T(function(){return _z4(_zq);}),_zs=new T(function(){return _z6(_zg);}),_zt=new T(function(){return _z8(_zc);}),_zu=new T(function(){return _z0(_zi);});return A(_zo,[new T(function(){var _zv=E(_zk);if(!_zv[0]){var _zw=E(_zg);return _4p(_zj,_zw[1],_zw[2],_zw[3]);}else{return A(_zp,[_zv[1]]);}}),function(_zx){return A(_zo,[new T(function(){var _zy=E(_zj);return _z2(_zg);}),function(_zz){return A(_zn[2],[new T(function(){return A(_zs,[new T(function(){var _zA=E(new T(function(){var _zB=E(_zj);return [0,coercionToken];})),_zC=E(_zz);return [0,_zC[1],_zC[2],_yZ,_zC[4],_zC[5],_zC[6]];})]);}),new T(function(){var _zD=new T(function(){return A(_zp,[[0,new T(function(){return A(_zu,[_zx,_zl,new T(function(){var _zE=E(_zm);if(!_zE[0]){return [0];}else{var _zF=_zE[1],_zG=_25(_zf,_wU,_zF);return _zG[0]==0?A(_z8,[_zd,_zF]):E(_zG[1]);}}),_0,_b]);}),_b]]);});return A(_zo,[new T(function(){var _zH=E(_zh);return _yJ(_zH[1],_zH[2],_ze,_zb,_zi,_zx);}),function(_zI){var _zJ=E(_zI);switch(_zJ[0]){case 0:return E(_zD);case 1:return A(_zp,[[0,new T(function(){return A(_zr,[new T(function(){return A(_zu,[_zx,_zl,_zJ[1],_0,_b]);}),_zJ[2]]);}),_b]]);default:var _zK=_zJ[1];return A(_zp,[[0,new T(function(){return A(_zu,[_zx,_zl,new T(function(){var _zL=_25(_ze,_wU,_zK);return _zL[0]==0?A(_zt,[_zK]):E(_zL[1]);}),_0,_b]);}),[1,_zK]]]);}}]);})]);}]);}]);},_zM=function(_zN,_zO,_zP,_zQ,_zR){var _zS=new T(function(){return _w6(_zO);}),_zT=new T(function(){return _47(_zS);}),_zU=new T(function(){return _wj(_zT,_zO);}),_zV=new T(function(){return _wu(_zT,_zS);});return function(_9A,_9B,_zW){return _za(_zR,_zQ,_zQ,_zP,_zP,_zV,_zU,_zN,[0,coercionToken],_9A,_9B,_zW);};},_zX=function(_zY){return _cL(_oE(function(_zZ){var _A0=E(_zZ);return _A0[0]==0?A(_zY,[_A0[1]]):[2];}),new T(function(){return _oX(_A1,_zY);}));},_A1=function(_A2,_A3){return _zX(_A3);},_A4=function(_A5,_A6){var _A7=function(_A8,_A9){var _Aa=new T(function(){return A(_A9,[_a]);}),_Ab=new T(function(){return A(_A5,[_oW,function(_Ac){return _A7(_5z,function(_Ad){return A(_A9,[[1,_Ac,_Ad]]);});}]);});return _oE(function(_Ae){var _Af=E(_Ae);if(_Af[0]==2){var _Ag=E(_Af[1]);if(!_Ag[0]){return [2];}else{var _Ah=_Ag[2];switch(E(E(_Ag[1])[1])){case 44:return E(_Ah)[0]==0?!E(_A8)?[2]:E(_Ab):[2];case 93:return E(_Ah)[0]==0?E(_Aa):[2];default:return [2];}}}else{return [2];}});},_Ai=function(_Aj){var _Ak=new T(function(){return _cL(_A7(_0,_Aj),new T(function(){return A(_A5,[_oW,function(_Al){return _A7(_5z,function(_Am){return A(_Aj,[[1,_Al,_Am]]);});}]);}));});return _cL(_oE(function(_An){var _Ao=E(_An);if(_Ao[0]==2){var _Ap=E(_Ao[1]);return _Ap[0]==0?[2]:E(E(_Ap[1])[1])==91?E(_Ap[2])[0]==0?E(_Ak):[2]:[2];}else{return [2];}}),new T(function(){return _oX(function(_Aq,_Ar){return _Ai(_Ar);},_Aj);}));};return _Ai(_A6);},_As=function(_At){return _cL(_cL(_oE(function(_Au){var _Av=E(_Au);return _Av[0]==1?A(_At,[_Av[1]]):[2];}),new T(function(){return _A4(_A1,_At);})),new T(function(){return _oX(_Aw,_At);}));},_Aw=function(_Ax,_Ay){return _As(_Ay);},_Az=new T(function(){return _oX(_Aw,_dM);}),_AA=new T(function(){return _A4(_A1,_dM);}),_AB=function(_AC){var _AD=E(_AC);return _AD[0]==1?[3,_AD[1],_dL]:[2];},_AE=new T(function(){return _ob(_AB);}),_AF=function(_AG){return E(_AE);},_AH=function(_AI){return A(_ms,[_AI,_AF]);},_AJ=[1,_AH],_AK=new T(function(){return _cL(_AJ,_AA);}),_AL=new T(function(){return _cL(_AK,_Az);}),_AM=function(_AN){return _cB(_AL,_AN);},_AO=new T(function(){return _zX(_dM);}),_AP=function(_AN){return _cB(_AO,_AN);},_AQ=function(_AR){return E(_AP);},_AS=[0,_AQ,_AM,_A1,_Aw],_AT=function(_AU){return E(E(_AU)[4]);},_AV=function(_AW,_AX,_AY){return _A4(new T(function(){return _AT(_AW);}),_AY);},_AZ=function(_B0){var _B1=new T(function(){return _A4(new T(function(){return _AT(_B0);}),_dM);});return function(_9A){return _cB(_B1,_9A);};},_B2=function(_B3,_B4){var _B5=new T(function(){return A(_AT,[_B3,_B4,_dM]);});return function(_9A){return _cB(_B5,_9A);};},_B6=function(_B7){return [0,function(_AN){return _B2(_B7,_AN);},new T(function(){return _AZ(_B7);}),new T(function(){return _AT(_B7);}),function(_B8,_AN){return _AV(_B7,_B8,_AN);}];},_B9=new T(function(){return _B6(_AS);}),_Ba=unCStr("Prelude.(!!): negative index\n"),_Bb=new T(function(){return err(_Ba);}),_Bc=unCStr("Prelude.(!!): index too large\n"),_Bd=new T(function(){return err(_Bc);}),_Be=function(_Bf,_Bg){while(1){var _Bh=E(_Bf);if(!_Bh[0]){return E(_Bd);}else{var _Bi=E(_Bg);if(!_Bi){return E(_Bh[1]);}else{_Bf=_Bh[2];_Bg=_Bi-1|0;continue;}}}},_Bj=unCStr("ACK"),_Bk=unCStr("BEL"),_Bl=unCStr("BS"),_Bm=unCStr("SP"),_Bn=[1,_Bm,_a],_Bo=unCStr("US"),_Bp=[1,_Bo,_Bn],_Bq=unCStr("RS"),_Br=[1,_Bq,_Bp],_Bs=unCStr("GS"),_Bt=[1,_Bs,_Br],_Bu=unCStr("FS"),_Bv=[1,_Bu,_Bt],_Bw=unCStr("ESC"),_Bx=[1,_Bw,_Bv],_By=unCStr("SUB"),_Bz=[1,_By,_Bx],_BA=unCStr("EM"),_BB=[1,_BA,_Bz],_BC=unCStr("CAN"),_BD=[1,_BC,_BB],_BE=unCStr("ETB"),_BF=[1,_BE,_BD],_BG=unCStr("SYN"),_BH=[1,_BG,_BF],_BI=unCStr("NAK"),_BJ=[1,_BI,_BH],_BK=unCStr("DC4"),_BL=[1,_BK,_BJ],_BM=unCStr("DC3"),_BN=[1,_BM,_BL],_BO=unCStr("DC2"),_BP=[1,_BO,_BN],_BQ=unCStr("DC1"),_BR=[1,_BQ,_BP],_BS=unCStr("DLE"),_BT=[1,_BS,_BR],_BU=unCStr("SI"),_BV=[1,_BU,_BT],_BW=unCStr("SO"),_BX=[1,_BW,_BV],_BY=unCStr("CR"),_BZ=[1,_BY,_BX],_C0=unCStr("FF"),_C1=[1,_C0,_BZ],_C2=unCStr("VT"),_C3=[1,_C2,_C1],_C4=unCStr("LF"),_C5=[1,_C4,_C3],_C6=unCStr("HT"),_C7=[1,_C6,_C5],_C8=[1,_Bl,_C7],_C9=[1,_Bk,_C8],_Ca=[1,_Bj,_C9],_Cb=unCStr("ENQ"),_Cc=[1,_Cb,_Ca],_Cd=unCStr("EOT"),_Ce=[1,_Cd,_Cc],_Cf=unCStr("ETX"),_Cg=[1,_Cf,_Ce],_Ch=unCStr("STX"),_Ci=[1,_Ch,_Cg],_Cj=unCStr("SOH"),_Ck=[1,_Cj,_Ci],_Cl=unCStr("NUL"),_Cm=[1,_Cl,_Ck],_Cn=[0,92],_Co=unCStr("\\DEL"),_Cp=unCStr("\\a"),_Cq=unCStr("\\\\"),_Cr=unCStr("\\SO"),_Cs=unCStr("\\r"),_Ct=unCStr("\\f"),_Cu=unCStr("\\v"),_Cv=unCStr("\\n"),_Cw=unCStr("\\t"),_Cx=unCStr("\\b"),_Cy=function(_Cz,_CA){if(_Cz<=127){var _CB=E(_Cz);switch(_CB){case 92:return _2l(_Cq,_CA);case 127:return _2l(_Co,_CA);default:if(_CB<32){var _CC=E(_CB);switch(_CC){case 7:return _2l(_Cp,_CA);case 8:return _2l(_Cx,_CA);case 9:return _2l(_Cw,_CA);case 10:return _2l(_Cv,_CA);case 11:return _2l(_Cu,_CA);case 12:return _2l(_Ct,_CA);case 13:return _2l(_Cs,_CA);case 14:return _2l(_Cr,new T(function(){var _CD=E(_CA);return _CD[0]==0?[0]:E(E(_CD[1])[1])==72?unAppCStr("\\&",_CD):E(_CD);}));default:return _2l([1,_Cn,new T(function(){var _CE=_CC;return _CE>=0?_Be(_Cm,_CE):E(_Bb);})],_CA);}}else{return [1,[0,_CB],_CA];}}}else{return [1,_Cn,new T(function(){var _CF=jsShowI(_Cz);return _2l(fromJSStr(_CF),new T(function(){var _CG=E(_CA);if(!_CG[0]){return [0];}else{var _CH=E(_CG[1])[1];return _CH<48?E(_CG):_CH>57?E(_CG):unAppCStr("\\&",_CG);}}));})];}},_CI=[0,39],_CJ=[1,_CI,_a],_CK=unCStr("\'\\\'\'"),_CL=function(_CM){var _CN=E(E(_CM)[1]);return _CN==39?E(_CK):[1,_CI,new T(function(){return _Cy(_CN,_CJ);})];},_CO=[0,34],_CP=unCStr("\\\""),_CQ=function(_CR,_CS){var _CT=E(_CR);if(!_CT[0]){return E(_CS);}else{var _CU=_CT[2],_CV=E(E(_CT[1])[1]);return _CV==34?_2l(_CP,new T(function(){return _CQ(_CU,_CS);})):_Cy(_CV,new T(function(){return _CQ(_CU,_CS);}));}},_CW=function(_CX,_CY){return [1,_CO,new T(function(){return _CQ(_CX,[1,_CO,_CY]);})];},_CZ=function(_D0){return _2l(_CK,_D0);},_D1=function(_D2,_D3){var _D4=E(E(_D3)[1]);return _D4==39?E(_CZ):function(_D5){return [1,_CI,new T(function(){return _Cy(_D4,[1,_CI,_D5]);})];};},_D6=[0,_D1,_CL,_CW],_D7=function(_D8){return E(E(_D8)[3]);},_D9=function(_Da,_Db){return A(_D7,[_Da,_Db,_a]);},_Dc=function(_Dd,_De,_Df){return _3b(new T(function(){return _D7(_Dd);}),_De,_Df);},_Dg=function(_Dh){var _Di=new T(function(){return _D7(_Dh);});return [0,function(_Dj){return E(_Di);},function(_D0){return _D9(_Dh,_D0);},function(_Dk,_D0){return _Dc(_Dh,_Dk,_D0);}];},_Dl=new T(function(){return _Dg(_D6);}),_Dm=new T(function(){return _zM(_w4,_w5,_wU,_Dl,_B9);}),_Dn=function(_Do,_Dp,_Dq,_Dr){return A(_Do,[new T(function(){return A(_Do,[new T(function(){return A(_Dp,[[0,_Dr,_Dr]]);}),function(_Ds){return A(_Dp,[[0,_A,new T(function(){var _Dt=E(E(_Ds)[1]);return [0,_Dt[1],_Dt[2],_Dt[3],_Dt[4],_5z,_Dt[6]];})]]);}]);}),function(_Du){return A(_Dq,[new T(function(){return E(E(_Du)[2]);})]);}]);},_Dv=function(_Dw){return E(E(_Dw)[1]);},_Dx=function(_Dy,_Dz,_DA,_DB,_DC){var _DD=new T(function(){return _Dv(_Dy);});return A(_Dz,[new T(function(){return A(_Dz,[_DB,function(_DE){return A(_DA,[[0,_DE,_DC]]);}]);}),function(_DF){return A(_DA,[[0,[0,_DD,[1,new T(function(){return E(E(_DF)[1]);})]],new T(function(){return E(E(_DF)[2]);})]]);}]);},_DG=function(_DH,_DI,_DJ){var _DK=new T(function(){return _w6(_DI);});return function(_DL){var _DM=E(_DK);return _Dn(_DM[1],_DM[3],new T(function(){var _DN=new T(function(){return _wA(_DH);});return function(_DO){var _DP=E(_DK);return _Dx(_DN,_DP[1],_DP[3],A(_w8,[_DI,_DJ]),_DO);};}),_DL);};},_DQ=function(_DR){return _DG(_w4,_w5,_DR);},_DS=unCStr("class"),_DT=function(_1w,_){return _1x(_J,_1w,_);},_DU=function(_DV,_DW,_DX,_){var _DY=A(_DW,[_DX,_]),_DZ=E(_DY),_E0=E(_DZ[1]);return [0,[0,function(_E1,_){var _E2=_1x(_J,_E1,_),_E3=A(_B,[_19,_E2,_1h,_DV,_]),_E4=A(_E0[1],[_E2,_]);return _E2;},_E0[2]],_DZ[2]];},_E5=new T(function(){return _4p(_1D,_4c,_1H,_1E);}),_E6=new T(function(){return _4p(_1D,_4c,_1H,_1E);}),_E7=function(_E8,_E9,_Ea,_){var _Eb=A(_E5,[_Ea,_]),_Ec=A(_E6,[new T(function(){return E(E(_Eb)[2]);}),_]),_Ed=E(_Ec),_Ee=_Ed[1],_Ef=E(_Ed[2]),_Eg=_Ef[2],_Eh=E(_Ef[4]),_Ei=new T(function(){return E(E(_Eb)[1]);}),_Ej=function(_Ek){var _El=new T(function(){return A(_E9,[_Ek]);});return function(_Em,_){var _En=A(_El,[_Em,_]),_Eo=E(_En),_Ep=E(_Eo[1]);return [0,[0,function(_Eq,_){var _Er=E(_Ei),_Es=jsFind(toJSStr(_Er)),_Et=E(_Es);if(!_Et[0]){return _4D(_Er);}else{var _Eu=E(_Et[1]),_Ev=A(_7,[E(_Eu[1]),_]),_Ew=jsKillChild(E(_Eu)[1],_Ev),_Ex=A(_Ep[1],[_Eq,_]);return _Eq;}},_Ep[2]],_Eo[2]];};},_Ey=_DU(_Ei,_E8,[0,_Ef[1],_Eg,_Ef[3],[0,function(_){return _4F(function(_Ez,_){var _EA=_DU(_Ei,_E8,new T(function(){var _EB=E(_Ez);return [0,_EB[1],_Eg,_EB[3],_EB[4],_EB[5],_EB[6]];}),_);return [0,[0,_3C,E(E(_EA)[1])[2]],_Ez];},_Ee,_);},function(_EC,_){var _ED=_4F(new T(function(){return _Ej(_EC);}),_Ee,_),_EE=E(_ED);return _EE[0]==0?_b:A(_Eh[2],[_EE[1],_]);}],_Ef[5],_Ef[6]],_),_EF=E(_Ey),_EG=_EF[2],_EH=E(_EF[1]),_EI=_EH[1],_EJ=new T(function(){return _1p(_DT,[1,[0,_1h,_Ee],_a]);}),_EK=E(_EH[2]);if(!_EK[0]){return [0,[0,function(_EL,_){var _EM=A(_EI,[_EL,_]),_EN=A(_EJ,[_EL,_]);return _EL;},_b],new T(function(){var _EO=E(_EG);return [0,_EO[1],_EO[2],_EO[3],_Eh,_EO[5],_EO[6]];})];}else{var _EP=A(_Ej,[_EK[1],new T(function(){var _EQ=E(_EG);return [0,_EQ[1],_EQ[2],_EQ[3],_Eh,_EQ[5],_EQ[6]];}),_]),_ER=E(_EP),_ES=E(_ER[1]);return [0,[0,function(_ET,_){var _EU=A(_EI,[_ET,_]),_EV=A(_EJ,[_ET,_]),_EW=A(_ES[1],[_EV,_]);return _ET;},_ES[2]],_ER[2]];}},_EX=function(_EY,_EZ,_F0){var _F1=new T(function(){return _84(new T(function(){return A(_Dm,[_b,_sY,[1,_F0]]);}),_sC);}),_F2=new T(function(){return _9u(new T(function(){if(!E(_EZ)){var _F3=new T(function(){return _t0(_62,_F0);});return function(_F4,_){var _F5=A(_F3,[_F4,_]),_F6=A(_B,[_19,_F5,_DS,_sX,_]);return _F5;};}else{return _t0(_62,_F0);}}),_sB);});return function(_9A,_9B){return _E7(_F2,function(_F7,_F8,_){return (function(_F8,_){return _E7(function(_F8,_){return _54(function(_F9,_){var _Fa=A(_F1,[_F9,_]),_Fb=E(_Fa),_Fc=E(_Fb[1]);return [0,[0,function(_Fd,_){var _Fe=A(_Fc[1],[_Fd,_]),_Ff=A(_B,[_19,_Fe,_DS,_sW,_]);return _Fe;},_Fc[2]],_Fb[2]];},_sQ,_F8,_);},function(_Fg){var _Fh=new T(function(){return _EX(_EY,_EZ,_Fg);}),_Fi=new T(function(){return _sr(_DQ,_EY,_Fg,_EZ);});return function(_9A,_9B){return _54(_Fi,function(_Fj){return E(_Fh);},_9A,_9B);};},_F8,_);})(_F8,_);},_9A,_9B);};},_Fk=unCStr(" item left"),_Fl=[0,49],_Fm=[1,_Fl,_a],_Fn=unCStr("strong"),_Fo=function(_Fp,_Fq){var _Fr=new T(function(){return A(_Fp,[_Fq]);});return function(_Fs,_){var _Ft=jsCreateElem(toJSStr(E(_Fn))),_Fu=jsAppendChild(_Ft,E(_Fs)[1]),_Fv=[0,_Ft],_Fw=A(_Fr,[_Fv,_]);return _Fv;};},_Fx=new T(function(){return _Fo(_62,_Fm);}),_Fy=function(_Fz,_){var _FA=A(_Fx,[_Fz,_]),_FB=_62(_Fk,_Fz,_);return _Fz;},_FC=unCStr(" items left"),_FD=function(_FE){return function(_FF,_){return [0,[0,new T(function(){var _FG=E(E(_FE)[1]);if(_FG==1){return E(_Fy);}else{var _FH=new T(function(){return _Fo(_62,new T(function(){return _4k(0,_FG,_a);}));});return function(_FI,_){var _FJ=A(_FH,[_FI,_]),_FK=_62(_FC,_FI,_);return _FI;};}}),_9t],_FF];};},_FL=function(_FM,_){var _FN=_5S(_sh,_qX,_),_FO=_sj(_FN,_);return [0,[0,_3C,[1,_FO]],new T(function(){var _FP=E(_FM);return [0,_FP[1],_FP[2],_FP[3],_FP[4],_5z,_FP[6]];})];},_FQ=function(_FR){return E(E(_FR)[1]);},_FS=function(_FT,_FU){return E(E(_FU)[2])==0?false:true;},_FV=function(_FW){var _FX=new T(function(){return [0,_5w(_5A(_FS,_FQ(_FW)))];});return function(_FY,_){return [0,[0,_3C,[1,_FX]],_FY];};},_FZ=function(_F8,_){return _54(_FL,_FV,_F8,_);},_G0=function(_F8,_){return _54(_FZ,_FD,_F8,_);},_G1=function(_G2,_G3,_G4){var _G5=new T(function(){return _EX(_G2,_G3,_G4);}),_G6=new T(function(){return _sr(_DQ,_G2,_G4,_G3);});return function(_G7,_){var _G8=A(_G6,[_G7,_]),_G9=E(_G8),_Ga=_K(_1f,_H,_G0,_G9[2],_),_Gb=E(_Ga),_Gc=_K(_1d,_H,_Gd,_Gb[2],_),_Ge=E(_Gc),_Gf=A(_G5,[_Ge[2],_]),_Gg=E(_Gf),_Gh=E(_Gg[1]);return [0,[0,function(_Gi,_){var _Gj=A(E(_G9[1])[1],[_Gi,_]),_Gk=A(E(_Gb[1])[1],[_Gi,_]),_Gl=A(E(_Ge[1])[1],[_Gi,_]),_Gm=A(_Gh[1],[_Gi,_]);return _Gi;},_Gh[2]],_Gg[2]];};},_Gn=unCStr("li"),_Go=function(_Gp,_Gq){var _Gr=new T(function(){return A(_Gp,[_Gq]);});return function(_Gs,_){var _Gt=jsCreateElem(toJSStr(E(_Gn))),_Gu=jsAppendChild(_Gt,E(_Gs)[1]),_Gv=[0,_Gt],_Gw=A(_Gr,[_Gv,_]);return _Gv;};},_Gx=new T(function(){return _rH(_sh,_qX);}),_Gy=unCStr("destroy"),_Gz=function(_GA){return E(_GA);},_GB=new T(function(){return _68(_Gz,_3C);}),_GC=function(_GD,_){var _GE=A(_GB,[_GD,_]),_GF=A(_B,[_19,_GE,_DS,_Gy,_]);return _GE;},_GG=new T(function(){return _9u(_GC,_60);}),_GH=unCStr("check"),_GI=unCStr("Pattern match failure in do expression at todo.hs:150:9-22"),_GJ=new T(function(){return _sM(_GI);}),_GK=unCStr("toggle"),_GL=function(_GM,_GN,_){var _GO=jsGet(_GM,toJSStr(E(_GN)));return new T(function(){return fromJSStr(_GO);});},_GP=function(_GQ,_GR,_){return _GL(E(_GQ)[1],_GR,_);},_GS=unCStr("checkbox"),_GT=unCStr("true"),_GU=function(_GV,_GW){var _GX=new T(function(){return _w6(_GW);}),_GY=new T(function(){return _4p([0,coercionToken],_47(_GX),function(_GZ){return _wn(_GX,_GZ);},function(_H0,_H1){return _wq(_GX,_H0,_H1);});}),_H2=new T(function(){return _45(_GX);}),_H3=new T(function(){return _45(_GX);}),_H4=new T(function(){return _3F(_GX);}),_H5=new T(function(){return _3F(_GX);}),_H6=new T(function(){return _45(_GX);}),_H7=new T(function(){return _3F(_GX);}),_H8=new T(function(){return _45(_GX);}),_H9=new T(function(){return _3F(_GX);}),_Ha=new T(function(){return _z0(_GV);});return function(_Hb,_Hc){var _Hd=new T(function(){return !E(_Hb)?[0]:E(_GT);});return function(_He){return A(_H4,[new T(function(){return A(_GY,[_He]);}),function(_Hf){var _Hg=new T(function(){return E(E(_Hf)[1]);}),_Hh=new T(function(){return _wa(_GW,function(_){return jsFind(toJSStr(E(_Hg)));});}),_Hi=new T(function(){return A(_Ha,[_Hg,_GS,_Hc,_Hb,_b]);});return A(_H9,[new T(function(){var _Hj=new T(function(){return E(E(_Hf)[2]);});return A(_H8,[[0,_Hj,_Hj]]);}),function(_Hk){return A(_H7,[new T(function(){return A(_H6,[[0,_A,new T(function(){var _Hl=E(E(_Hk)[1]);return [0,_Hl[1],_Hl[2],_yZ,_Hl[4],_Hl[5],_Hl[6]];})]]);}),function(_Hm){return A(_H5,[new T(function(){return A(_Hh,[new T(function(){return E(E(_Hm)[2]);})]);}),function(_Hn){return A(_H4,[new T(function(){var _Ho=E(_Hn),_Hp=_Ho[2],_Hq=E(_Ho[1]);return _Hq[0]==0?A(_H3,[[0,_Hd,_Hp]]):A(_wa,[_GW,function(_){return _GP(_Hq[1],_uk,_);},_Hp]);}),function(_Hr){return A(_H2,[[0,[0,_Hi,[1,[0,new T(function(){return !_6E(E(_Hr)[1],_GT)?[0]:E([1,_Hc,_a]);})]]],new T(function(){return E(E(_Hr)[2]);})]]);}]);}]);}]);}]);}]);};};},_Hs=new T(function(){return _GU(_w4,_w5);}),_Ht=function(_Hu,_Hv){while(1){var _Hw=(function(_Hx,_Hy){var _Hz=E(_Hy);switch(_Hz[0]){case 0:_Hu=new T(function(){return _Ht(_Hx,_Hz[4]);});_Hv=_Hz[3];return null;case 1:var _HA=_Hz[1];return [1,function(_F8,_){return _E7(function(_HB,_){var _HC=_54(function(_HD,_){var _HE=_5S(_sh,_qX,_),_HF=_sj(_HE,_);return [0,[0,_3C,[1,new T(function(){return _a4(_HA,E(_HF)[1]);})]],new T(function(){var _HG=E(_HD);return [0,_HG[1],_HG[2],_HG[3],_HG[4],_5z,_HG[6]];})];},function(_HH){var _HI=E(_HH);if(!_HI[0]){return E(_GJ);}else{var _HJ=E(_HI[1]),_HK=_HJ[1],_HL=new T(function(){return _G1([0,_HA],_ab,_HK);}),_HM=new T(function(){return _G1([0,_HA],_ac,_HK);}),_HN=new T(function(){return _84(new T(function(){return A(_Hs,[new T(function(){return E(_HJ[2])==0?true:false;}),_GH]);}),_60);});return function(_9A,_9B){return _54(function(_HO,_){var _HP=A(_HN,[_HO,_]),_HQ=E(_HP),_HR=E(_HQ[1]);return [0,[0,function(_HS,_){var _HT=A(_HR[1],[_HS,_]),_HU=A(_B,[_19,_HT,_DS,_GK,_]);return _HT;},_HR[2]],_HQ[2]];},function(_HV){var _HW=E(E(_HV)[1]);return _HW[0]==0?E(_HL):!_6E(_HW[1],_GH)?E(_HL):E(_HW[2])[0]==0?E(_HM):E(_HL);},_9A,_9B);};}},_HB,_),_HX=E(_HC),_HY=A(_GG,[_HX[2],_]),_HZ=E(_HY),_I0=E(_HZ[1]);return [0,[0,new T(function(){return _Go(_Gz,function(_I1,_){var _I2=A(E(_HX[1])[1],[_I1,_]),_I3=A(_I0[1],[_I1,_]);return _I1;});}),_I0[2]],_HZ[2]];},function(_I4,_F8,_){return (function(_I5,_){var _I6=_5S(_sh,_qX,_),_I7=_sj(_I6,_),_I8=_5S(_sh,_qX,_),_I9=_sj(_I8,_),_Ia=A(_Gx,[[0,new T(function(){return _s7(_9Q(_HA,E(_I7)[1]));}),E(_I9)[2]],_]);return [0,[0,_3C,[1,_Ia]],new T(function(){var _Ib=E(_I5);return [0,_Ib[1],_Ib[2],_Ib[3],_Ib[4],_5z,_Ib[6]];})];})(_F8,_);},_F8,_);},_Hx];default:return E(_Hx);}})(_Hu,_Hv);if(_Hw!=null){return _Hw;}}},_Ic=function(_Id,_Ie){while(1){var _If=(function(_Ig,_Ih){var _Ii=E(_Ih);if(!_Ii[0]){return E(_Ig);}else{_Id=function(_Ij,_){var _Ik=A(_Ig,[_Ij,_]),_Il=E(_Ik),_Im=E(_Il[1]),_In=A(_Ii[1],[_Il[2],_]),_Io=E(_In),_Ip=E(_Io[1]);return [0,[0,function(_Iq,_){var _Ir=A(_Im[1],[_Iq,_]),_Is=A(_Ip[1],[_Iq,_]);return _Iq;},new T(function(){var _It=E(_Im[2]);return _It[0]==0?E(_Ip[2]):E(_It);})],_Io[2]];};_Ie=_Ii[2];return null;}})(_Id,_Ie);if(_If!=null){return _If;}}},_Iu=function(_Iv,_Iw){while(1){var _Ix=E(_Iv);if(!_Ix[0]){return E(_Iw);}else{_Iv=_Ix[2];var _Iy=[1,_Ix[1],_Iw];_Iw=_Iy;continue;}}},_Iz=function(_IA){return function(_9A,_9B){return _K(_9M,_H,new T(function(){var _IB=E(_IA);if(!_IB[0]){var _IC=_IB[3],_ID=_IB[4];return _IB[2]>=0?_Ic(_9O,_Iu(_Ht(new T(function(){return _Ht(_a,_ID);}),_IC),_a)):_Ic(_9O,_Iu(_Ht(new T(function(){return _Ht(_a,_IC);}),_ID),_a));}else{return _Ic(_9O,_Iu(_Ht(_a,_IB),_a));}}),_9A,_9B);};},_IE=unCStr("active"),_IF=[0,_a],_IG=[1,_IF],_IH=function(_II,_){return _II;},_IJ=unCStr("Main"),_IK=unCStr("PresentationMode"),_IL=[0,I_fromBits([2632422978,3726471947]),I_fromBits([3374154759,1744714295]),_z,_IJ,_IK],_IM=[0,I_fromBits([2632422978,3726471947]),I_fromBits([3374154759,1744714295]),_IL,_a],_IN=function(_IO){return E(_IM);},_IP=function(_IQ,_IR){var _IS=hs_leWord64(_IQ,_IR);return E(_IS)==0?false:true;},_IT=function(_IU,_IV,_IW,_IX){var _IY=hs_eqWord64(_IU,_IW);if(!E(_IY)){var _IZ=hs_leWord64(_IU,_IW);return E(_IZ)==0?false:true;}else{return _IP(_IV,_IX);}},_J0=function(_J1,_J2){var _J3=E(_J1),_J4=_J3[1],_J5=_J3[2],_J6=E(_J2),_J7=_J6[1],_J8=_J6[2],_J9=hs_eqWord64(_J4,_J7);if(!E(_J9)){return !_IT(_J4,_J5,_J7,_J8)?2:0;}else{var _Ja=hs_eqWord64(_J5,_J8);return E(_Ja)==0?!_IT(_J4,_J5,_J7,_J8)?2:0:1;}},_Jb=function(_Jc,_Jd){while(1){var _Je=E(_Jc),_Jf=E(_Jd);if(!_Jf[0]){switch(_J0(_Je,_Jf[2])){case 0:_Jc=_Je;_Jd=_Jf[4];continue;case 1:return [1,_Jf[3]];default:_Jc=_Je;_Jd=_Jf[5];continue;}}else{return [0];}}},_Jg=function(_Jh,_Ji,_Jj,_Jk){var _Jl=E(_Ji),_Jm=_Jl[1],_Jn=_Jl[3],_Jo=new T(function(){return A(_Jk,[_wW]);}),_Jp=new T(function(){return A(_Jn,[_b]);});return A(_Jm,[new T(function(){return A(_Jm,[_Jj,function(_Jq){return A(_Jn,[new T(function(){var _Jr=E(_Jh);return E(E(_Jq)[6]);})]);}]);}),function(_Js){var _Jt=_Jb(_Jo,_Js);return _Jt[0]==0?E(_Jp):A(_Jn,[[1,_Jt[1]]]);}]);},_Ju=new T(function(){return _Jg(_1D,_4c,_1H,_IN);}),_Jv=function(_Jw,_){var _Jx=A(_Ju,[_Jw,_]);return [0,[0,_IH,new T(function(){var _Jy=E(E(_Jx)[1]);return _Jy[0]==0?E(_IG):E(_Jy);})],new T(function(){return E(E(_Jx)[2]);})];},_Jz=function(_JA,_JB,_){return _54(_Jv,function(_JC){var _JD=E(_JC)[1];return function(_JE,_){return [0,[0,_3C,[1,new T(function(){var _JF=new T(function(){return _6E(_JD,_IE);}),_JG=new T(function(){return _6E(_JD,_a);});return _5A(function(_JH,_JI){var _JJ=E(_JI)[2];return !E(_JG)?!E(_JF)?E(_JJ)==0?true:false:E(_JJ)==0?false:true:true;},_JA);})]],_JE];};},_JB,_);},_JK=function(_JL,_JM,_){return _54(function(_F8,_){return _Jz(E(_JL)[1],_F8,_);},_Iz,_JM,_);},_JN=function(_JO,_){var _JP=_54(_FL,_JK,_JO,_),_JQ=E(_JP);return [0,[0,function(_JR,_){var _JS=A(E(_JQ[1])[1],[_JR,_]);return _JR;},_9L],_JQ[2]];},_JT=function(_JU,_JV,_){return _JN(_JV,_);},_JW=new T(function(){return _rH(_sh,_qX);}),_JX=[0,_3C,_9L],_JY=function(_JZ,_){return [0,_JX,_JZ];},_K0=function(_K1,_K2){return E(E(_K2)[2])==0?true:false;},_K3=function(_K4){return _5w(_5A(_K0,_K4))<=0?E(_JY):function(_9A,_9B){return _54(_9K,function(_K5,_F8,_){return (function(_F8,_){return _54(function(_K6,_){var _K7=_5S(_sh,_qX,_),_K8=_sj(_K7,_),_K9=A(_JW,[[0,new T(function(){return _s7(_5A(_FS,_K4));}),E(_K8)[2]],_]);return [0,[0,_3C,[1,_K9]],new T(function(){var _Ka=E(_K6);return [0,_Ka[1],_Ka[2],_Ka[3],_Ka[4],_5z,_Ka[6]];})];},_JT,_F8,_);})(_F8,_);},_9A,_9B);};},_Kb=function(_Kc){return _K3(E(_Kc)[1]);},_Gd=function(_F8,_){return _54(_FL,_Kb,_F8,_);},_Kd=unCStr("info"),_Ke=unCStr("unsel"),_Kf=unCStr("selected"),_Kg=function(_F8,_){return _62(_bp,_F8,_);},_Kh=new T(function(){return _u7(_wN,_wS);}),_Ki=new T(function(){return _Dg(_D6);}),_Kj=function(_Kk,_Kl,_){var _Km=jsWriteHandle(E(_Kk)[1],toJSStr(E(_Kl)));return _A;},_Kn=[0,10],_Ko=[1,_Kn,_a],_Kp=function(_Kq,_Kr,_){var _Ks=E(_Kq),_Kt=jsWriteHandle(_Ks[1],toJSStr(E(_Kr)));return _Kj(_Ks,_Ko,_);},_Ku=[0,97],_Kv=[1,_Ku,_a],_Kw=function(_Kx,_Ky){var _Kz=new T(function(){return A(_Kx,[_Ky]);});return function(_KA,_){var _KB=jsCreateElem(toJSStr(_Kv)),_KC=jsAppendChild(_KB,E(_KA)[1]),_KD=[0,_KB],_KE=A(_Kz,[_KD,_]);return _KD;};},_KF=unCStr("href"),_KG=function(_){var _=0,_KH=newMVar(),_=putMVar(_KH,_b);return [0,_KH];},_KI=new T(function(){return _2(_KG);}),_KJ=new T(function(){return _4p(_1D,_4c,_1H,_1E);}),_KK=new T(function(){return A(_wU,[_u6]);}),_KL=unCStr("EMPTY"),_KM=[1,_CO,_a],_KN=new T(function(){return _CQ(_KL,_KM);}),_KO=[1,_CO,_KN],_KP=function(_KQ,_KR,_){var _=putMVar(E(_KQ)[1],_KR);return _A;},_KS=function(_KT,_KU,_){return _6R(function(_){return _KP(_KI,_KT,_);},_KU,_);},_KV=function(_){var _KW=E(_KI)[1],_KX=takeMVar(_KW),_=putMVar(_KW,_KX);return _KX;},_KY=function(_){var _=0,_KZ=jsMkStdout();return [0,_KZ];},_L0=new T(function(){return _2(_KY);}),_L1=function(_L2,_L3,_L4,_L5){var _L6=new T(function(){return _Kw(_Gz,_L5);}),_L7=new T(function(){return unAppCStr("#/",new T(function(){var _L8=A(_L3,[_L4]),_L9=E(_KK),_La=hs_eqWord64(_L8[1],_L9[1]);if(!E(_La)){return A(_z8,[_L2,_L4]);}else{var _Lb=hs_eqWord64(_L8[2],_L9[2]);return E(_Lb)==0?A(_z8,[_L2,_L4]):E(_L4);}}));});return function(_Lc,_){var _Ld=A(_KJ,[_Lc,_]),_Le=0,_Lf=function(_,_Lg,_Lh){var _Li=new T(function(){return E(E(_Ld)[1]);}),_Lj=function(_Lk,_){var _Ll=A(_L6,[_Lk,_]),_Lm=A(_B,[_19,_Ll,_KF,_L7,_]),_Ln=E(_Ll),_Lo=jsSetCB(_Ln[1],E(_7K)[1],E([0,function(_Lp,_Lq,_){return (function(_){var _Lr=0;if(!E(_Lr)){return (function(_){var _Ls=takeMVar(E(_KI)[1]),_Lt=jsCatch(function(_){return (function(_){return [1,_Li];})();},function(_1w,_){return _KS(_Ls,_1w,_);});return _KP(_KI,_Lt,_);})();}else{var _Lu=takeMVar(E(_KI)[1]),_Lv=jsCatch(function(_){return [1,_Li];},function(_1w,_){return _KS(_Lu,_1w,_);});return _KP(_KI,_Lv,_);}})(_);}])[1]);return _Ln;},_Lw=E(_Lg);if(!_Lw[0]){var _Lx=_Kp(_L0,_KO,_);return [0,[0,_Lj,_b],_Lh];}else{if(!_6E(_Lw[1],_Li)){var _Ly=_Kp(_L0,_KO,_);return [0,[0,_Lj,_b],_Lh];}else{return [0,[0,_Lj,[1,_L4]],_Lh];}}};if(!E(_Le)){var _Lz=_KV();return _Lf(_,_Lz,new T(function(){return E(E(_Ld)[2]);}));}else{var _LA=E(_KI)[1],_LB=takeMVar(_LA),_=putMVar(_LA,_LB);return _Lf(_,_LB,new T(function(){return E(E(_Ld)[2]);}));}};},_LC=new T(function(){return _L1(_Ki,_Kh,_sX,_Kg);}),_LD=function(_F8,_){return _62(_bq,_F8,_);},_LE=new T(function(){return _L1(_Ki,_Kh,_IE,_LD);}),_LF=unCStr("All"),_LG=function(_F8,_){return _62(_LF,_F8,_);},_LH=new T(function(){return _L1(_Ki,_Kh,_a,_LG);}),_LI=unCStr("Failure in Data.Map.balanceL"),_LJ=new T(function(){return err(_LI);}),_LK=function(_LL,_LM,_LN,_LO){var _LP=E(_LO);if(!_LP[0]){var _LQ=_LP[1],_LR=E(_LN);if(!_LR[0]){var _LS=_LR[1],_LT=_LR[2],_LU=_LR[3];if(_LS<=(imul(3,_LQ)|0)){return [0,(1+_LS|0)+_LQ|0,E(E(_LL)),_LM,E(_LR),E(_LP)];}else{var _LV=E(_LR[4]);if(!_LV[0]){var _LW=_LV[1],_LX=E(_LR[5]);if(!_LX[0]){var _LY=_LX[1],_LZ=_LX[2],_M0=_LX[3],_M1=_LX[4];if(_LY>=(imul(2,_LW)|0)){var _M2=function(_M3){var _M4=E(_LX[5]);return _M4[0]==0?[0,(1+_LS|0)+_LQ|0,E(_LZ),_M0,E([0,(1+_LW|0)+_M3|0,E(_LT),_LU,E(_LV),E(_M1)]),E([0,(1+_LQ|0)+_M4[1]|0,E(E(_LL)),_LM,E(_M4),E(_LP)])]:[0,(1+_LS|0)+_LQ|0,E(_LZ),_M0,E([0,(1+_LW|0)+_M3|0,E(_LT),_LU,E(_LV),E(_M1)]),E([0,1+_LQ|0,E(E(_LL)),_LM,E(_9),E(_LP)])];},_M5=E(_M1);return _M5[0]==0?_M2(_M5[1]):_M2(0);}else{return [0,(1+_LS|0)+_LQ|0,E(_LT),_LU,E(_LV),E([0,(1+_LQ|0)+_LY|0,E(E(_LL)),_LM,E(_LX),E(_LP)])];}}else{return E(_LJ);}}else{return E(_LJ);}}}else{return [0,1+_LQ|0,E(E(_LL)),_LM,E(_9),E(_LP)];}}else{var _M6=E(_LN);if(!_M6[0]){var _M7=_M6[1],_M8=_M6[2],_M9=_M6[3],_Ma=_M6[5],_Mb=E(_M6[4]);if(!_Mb[0]){var _Mc=_Mb[1],_Md=E(_Ma);if(!_Md[0]){var _Me=_Md[1],_Mf=_Md[2],_Mg=_Md[3],_Mh=_Md[4];if(_Me>=(imul(2,_Mc)|0)){var _Mi=function(_Mj){var _Mk=E(_Md[5]);return _Mk[0]==0?[0,1+_M7|0,E(_Mf),_Mg,E([0,(1+_Mc|0)+_Mj|0,E(_M8),_M9,E(_Mb),E(_Mh)]),E([0,1+_Mk[1]|0,E(E(_LL)),_LM,E(_Mk),E(_9)])]:[0,1+_M7|0,E(_Mf),_Mg,E([0,(1+_Mc|0)+_Mj|0,E(_M8),_M9,E(_Mb),E(_Mh)]),E([0,1,E(E(_LL)),_LM,E(_9),E(_9)])];},_Ml=E(_Mh);return _Ml[0]==0?_Mi(_Ml[1]):_Mi(0);}else{return [0,1+_M7|0,E(_M8),_M9,E(_Mb),E([0,1+_Me|0,E(E(_LL)),_LM,E(_Md),E(_9)])];}}else{return [0,3,E(_M8),_M9,E(_Mb),E([0,1,E(E(_LL)),_LM,E(_9),E(_9)])];}}else{var _Mm=E(_Ma);return _Mm[0]==0?[0,3,E(_Mm[2]),_Mm[3],E([0,1,E(_M8),_M9,E(_9),E(_9)]),E([0,1,E(E(_LL)),_LM,E(_9),E(_9)])]:[0,2,E(E(_LL)),_LM,E(_M6),E(_9)];}}else{return [0,1,E(E(_LL)),_LM,E(_9),E(_9)];}}},_Mn=unCStr("Failure in Data.Map.balanceR"),_Mo=new T(function(){return err(_Mn);}),_Mp=function(_Mq,_Mr,_Ms,_Mt){var _Mu=E(_Ms);if(!_Mu[0]){var _Mv=_Mu[1],_Mw=E(_Mt);if(!_Mw[0]){var _Mx=_Mw[1],_My=_Mw[2],_Mz=_Mw[3];if(_Mx<=(imul(3,_Mv)|0)){return [0,(1+_Mv|0)+_Mx|0,E(E(_Mq)),_Mr,E(_Mu),E(_Mw)];}else{var _MA=E(_Mw[4]);if(!_MA[0]){var _MB=_MA[1],_MC=_MA[2],_MD=_MA[3],_ME=_MA[4],_MF=E(_Mw[5]);if(!_MF[0]){var _MG=_MF[1];if(_MB>=(imul(2,_MG)|0)){var _MH=function(_MI){var _MJ=E(_Mq),_MK=E(_MA[5]);return _MK[0]==0?[0,(1+_Mv|0)+_Mx|0,E(_MC),_MD,E([0,(1+_Mv|0)+_MI|0,E(_MJ),_Mr,E(_Mu),E(_ME)]),E([0,(1+_MG|0)+_MK[1]|0,E(_My),_Mz,E(_MK),E(_MF)])]:[0,(1+_Mv|0)+_Mx|0,E(_MC),_MD,E([0,(1+_Mv|0)+_MI|0,E(_MJ),_Mr,E(_Mu),E(_ME)]),E([0,1+_MG|0,E(_My),_Mz,E(_9),E(_MF)])];},_ML=E(_ME);return _ML[0]==0?_MH(_ML[1]):_MH(0);}else{return [0,(1+_Mv|0)+_Mx|0,E(_My),_Mz,E([0,(1+_Mv|0)+_MB|0,E(E(_Mq)),_Mr,E(_Mu),E(_MA)]),E(_MF)];}}else{return E(_Mo);}}else{return E(_Mo);}}}else{return [0,1+_Mv|0,E(E(_Mq)),_Mr,E(_Mu),E(_9)];}}else{var _MM=E(_Mt);if(!_MM[0]){var _MN=_MM[1],_MO=_MM[2],_MP=_MM[3],_MQ=_MM[5],_MR=E(_MM[4]);if(!_MR[0]){var _MS=_MR[1],_MT=_MR[2],_MU=_MR[3],_MV=_MR[4],_MW=E(_MQ);if(!_MW[0]){var _MX=_MW[1];if(_MS>=(imul(2,_MX)|0)){var _MY=function(_MZ){var _N0=E(_Mq),_N1=E(_MR[5]);return _N1[0]==0?[0,1+_MN|0,E(_MT),_MU,E([0,1+_MZ|0,E(_N0),_Mr,E(_9),E(_MV)]),E([0,(1+_MX|0)+_N1[1]|0,E(_MO),_MP,E(_N1),E(_MW)])]:[0,1+_MN|0,E(_MT),_MU,E([0,1+_MZ|0,E(_N0),_Mr,E(_9),E(_MV)]),E([0,1+_MX|0,E(_MO),_MP,E(_9),E(_MW)])];},_N2=E(_MV);return _N2[0]==0?_MY(_N2[1]):_MY(0);}else{return [0,1+_MN|0,E(_MO),_MP,E([0,1+_MS|0,E(E(_Mq)),_Mr,E(_9),E(_MR)]),E(_MW)];}}else{return [0,3,E(_MT),_MU,E([0,1,E(E(_Mq)),_Mr,E(_9),E(_9)]),E([0,1,E(_MO),_MP,E(_9),E(_9)])];}}else{var _N3=E(_MQ);return _N3[0]==0?[0,3,E(_MO),_MP,E([0,1,E(E(_Mq)),_Mr,E(_9),E(_9)]),E(_N3)]:[0,2,E(E(_Mq)),_Mr,E(_9),E(_MM)];}}else{return [0,1,E(E(_Mq)),_Mr,E(_9),E(_9)];}}},_N4=function(_N5,_N6,_N7){var _N8=E(_N5),_N9=E(_N7);if(!_N9[0]){var _Na=_N9[2],_Nb=_N9[3],_Nc=_N9[4],_Nd=_N9[5];switch(_J0(_N8,_Na)){case 0:return _LK(_Na,_Nb,_N4(_N8,_N6,_Nc),_Nd);case 1:return [0,_N9[1],E(_N8),_N6,E(_Nc),E(_Nd)];default:return _Mp(_Na,_Nb,_Nc,_N4(_N8,_N6,_Nd));}}else{return [0,1,E(_N8),_N6,E(_9),E(_9)];}},_Ne=function(_Nf,_){var _Ng=_54(_FL,_JK,_Nf,_),_Nh=E(_Ng);return [0,[0,function(_Ni,_){var _Nj=A(E(_Nh[1])[1],[_Ni,_]);return _Ni;},_9L],_Nh[2]];},_Nk=[0,_3C,_9L],_Nl=function(_Nm,_){return [0,[0,_3C,[1,_Nm]],_Nm];},_Nn=function(_No){var _Np=new T(function(){return _Nq(_No);});return function(_9A,_9B){return _54(function(_F8,_){return _54(_Nl,function(_Nr){return function(_Ns,_){return [0,_Nk,new T(function(){var _Nt=E(_Nr);return [0,_Nt[1],_Nt[2],_Nt[3],_Nt[4],_Nt[5],new T(function(){return _N4(_IM,[0,_No],_Nt[6]);})];})];};},_F8,_);},function(_Nu,_F8,_){return (function(_F8,_){return _54(_Ne,function(_Nv){return E(_Np);},_F8,_);})(_F8,_);},_9A,_9B);};},_Nq=function(_Nw){return function(_9A,_9B){return _E7(new T(function(){var _Nx=new T(function(){return !_6E(_Nw,_sX)?E(_Ke):E(_Kf);}),_Ny=new T(function(){return !_6E(_Nw,_IE)?E(_Ke):E(_Kf);}),_Nz=new T(function(){return !_6E(_Nw,_a)?E(_Ke):E(_Kf);});return _84(function(_NA,_){var _NB=A(_LH,[_NA,_]),_NC=E(_NB),_ND=E(_NC[1]),_NE=A(_LE,[_NC[2],_]),_NF=E(_NE),_NG=E(_NF[1]),_NH=A(_LC,[_NF[2],_]),_NI=E(_NH),_NJ=E(_NI[1]),_NK=new T(function(){return _Go(_Gz,_NJ[1]);}),_NL=new T(function(){return _Go(_Gz,_NG[1]);}),_NM=new T(function(){return _Go(_Gz,_ND[1]);});return [0,[0,function(_NN,_){var _NO=A(_NM,[_NN,_]),_NP=A(_B,[_19,_NO,_DS,_Nz,_]),_NQ=A(_NL,[_NN,_]),_NR=A(_B,[_19,_NQ,_DS,_Ny,_]),_NS=A(_NK,[_NN,_]),_NT=A(_B,[_19,_NS,_DS,_Nx,_]);return _NN;},new T(function(){var _NU=E(_ND[2]);if(!_NU[0]){var _NV=E(_NG[2]);return _NV[0]==0?E(_NJ[2]):E(_NV);}else{return E(_NU);}})],_NI[2]];},_60);}),_Nn,_9A,_9B);};},_NW=new T(function(){return _Nq(_a);}),_NX=function(_F8,_){return _K(_1f,_H,_G0,_F8,_);},_NY=function(_NZ){return E(_NX);},_O0=function(_F8,_){return _54(_JN,_NY,_F8,_);},_O1=function(_O2){return E(_O0);},_O3=function(_O4,_O5){var _O6=E(_O5);switch(_O6[0]){case 0:return [0,_O6[1],_O6[2],E(_O3(_O4,_O6[3])),E(_O3(_O4,_O6[4]))];case 1:return [1,_O6[1],new T(function(){return A(_O4,[_O6[2]]);})];default:return [2];}},_O7=function(_O8,_O9,_Oa){var _Ob=E(_Oa);switch(_Ob[0]){case 0:var _Oc=_Ob[1],_Od=_Ob[2],_Oe=_Ob[3],_Of=_Ob[4],_Og=_O9>>>0,_Oh=_Od>>>0;if(((_Og&((_Oh-1>>>0^4.294967295e9)>>>0^_Oh)>>>0)>>>0&4.294967295e9)==_Oc){return (_Og&_Oh)>>>0!=0?[0,_Oc,_Od,E(_Oe),E(_O7(_O8,_O9,_Of))]:[0,_Oc,_Od,E(_O7(_O8,_O9,_Oe)),E(_Of)];}else{var _Oi=E(_O8);if(_Oi[0]==2){return E(_Ob);}else{var _Oj=(_Og^_Oc>>>0)>>>0,_Ok=(_Oj|_Oj>>>1)>>>0,_Ol=(_Ok|_Ok>>>2)>>>0,_Om=(_Ol|_Ol>>>4)>>>0,_On=(_Om|_Om>>>8)>>>0,_Oo=(_On|_On>>>16)>>>0,_Op=(_Oo^_Oo>>>1)>>>0&4.294967295e9,_Oq=_Op>>>0,_Or=(_Og&((_Oq-1>>>0^4.294967295e9)>>>0^_Oq)>>>0)>>>0&4.294967295e9;return (_Og&_Oq)>>>0!=0?[0,_Or,_Op,E(_Ob),E(_Oi)]:[0,_Or,_Op,E(_Oi),E(_Ob)];}}break;case 1:var _Os=_Ob[1];if(_O9!=_Os){var _Ot=E(_O8);if(_Ot[0]==2){return E(_Ob);}else{var _Ou=_O9>>>0,_Ov=(_Ou^_Os>>>0)>>>0,_Ow=(_Ov|_Ov>>>1)>>>0,_Ox=(_Ow|_Ow>>>2)>>>0,_Oy=(_Ox|_Ox>>>4)>>>0,_Oz=(_Oy|_Oy>>>8)>>>0,_OA=(_Oz|_Oz>>>16)>>>0,_OB=(_OA^_OA>>>1)>>>0&4.294967295e9,_OC=_OB>>>0,_OD=(_Ou&((_OC-1>>>0^4.294967295e9)>>>0^_OC)>>>0)>>>0&4.294967295e9;return (_Ou&_OC)>>>0!=0?[0,_OD,_OB,E(_Ob),E(_Ot)]:[0,_OD,_OB,E(_Ot),E(_Ob)];}}else{return E(_O8);}break;default:return E(_O8);}},_OE=function(_OF,_OG,_OH){var _OI=E(_OH);switch(_OI[0]){case 0:var _OJ=_OI[1],_OK=_OI[2],_OL=_OI[3],_OM=_OI[4],_ON=_OG>>>0,_OO=_OK>>>0;if(((_ON&((_OO-1>>>0^4.294967295e9)>>>0^_OO)>>>0)>>>0&4.294967295e9)==_OJ){return (_ON&_OO)>>>0!=0?[0,_OJ,_OK,E(_OL),E(_OE(_OF,_OG,_OM))]:[0,_OJ,_OK,E(_OE(_OF,_OG,_OL)),E(_OM)];}else{var _OP=E(_OF);if(_OP[0]==2){return E(_OI);}else{var _OQ=_OJ>>>0,_OR=(_OQ^_ON)>>>0,_OS=(_OR|_OR>>>1)>>>0,_OT=(_OS|_OS>>>2)>>>0,_OU=(_OT|_OT>>>4)>>>0,_OV=(_OU|_OU>>>8)>>>0,_OW=(_OV|_OV>>>16)>>>0,_OX=(_OW^_OW>>>1)>>>0&4.294967295e9,_OY=_OX>>>0,_OZ=(_OQ&((_OY-1>>>0^4.294967295e9)>>>0^_OY)>>>0)>>>0&4.294967295e9;return (_OQ&_OY)>>>0!=0?[0,_OZ,_OX,E(_OP),E(_OI)]:[0,_OZ,_OX,E(_OI),E(_OP)];}}break;case 1:var _P0=_OI[1];if(_P0!=_OG){var _P1=E(_OF);if(_P1[0]==2){return E(_OI);}else{var _P2=_P0>>>0,_P3=(_P2^_OG>>>0)>>>0,_P4=(_P3|_P3>>>1)>>>0,_P5=(_P4|_P4>>>2)>>>0,_P6=(_P5|_P5>>>4)>>>0,_P7=(_P6|_P6>>>8)>>>0,_P8=(_P7|_P7>>>16)>>>0,_P9=(_P8^_P8>>>1)>>>0&4.294967295e9,_Pa=_P9>>>0,_Pb=(_P2&((_Pa-1>>>0^4.294967295e9)>>>0^_Pa)>>>0)>>>0&4.294967295e9;return (_P2&_Pa)>>>0!=0?[0,_Pb,_P9,E(_P1),E(_OI)]:[0,_Pb,_P9,E(_OI),E(_P1)];}}else{return E(_OI);}break;default:return E(_OF);}},_Pc=function(_Pd,_Pe,_Pf,_Pg,_Ph){var _Pi=E(_Ph);switch(_Pi[0]){case 0:var _Pj=_Pi[1],_Pk=_Pi[2],_Pl=_Pi[3],_Pm=_Pi[4],_Pn=_Pe>>>0,_Po=_Pk>>>0;if(_Pn<=_Po){if(_Po<=_Pn){if(_Pd!=_Pj){var _Pp=_Pd>>>0,_Pq=(_Pp^_Pj>>>0)>>>0,_Pr=(_Pq|_Pq>>>1)>>>0,_Ps=(_Pr|_Pr>>>2)>>>0,_Pt=(_Ps|_Ps>>>4)>>>0,_Pu=(_Pt|_Pt>>>8)>>>0,_Pv=(_Pu|_Pu>>>16)>>>0,_Pw=(_Pv^_Pv>>>1)>>>0&4.294967295e9,_Px=_Pw>>>0,_Py=(_Pp&((_Px-1>>>0^4.294967295e9)>>>0^_Px)>>>0)>>>0&4.294967295e9;return (_Pp&_Px)>>>0!=0?[0,_Py,_Pw,E(_Pi),E([0,_Pd,_Pe,E(_Pf),E(_Pg)])]:[0,_Py,_Pw,E([0,_Pd,_Pe,E(_Pf),E(_Pg)]),E(_Pi)];}else{return [0,_Pd,_Pe,E(_Pz(_Pf,_Pl)),E(_Pz(_Pg,_Pm))];}}else{var _PA=_Pd>>>0;if(((_PA&((_Po-1>>>0^4.294967295e9)>>>0^_Po)>>>0)>>>0&4.294967295e9)==_Pj){return (_PA&_Po)>>>0!=0?[0,_Pj,_Pk,E(_Pl),E(_Pc(_Pd,_Pe,_Pf,_Pg,_Pm))]:[0,_Pj,_Pk,E(_Pc(_Pd,_Pe,_Pf,_Pg,_Pl)),E(_Pm)];}else{var _PB=(_PA^_Pj>>>0)>>>0,_PC=(_PB|_PB>>>1)>>>0,_PD=(_PC|_PC>>>2)>>>0,_PE=(_PD|_PD>>>4)>>>0,_PF=(_PE|_PE>>>8)>>>0,_PG=(_PF|_PF>>>16)>>>0,_PH=(_PG^_PG>>>1)>>>0&4.294967295e9,_PI=_PH>>>0,_PJ=(_PA&((_PI-1>>>0^4.294967295e9)>>>0^_PI)>>>0)>>>0&4.294967295e9;return (_PA&_PI)>>>0!=0?[0,_PJ,_PH,E(_Pi),E([0,_Pd,_Pe,E(_Pf),E(_Pg)])]:[0,_PJ,_PH,E([0,_Pd,_Pe,E(_Pf),E(_Pg)]),E(_Pi)];}}}else{var _PK=_Pj>>>0;if(((_PK&((_Pn-1>>>0^4.294967295e9)>>>0^_Pn)>>>0)>>>0&4.294967295e9)==_Pd){return (_PK&_Pn)>>>0!=0?[0,_Pd,_Pe,E(_Pf),E(_PL(_Pg,_Pj,_Pk,_Pl,_Pm))]:[0,_Pd,_Pe,E(_PL(_Pf,_Pj,_Pk,_Pl,_Pm)),E(_Pg)];}else{var _PM=_Pd>>>0,_PN=(_PM^_PK)>>>0,_PO=(_PN|_PN>>>1)>>>0,_PP=(_PO|_PO>>>2)>>>0,_PQ=(_PP|_PP>>>4)>>>0,_PR=(_PQ|_PQ>>>8)>>>0,_PS=(_PR|_PR>>>16)>>>0,_PT=(_PS^_PS>>>1)>>>0&4.294967295e9,_PU=_PT>>>0,_PV=(_PM&((_PU-1>>>0^4.294967295e9)>>>0^_PU)>>>0)>>>0&4.294967295e9;return (_PM&_PU)>>>0!=0?[0,_PV,_PT,E(_Pi),E([0,_Pd,_Pe,E(_Pf),E(_Pg)])]:[0,_PV,_PT,E([0,_Pd,_Pe,E(_Pf),E(_Pg)]),E(_Pi)];}}break;case 1:return _OE(_Pi,_Pi[1],[0,_Pd,_Pe,E(_Pf),E(_Pg)]);default:return [0,_Pd,_Pe,E(_Pf),E(_Pg)];}},_PL=function(_PW,_PX,_PY,_PZ,_Q0){var _Q1=E(_PW);switch(_Q1[0]){case 0:var _Q2=_Q1[1],_Q3=_Q1[2],_Q4=_Q1[3],_Q5=_Q1[4],_Q6=_Q3>>>0,_Q7=_PY>>>0;if(_Q6<=_Q7){if(_Q7<=_Q6){if(_Q2!=_PX){var _Q8=_Q2>>>0,_Q9=(_Q8^_PX>>>0)>>>0,_Qa=(_Q9|_Q9>>>1)>>>0,_Qb=(_Qa|_Qa>>>2)>>>0,_Qc=(_Qb|_Qb>>>4)>>>0,_Qd=(_Qc|_Qc>>>8)>>>0,_Qe=(_Qd|_Qd>>>16)>>>0,_Qf=(_Qe^_Qe>>>1)>>>0&4.294967295e9,_Qg=_Qf>>>0,_Qh=(_Q8&((_Qg-1>>>0^4.294967295e9)>>>0^_Qg)>>>0)>>>0&4.294967295e9;return (_Q8&_Qg)>>>0!=0?[0,_Qh,_Qf,E([0,_PX,_PY,E(_PZ),E(_Q0)]),E(_Q1)]:[0,_Qh,_Qf,E(_Q1),E([0,_PX,_PY,E(_PZ),E(_Q0)])];}else{return [0,_Q2,_Q3,E(_Pz(_Q4,_PZ)),E(_Pz(_Q5,_Q0))];}}else{var _Qi=_Q2>>>0;if(((_Qi&((_Q7-1>>>0^4.294967295e9)>>>0^_Q7)>>>0)>>>0&4.294967295e9)==_PX){return (_Qi&_Q7)>>>0!=0?[0,_PX,_PY,E(_PZ),E(_Pc(_Q2,_Q3,_Q4,_Q5,_Q0))]:[0,_PX,_PY,E(_Pc(_Q2,_Q3,_Q4,_Q5,_PZ)),E(_Q0)];}else{var _Qj=(_Qi^_PX>>>0)>>>0,_Qk=(_Qj|_Qj>>>1)>>>0,_Ql=(_Qk|_Qk>>>2)>>>0,_Qm=(_Ql|_Ql>>>4)>>>0,_Qn=(_Qm|_Qm>>>8)>>>0,_Qo=(_Qn|_Qn>>>16)>>>0,_Qp=(_Qo^_Qo>>>1)>>>0&4.294967295e9,_Qq=_Qp>>>0,_Qr=(_Qi&((_Qq-1>>>0^4.294967295e9)>>>0^_Qq)>>>0)>>>0&4.294967295e9;return (_Qi&_Qq)>>>0!=0?[0,_Qr,_Qp,E([0,_PX,_PY,E(_PZ),E(_Q0)]),E(_Q1)]:[0,_Qr,_Qp,E(_Q1),E([0,_PX,_PY,E(_PZ),E(_Q0)])];}}}else{var _Qs=_PX>>>0;if(((_Qs&((_Q6-1>>>0^4.294967295e9)>>>0^_Q6)>>>0)>>>0&4.294967295e9)==_Q2){return (_Qs&_Q6)>>>0!=0?[0,_Q2,_Q3,E(_Q4),E(_PL(_Q5,_PX,_PY,_PZ,_Q0))]:[0,_Q2,_Q3,E(_PL(_Q4,_PX,_PY,_PZ,_Q0)),E(_Q5)];}else{var _Qt=_Q2>>>0,_Qu=(_Qt^_Qs)>>>0,_Qv=(_Qu|_Qu>>>1)>>>0,_Qw=(_Qv|_Qv>>>2)>>>0,_Qx=(_Qw|_Qw>>>4)>>>0,_Qy=(_Qx|_Qx>>>8)>>>0,_Qz=(_Qy|_Qy>>>16)>>>0,_QA=(_Qz^_Qz>>>1)>>>0&4.294967295e9,_QB=_QA>>>0,_QC=(_Qt&((_QB-1>>>0^4.294967295e9)>>>0^_QB)>>>0)>>>0&4.294967295e9;return (_Qt&_QB)>>>0!=0?[0,_QC,_QA,E([0,_PX,_PY,E(_PZ),E(_Q0)]),E(_Q1)]:[0,_QC,_QA,E(_Q1),E([0,_PX,_PY,E(_PZ),E(_Q0)])];}}break;case 1:return _O7(_Q1,_Q1[1],[0,_PX,_PY,E(_PZ),E(_Q0)]);default:return [0,_PX,_PY,E(_PZ),E(_Q0)];}},_Pz=function(_QD,_QE){var _QF=E(_QD);switch(_QF[0]){case 0:var _QG=_QF[1],_QH=_QF[2],_QI=_QF[3],_QJ=_QF[4],_QK=E(_QE);switch(_QK[0]){case 0:var _QL=_QK[1],_QM=_QK[2],_QN=_QK[3],_QO=_QK[4],_QP=_QH>>>0,_QQ=_QM>>>0;if(_QP<=_QQ){if(_QQ<=_QP){if(_QG!=_QL){var _QR=_QG>>>0,_QS=(_QR^_QL>>>0)>>>0,_QT=(_QS|_QS>>>1)>>>0,_QU=(_QT|_QT>>>2)>>>0,_QV=(_QU|_QU>>>4)>>>0,_QW=(_QV|_QV>>>8)>>>0,_QX=(_QW|_QW>>>16)>>>0,_QY=(_QX^_QX>>>1)>>>0&4.294967295e9,_QZ=_QY>>>0,_R0=(_QR&((_QZ-1>>>0^4.294967295e9)>>>0^_QZ)>>>0)>>>0&4.294967295e9;return (_QR&_QZ)>>>0!=0?[0,_R0,_QY,E(_QK),E(_QF)]:[0,_R0,_QY,E(_QF),E(_QK)];}else{return [0,_QG,_QH,E(_Pz(_QI,_QN)),E(_Pz(_QJ,_QO))];}}else{var _R1=_QG>>>0;if(((_R1&((_QQ-1>>>0^4.294967295e9)>>>0^_QQ)>>>0)>>>0&4.294967295e9)==_QL){return (_R1&_QQ)>>>0!=0?[0,_QL,_QM,E(_QN),E(_Pc(_QG,_QH,_QI,_QJ,_QO))]:[0,_QL,_QM,E(_Pc(_QG,_QH,_QI,_QJ,_QN)),E(_QO)];}else{var _R2=(_R1^_QL>>>0)>>>0,_R3=(_R2|_R2>>>1)>>>0,_R4=(_R3|_R3>>>2)>>>0,_R5=(_R4|_R4>>>4)>>>0,_R6=(_R5|_R5>>>8)>>>0,_R7=(_R6|_R6>>>16)>>>0,_R8=(_R7^_R7>>>1)>>>0&4.294967295e9,_R9=_R8>>>0,_Ra=(_R1&((_R9-1>>>0^4.294967295e9)>>>0^_R9)>>>0)>>>0&4.294967295e9;return (_R1&_R9)>>>0!=0?[0,_Ra,_R8,E(_QK),E(_QF)]:[0,_Ra,_R8,E(_QF),E(_QK)];}}}else{var _Rb=_QL>>>0;if(((_Rb&((_QP-1>>>0^4.294967295e9)>>>0^_QP)>>>0)>>>0&4.294967295e9)==_QG){return (_Rb&_QP)>>>0!=0?[0,_QG,_QH,E(_QI),E(_PL(_QJ,_QL,_QM,_QN,_QO))]:[0,_QG,_QH,E(_PL(_QI,_QL,_QM,_QN,_QO)),E(_QJ)];}else{var _Rc=_QG>>>0,_Rd=(_Rc^_Rb)>>>0,_Re=(_Rd|_Rd>>>1)>>>0,_Rf=(_Re|_Re>>>2)>>>0,_Rg=(_Rf|_Rf>>>4)>>>0,_Rh=(_Rg|_Rg>>>8)>>>0,_Ri=(_Rh|_Rh>>>16)>>>0,_Rj=(_Ri^_Ri>>>1)>>>0&4.294967295e9,_Rk=_Rj>>>0,_Rl=(_Rc&((_Rk-1>>>0^4.294967295e9)>>>0^_Rk)>>>0)>>>0&4.294967295e9;return (_Rc&_Rk)>>>0!=0?[0,_Rl,_Rj,E(_QK),E(_QF)]:[0,_Rl,_Rj,E(_QF),E(_QK)];}}break;case 1:return _OE(_QK,_QK[1],_QF);default:return E(_QF);}break;case 1:return _O7(_QF,_QF[1],_QE);default:return E(_QE);}},_Rm=function(_Rn,_Ro,_){var _Rp=new T(function(){return E(_Rn)[0]==0?1:0;});return _54(_FL,function(_Rq){var _Rr=E(_Rq)[1];return function(_9A,_9B){return _54(function(_F8,_){return _Jz(_Rr,_F8,_);},function(_Rs){var _Rt=new T(function(){return _DG(_w4,_w5,function(_){var _Ru=_5S(_sh,_qX,_),_Rv=_sj(_Ru,_);return A(_JW,[[0,new T(function(){return _s7(_Pz(_O3(function(_Rw){return [0,E(_Rw)[1],_Rp];},_Rs),_Rr));}),E(_Rv)[2]],_]);});});return function(_9A,_9B){return _54(_Rt,_O1,_9A,_9B);};},_9A,_9B);};},_Ro,_);},_Rx=function(_Ry,_Rz,_){return _Rm(E(_Ry)[1],_Rz,_);},_RA=unCStr("toggle-all"),_RB=new T(function(){return A(_Hs,[_0,_GK]);}),_RC=new T(function(){return _84(_RB,_60);}),_RD=function(_RE,_){var _RF=A(_RC,[_RE,_]),_RG=E(_RF),_RH=E(_RG[1]);return [0,[0,function(_RI,_){var _RJ=A(_RH[1],[_RI,_]),_RK=A(_B,[_19,_RJ,_DS,_RA,_]);return _RJ;},_RH[2]],_RG[2]];},_RL=function(_F8,_){return _54(_RD,_Rx,_F8,_);},_RM=unCStr("h1"),_RN=function(_RO,_RP){var _RQ=new T(function(){return A(_RO,[_RP]);});return function(_RR,_){var _RS=jsCreateElem(toJSStr(E(_RM))),_RT=jsAppendChild(_RS,E(_RR)[1]),_RU=[0,_RS],_RV=A(_RQ,[_RU,_]);return _RU;};},_RW=unCStr("todos"),_RX=new T(function(){return _RN(_62,_RW);}),_RY=[0,_3C,_9L],_RZ=function(_S0,_){return [0,_RY,_S0];},_S1=[0,_3C,_9L],_S2=function(_S3,_){return [0,_S1,_S3];},_S4=[0,_3C,_9L],_S5=function(_S6,_){return [0,_S4,_S6];},_S7=[0,_3C,_9L],_S8=function(_S9,_){return [0,_S7,_S9];},_Sa=function(_Sb,_Sc,_Sd,_Se){return A(_Sb,[new T(function(){return function(_){var _Sf=jsSet(E(_Sc)[1],toJSStr(E(_Sd)),toJSStr(E(_Se)));return _A;};})]);},_Sg=unCStr("text"),_Sh=unCStr("value"),_Si=new T(function(){return _u7(_wN,_wS);}),_Sj=new T(function(){return A(_Si,[_u6]);}),_Sk=new T(function(){return A(_Si,[_u6]);}),_Sl=function(_Sm,_Sn){var _So=_po(_cB(A(E(_Sm)[3],[_oW,_pg]),_Sn));return _So[0]==0?err(_bx):E(_So[2])[0]==0?E(_So[1]):err(_bv);},_Sp=function(_Sq,_Sr,_Ss,_St){var _Su=new T(function(){return _z8(_Sr);}),_Sv=new T(function(){return _zM(_w4,_w5,_Ss,_Sr,_Sq);});return [0,function(_Sw){return A(_Sv,[[1,_St],_Sg,_Sw]);},function(_Sx,_){var _Sy=E(_St),_Sz=jsFind(toJSStr(_Sy)),_SA=E(_Sz);return _SA[0]==0?_4D(_Sy):A(_Sa,[_19,_SA[1],_Sh,new T(function(){var _SB=A(_Ss,[_Sx]),_SC=E(_Sj),_SD=hs_eqWord64(_SB[1],_SC[1]);if(!E(_SD)){return A(_Su,[_Sx]);}else{var _SE=hs_eqWord64(_SB[2],_SC[2]);return E(_SE)==0?A(_Su,[_Sx]):E(_Sx);}}),_]);},function(_){var _SF=E(_St),_SG=jsFind(toJSStr(_SF)),_SH=E(_SG);if(!_SH[0]){return _4D(_SF);}else{var _SI=_GL(E(_SH[1])[1],_Sh,_);return new T(function(){var _SJ=A(_Si,[_SI]),_SK=E(_Sk),_SL=hs_eqWord64(_SJ[1],_SK[1]);if(!E(_SL)){return _Sl(_Sq,_SI);}else{var _SM=hs_eqWord64(_SJ[2],_SK[2]);return E(_SM)==0?_Sl(_Sq,_SI):E(_SI);}});}}];},_SN=unCStr("new-todo"),_SO=new T(function(){return _B6(_AS);}),_SP=new T(function(){var _SQ=_Sp(_SO,_Ki,_Kh,_SN);return [0,_SQ[1],_SQ[2],_SQ[3]];}),_SR=new T(function(){var _SS=A(E(_SP)[2],[_a]);return function(_ST,_){var _SU=A(_SS,[_]);return [0,[0,_3C,[1,_SU]],_ST];};}),_SV=function(_SW,_){return A(_SR,[new T(function(){var _SX=E(_SW);return [0,_SX[1],_SX[2],_SX[3],_SX[4],_5z,_SX[6]];}),_]);},_SY=function(_SZ,_T0){while(1){var _T1=E(_SZ);if(!_T1[0]){return E(_T0)[0]==0?true:false;}else{var _T2=E(_T0);if(!_T2[0]){return false;}else{if(E(_T1[1])[1]!=E(_T2[1])[1]){return false;}else{_SZ=_T1[2];_T0=_T2[2];continue;}}}}},_T3=[1,_IF],_T4=function(_T5,_){var _T6=A(_Ju,[_T5,_]);return [0,[0,_IH,new T(function(){var _T7=E(E(_T6)[1]);return _T7[0]==0?E(_T3):E(_T7);})],new T(function(){return E(E(_T6)[2]);})];},_T8=function(_T9,_Ta){return _a4(E(_T9)[1],_Ta);},_Tb=[0,_3C,_9L],_Tc=function(_Td,_){return [0,_Tb,_Td];},_Te=function(_Tf,_Tg,_){return _54(_T4,function(_Th){return !_SY(E(_Th)[1],_sX)?function(_F8,_){return _K(_9M,_I,function(_F8,_){return _E7(function(_Ti,_){var _Tj=_54(function(_Tk,_){var _Tl=_5S(_sh,_qX,_),_Tm=_sj(_Tl,_);return [0,[0,_3C,[1,new T(function(){return _T8(_Tf,E(_Tm)[1]);})]],new T(function(){var _Tn=E(_Tk);return [0,_Tn[1],_Tn[2],_Tn[3],_Tn[4],_5z,_Tn[6]];})];},function(_To){var _Tp=E(_To);if(!_Tp[0]){return E(_GJ);}else{var _Tq=E(_Tp[1]),_Tr=_Tq[1],_Ts=new T(function(){return _G1(_Tf,_ab,_Tr);}),_Tt=new T(function(){return _G1(_Tf,_ac,_Tr);}),_Tu=new T(function(){return _84(new T(function(){return A(_Hs,[new T(function(){return E(_Tq[2])==0?true:false;}),_GH]);}),_60);});return function(_9A,_9B){return _54(function(_Tv,_){var _Tw=A(_Tu,[_Tv,_]),_Tx=E(_Tw),_Ty=E(_Tx[1]);return [0,[0,function(_Tz,_){var _TA=A(_Ty[1],[_Tz,_]),_TB=A(_B,[_19,_TA,_DS,_GK,_]);return _TA;},_Ty[2]],_Tx[2]];},function(_TC){var _TD=E(E(_TC)[1]);return _TD[0]==0?E(_Ts):!_6E(_TD[1],_GH)?E(_Ts):E(_TD[2])[0]==0?E(_Tt):E(_Ts);},_9A,_9B);};}},_Ti,_),_TE=E(_Tj),_TF=A(_GG,[_TE[2],_]),_TG=E(_TF),_TH=E(_TG[1]);return [0,[0,new T(function(){return _Go(_Gz,function(_TI,_){var _TJ=A(E(_TE[1])[1],[_TI,_]),_TK=A(_TH[1],[_TI,_]);return _TI;});}),_TH[2]],_TG[2]];},function(_TL,_TM,_){var _TN=_5S(_sh,_qX,_),_TO=_sj(_TN,_),_TP=_5S(_sh,_qX,_),_TQ=_sj(_TP,_),_TR=A(_Gx,[[0,new T(function(){return _s7(_9Q(E(_Tf)[1],E(_TO)[1]));}),E(_TQ)[2]],_]);return [0,[0,_3C,[1,_TR]],new T(function(){var _TS=E(_TM);return [0,_TS[1],_TS[2],_TS[3],_TS[4],_5z,_TS[6]];})];},_F8,_);},_F8,_);}:E(_Tc);},_Tg,_);},_TT=new T(function(){return _rH(_sh,_qX);}),_TU=function(_TV,_TW,_){return _54(_sE,function(_TX){var _TY=E(E(_TX)[2]);switch(_TY[0]){case 0:return E(_S8);case 1:return E(_S5);case 2:return E(_S2);default:return E(E(_TY[1])[1])==13?function(_F8,_){return _54(_SV,function(_TZ){return function(_F8,_){return _54(function(_U0,_){var _U1=_5S(_sh,_qX,_),_U2=_sj(_U1,_),_U3=E(_U2),_U4=_U3[2],_U5=A(_TT,[[0,new T(function(){return _s7(_ad(E(_U4)[1],[0,_TV,_ab],_U3[1]));}),new T(function(){return [0,E(_U4)[1]+1|0];})],_]);return [0,[0,_3C,[1,_U4]],new T(function(){var _U6=E(_U0);return [0,_U6[1],_U6[2],_U6[3],_U6[4],_5z,_U6[6]];})];},_Te,_F8,_);};},_F8,_);}:E(_RZ);}},_TW,_);},_U7=unCStr("autofocus"),_U8=unCStr("What needs to be done?"),_U9=unCStr("placeholder"),_Ua=new T(function(){return A(E(_SP)[1],[_b]);}),_Ub=new T(function(){return _84(_Ua,_sC);}),_Uc=function(_Ud,_){var _Ue=A(_Ub,[_Ud,_]),_Uf=E(_Ue),_Ug=E(_Uf[1]);return [0,[0,function(_Uh,_){var _Ui=A(_Ug[1],[_Uh,_]),_Uj=A(_B,[_19,_Ui,_U9,_U8,_]),_Uk=A(_B,[_19,_Ui,_U7,_a,_]);return _Ui;},_Ug[2]],_Uf[2]];},_Ul=function(_Um,_){var _Un=_54(_Uc,_TU,_Um,_),_Uo=E(_Un),_Up=E(_Uo[1]);return [0,[0,function(_Uq,_){var _Ur=A(_RX,[_Uq,_]),_Us=A(_Up[1],[_Uq,_]);return _Uq;},_Up[2]],_Uo[2]];},_Ut=unCStr("footer"),_Uu=function(_Uv,_Uw){var _Ux=new T(function(){return A(_Uv,[_Uw]);});return function(_Uy,_){var _Uz=jsCreateElem(toJSStr(E(_Ut))),_UA=jsAppendChild(_Uz,E(_Uy)[1]),_UB=[0,_Uz],_UC=A(_Ux,[_UB,_]);return _UB;};},_UD=unCStr("Double-click to edit a todo"),_UE=[0,112],_UF=[1,_UE,_a],_UG=function(_UH,_UI){var _UJ=new T(function(){return A(_UH,[_UI]);});return function(_UK,_){var _UL=jsCreateElem(toJSStr(_UF)),_UM=jsAppendChild(_UL,E(_UK)[1]),_UN=[0,_UL],_UO=A(_UJ,[_UN,_]);return _UN;};},_UP=new T(function(){return _UG(_62,_UD);}),_UQ=unCStr("http://twitter.com/agocorona"),_UR=unCStr("Created by "),_US=unCStr("Alberto G. Corona"),_UT=new T(function(){return _Kw(_62,_US);}),_UU=unCStr("http://todomvc.com"),_UV=unCStr("Part of "),_UW=unCStr("TodoMVC"),_UX=new T(function(){return _Kw(_62,_UW);}),_UY=function(_UZ,_){var _V0=_62(_UV,_UZ,_),_V1=A(_UX,[_UZ,_]),_V2=A(_B,[_19,_V1,_KF,_UU,_]);return _UZ;},_V3=new T(function(){return _UG(_Gz,_UY);}),_V4=function(_V5,_){var _V6=_62(_UR,_V5,_),_V7=A(_UT,[_V5,_]),_V8=A(_B,[_19,_V7,_KF,_UQ,_]),_V9=A(_V3,[_V5,_]);return _V5;},_Va=new T(function(){return _UG(_Gz,_V4);}),_Vb=function(_Vc,_){var _Vd=A(_UP,[_Vc,_]),_Ve=A(_Va,[_Vc,_]);return _Vc;},_Vf=new T(function(){return _Uu(_Gz,_Vb);}),_Vg=unCStr("footer"),_Vh=unCStr("ul"),_Vi=function(_Vj,_Vk){var _Vl=new T(function(){return A(_Vj,[_Vk]);});return function(_Vm,_){var _Vn=jsCreateElem(toJSStr(E(_Vh))),_Vo=jsAppendChild(_Vn,E(_Vm)[1]),_Vp=[0,_Vn],_Vq=A(_Vl,[_Vp,_]);return _Vp;};},_Vr=new T(function(){return _Vi(_Gz,_3C);}),_Vs=unCStr("span"),_Vt=function(_Vu,_Vv){var _Vw=new T(function(){return A(_Vu,[_Vv]);});return function(_Vx,_){var _Vy=jsCreateElem(toJSStr(E(_Vs))),_Vz=jsAppendChild(_Vy,E(_Vx)[1]),_VA=[0,_Vy],_VB=A(_Vw,[_VA,_]);return _VA;};},_VC=new T(function(){return _Vt(_Gz,_3C);}),_VD=function(_VE,_){var _VF=A(_VC,[_VE,_]),_VG=A(_B,[_19,_VF,_1b,_1f,_]),_VH=A(_Vr,[_VE,_]),_VI=A(_B,[_19,_VH,_1b,_1e,_]),_VJ=A(_VC,[_VE,_]),_VK=A(_B,[_19,_VJ,_1b,_1d,_]);return _VE;},_VL=new T(function(){return _Uu(_Gz,_VD);}),_VM=function(_VN,_){var _VO=A(_Vr,[_VN,_]),_VP=A(_B,[_19,_VO,_1b,_9M,_]);return _VO;},_VQ=unCStr("section"),_VR=function(_VS,_VT){var _VU=new T(function(){return A(_VS,[_VT]);});return function(_VV,_){var _VW=jsCreateElem(toJSStr(E(_VQ))),_VX=jsAppendChild(_VW,E(_VV)[1]),_VY=[0,_VW],_VZ=A(_VU,[_VY,_]);return _VY;};},_W0=new T(function(){return _VR(_Gz,_VM);}),_W1=function(_W2,_){var _W3=_1x(_1g,_W2,_),_W4=A(_B,[_19,_W3,_1b,_1g,_]),_W5=A(_W0,[_W2,_]),_W6=A(_B,[_19,_W5,_1b,_z,_]),_W7=A(_VL,[_W2,_]),_W8=A(_B,[_19,_W7,_1b,_Vg,_]);return _W2;},_W9=new T(function(){return _VR(_Gz,_W1);}),_Wa=function(_Wb,_){var _Wc=_K(_1g,_H,_Ul,_Wb,_),_Wd=E(_Wc),_We=_K(_z,_I,_RL,_Wd[2],_),_Wf=E(_We),_Wg=_K(_1e,_H,_NW,_Wf[2],_),_Wh=E(_Wg),_Wi=_K(_1f,_H,_G0,_Wh[2],_),_Wj=E(_Wi),_Wk=_K(_1d,_H,_Gd,_Wj[2],_),_Wl=E(_Wk),_Wm=E(_Wl[1]);return [0,[0,function(_Wn,_){var _Wo=A(_W9,[_Wn,_]),_Wp=A(_B,[_19,_Wo,_1b,_1c,_]),_Wq=A(_Vf,[_Wn,_]),_Wr=A(_B,[_19,_Wq,_1b,_Kd,_]),_Ws=A(E(_Wd[1])[1],[_Wn,_]),_Wt=A(E(_Wf[1])[1],[_Wn,_]),_Wu=A(E(_Wh[1])[1],[_Wn,_]),_Wv=A(E(_Wj[1])[1],[_Wn,_]),_Ww=A(_Wm[1],[_Wn,_]);return _Wn;},_Wm[2]],_Wl[2]];},_Wx=unCStr("todo-body"),_Wy=function(_){var _Wz=E(_Wx),_WA=jsFind(toJSStr(_Wz)),_WB=E(_WA);return _WB[0]==0?_4D(_Wz):_l(_Wa,_WB[1],_);},_WC=function(_){return _Wy(_);};
var hasteMain = function() {A(_WC, [0]);};window.onload = hasteMain;