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

var _0=false,_1=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_2=function(_3){var _4=A(_3,[_]);return E(_4);},_5=function(_6){return _2(function(_){var _=0;return eval(E(_6)[1]);});},_7=new T(function(){return _5(_1);}),_8=2,_9=[1],_a=[0],_b=[0],_c=function(_d,_){return _b;},_e=function(_){return _b;},_f=[0,_e,_c],_g=[0,0],_h=[0,_a,_g,_8,_f,_0,_9],_i=function(_){var _=0,_j=newMVar(),_=putMVar(_j,_h);return [0,_j];},_k=new T(function(){return _2(_i);}),_l=function(_m,_n,_){var _o=E(_k)[1],_p=takeMVar(_o),_q=A(_m,[_p,_]),_r=E(_q),_s=E(_r[1]),_t=_s[1],_u=_s[2],_=putMVar(_o,new T(function(){var _v=E(_r[2]);return [0,_v[1],_v[2],_v[3],_v[4],_0,_v[6]];}));if(!E(E(_p)[5])){var _w=A(_t,[_n,_]);return _u;}else{var _x=A(_7,[E(E(_n)[1]),_]),_y=A(_t,[[0,_x],_]);return _u;}},_z=unCStr("main"),_A=0,_B=function(_C,_D,_E,_F){return A(_C,[new T(function(){return function(_){var _G=jsSetAttr(E(_D)[1],toJSStr(E(_E)),toJSStr(E(_F)));return _A;};})]);},_H=2,_I=1,_J=unCStr("span"),_K=function(_L,_M,_N,_O,_){var _P=A(_N,[_O,_]),_Q=E(_P),_R=E(_Q[1]),_S=_R[1];return [0,[0,function(_T,_){var _U=jsFind(toJSStr(E(_L))),_V=E(_U);if(!_V[0]){return _T;}else{var _W=_V[1];switch(E(_M)){case 0:var _X=A(_S,[_W,_]);return _T;case 1:var _Y=E(_W),_Z=_Y[1],_10=jsGetChildren(_Z),_11=E(_10);if(!_11[0]){var _12=A(_S,[_Y,_]);return _T;}else{var _13=jsCreateElem(toJSStr(E(_J))),_14=jsAddChildBefore(_13,_Z,E(_11[1])[1]),_15=A(_S,[[0,_13],_]);return _T;}break;default:var _16=E(_W),_17=jsClearChildren(_16[1]),_18=A(_S,[_16,_]);return _T;}}},_R[2]],_Q[2]];},_19=function(_1a){return E(_1a);},_1b=unCStr("id"),_1c=unCStr("todoapp"),_1d=unCStr("clear-holder"),_1e=unCStr("filters"),_1f=unCStr("todo-count"),_1g=unCStr("header"),_1h=unCStr("id"),_1i=function(_1j,_1k,_1l,_){var _1m=E(_1k),_1n=A(_1j,[_1l,_]),_1o=A(_B,[_19,_1n,_1m[1],_1m[2],_]);return _1n;},_1p=function(_1q,_1r){while(1){var _1s=(function(_1t,_1u){var _1v=E(_1u);if(!_1v[0]){return E(_1t);}else{_1q=function(_1w,_){return _1i(_1t,_1v[1],_1w,_);};_1r=_1v[2];return null;}})(_1q,_1r);if(_1s!=null){return _1s;}}},_1x=function(_1y,_1z,_){var _1A=jsCreateElem(toJSStr(E(_1y))),_1B=jsAppendChild(_1A,E(_1z)[1]);return [0,_1A];},_1C=function(_1w,_){return _1x(_J,_1w,_);},_1D=[0,coercionToken],_1E=function(_1F,_1G,_){return [0,_A,_1F];},_1H=function(_1I,_){return [0,_1I,_1I];},_1J=function(_1K,_1L,_){var _1M=A(_1K,[_]);return A(_1L,[_]);},_1N=function(_1O,_1P,_){return _1J(_1O,_1P,_);},_1Q=function(_1R,_1S,_){var _1T=A(_1R,[_]);return A(_1S,[_1T,_]);},_1U=unCStr("base"),_1V=unCStr("GHC.IO.Exception"),_1W=unCStr("IOException"),_1X=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1U,_1V,_1W],_1Y=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1X,_a],_1Z=function(_20){return E(_1Y);},_21=function(_22){return E(E(_22)[1]);},_23=unCStr("Maybe.fromJust: Nothing"),_24=new T(function(){return err(_23);}),_25=function(_26,_27,_28){var _29=new T(function(){var _2a=A(_26,[_28]),_2b=A(_27,[new T(function(){var _2c=E(_29);return _2c[0]==0?E(_24):E(_2c[1]);})]),_2d=hs_eqWord64(_2a[1],_2b[1]);if(!E(_2d)){return [0];}else{var _2e=hs_eqWord64(_2a[2],_2b[2]);return E(_2e)==0?[0]:[1,_28];}});return E(_29);},_2f=function(_2g){var _2h=E(_2g);return _25(_21(_2h[1]),_1Z,_2h[2]);},_2i=unCStr(": "),_2j=[0,41],_2k=unCStr(" ("),_2l=function(_2m,_2n){var _2o=E(_2m);return _2o[0]==0?E(_2n):[1,_2o[1],new T(function(){return _2l(_2o[2],_2n);})];},_2p=unCStr("already exists"),_2q=unCStr("does not exist"),_2r=unCStr("protocol error"),_2s=unCStr("failed"),_2t=unCStr("invalid argument"),_2u=unCStr("inappropriate type"),_2v=unCStr("hardware fault"),_2w=unCStr("unsupported operation"),_2x=unCStr("timeout"),_2y=unCStr("resource vanished"),_2z=unCStr("interrupted"),_2A=unCStr("resource busy"),_2B=unCStr("resource exhausted"),_2C=unCStr("end of file"),_2D=unCStr("illegal operation"),_2E=unCStr("permission denied"),_2F=unCStr("user error"),_2G=unCStr("unsatisified constraints"),_2H=unCStr("system error"),_2I=function(_2J,_2K){switch(E(_2J)){case 0:return _2l(_2p,_2K);case 1:return _2l(_2q,_2K);case 2:return _2l(_2A,_2K);case 3:return _2l(_2B,_2K);case 4:return _2l(_2C,_2K);case 5:return _2l(_2D,_2K);case 6:return _2l(_2E,_2K);case 7:return _2l(_2F,_2K);case 8:return _2l(_2G,_2K);case 9:return _2l(_2H,_2K);case 10:return _2l(_2r,_2K);case 11:return _2l(_2s,_2K);case 12:return _2l(_2t,_2K);case 13:return _2l(_2u,_2K);case 14:return _2l(_2v,_2K);case 15:return _2l(_2w,_2K);case 16:return _2l(_2x,_2K);case 17:return _2l(_2y,_2K);default:return _2l(_2z,_2K);}},_2L=[0,125],_2M=unCStr("{handle: "),_2N=function(_2O,_2P,_2Q,_2R,_2S,_2T){var _2U=new T(function(){var _2V=new T(function(){return _2I(_2P,new T(function(){var _2W=E(_2R);return _2W[0]==0?E(_2T):_2l(_2k,new T(function(){return _2l(_2W,[1,_2j,_2T]);}));}));}),_2X=E(_2Q);return _2X[0]==0?E(_2V):_2l(_2X,new T(function(){return _2l(_2i,_2V);}));}),_2Y=E(_2S);if(!_2Y[0]){var _2Z=E(_2O);if(!_2Z[0]){return E(_2U);}else{var _30=E(_2Z[1]);return _30[0]==0?_2l(_2M,new T(function(){return _2l(_30[1],[1,_2L,new T(function(){return _2l(_2i,_2U);})]);})):_2l(_2M,new T(function(){return _2l(_30[1],[1,_2L,new T(function(){return _2l(_2i,_2U);})]);}));}}else{return _2l(_2Y[1],new T(function(){return _2l(_2i,_2U);}));}},_31=function(_32){var _33=E(_32);return _2N(_33[1],_33[2],_33[3],_33[4],_33[6],_a);},_34=function(_35,_36){var _37=E(_35);return _2N(_37[1],_37[2],_37[3],_37[4],_37[6],_36);},_38=[0,44],_39=[0,93],_3a=[0,91],_3b=function(_3c,_3d,_3e){var _3f=E(_3d);return _3f[0]==0?unAppCStr("[]",_3e):[1,_3a,new T(function(){return A(_3c,[_3f[1],new T(function(){var _3g=function(_3h){var _3i=E(_3h);return _3i[0]==0?E([1,_39,_3e]):[1,_38,new T(function(){return A(_3c,[_3i[1],new T(function(){return _3g(_3i[2]);})]);})];};return _3g(_3f[2]);})]);})];},_3j=function(_3k,_3l){return _3b(_34,_3k,_3l);},_3m=function(_3n,_3o,_3p){var _3q=E(_3o);return _2N(_3q[1],_3q[2],_3q[3],_3q[4],_3q[6],_3p);},_3r=[0,_3m,_31,_3j],_3s=new T(function(){return [0,_1Z,_3r,_3t,_2f];}),_3t=function(_3u){return [0,_3s,_3u];},_3v=7,_3w=function(_3x){return [0,_b,_3v,_a,_3x,_b,_b];},_3y=function(_3z,_){return die(new T(function(){return _3t(new T(function(){return _3w(_3z);}));}));},_3A=function(_3B,_){return _3y(_3B,_);},_3C=function(_3D,_){return _3D;},_3E=[0,_1Q,_1N,_3C,_3A],_3F=function(_3G){return E(E(_3G)[1]);},_3H=function(_3I,_3J,_3K,_3L){return A(_3F,[_3I,new T(function(){return A(_3J,[_3L]);}),function(_3M){return A(_3K,[new T(function(){return E(E(_3M)[1]);}),new T(function(){return E(E(_3M)[2]);})]);}]);},_3N=function(_3O,_3P,_3Q,_3R){return A(_3F,[_3O,new T(function(){return A(_3P,[_3R]);}),function(_3S){return A(_3Q,[new T(function(){return E(E(_3S)[2]);})]);}]);},_3T=function(_3U,_3V,_3W,_3X){return _3N(_3U,_3V,_3W,_3X);},_3Y=function(_3Z){return E(E(_3Z)[4]);},_40=function(_41,_42){var _43=new T(function(){return A(_3Y,[_41,_42]);});return function(_44){return E(_43);};},_45=function(_46){return E(E(_46)[3]);},_47=function(_48){var _49=new T(function(){return _45(_48);});return [0,function(_3V,_3W,_3X){return _3H(_48,_3V,_3W,_3X);},function(_3V,_3W,_3X){return _3T(_48,_3V,_3W,_3X);},function(_4a,_4b){return A(_49,[[0,_4a,_4b]]);},function(_3X){return _40(_48,_3X);}];},_4c=new T(function(){return _47(_3E);}),_4d=[0,112],_4e=function(_4f,_4g){var _4h=jsShowI(_4f);return _2l(fromJSStr(_4h),_4g);},_4i=[0,41],_4j=[0,40],_4k=function(_4l,_4m,_4n){return _4m>=0?_4e(_4m,_4n):_4l<=6?_4e(_4m,_4n):[1,_4j,new T(function(){var _4o=jsShowI(_4m);return _2l(fromJSStr(_4o),[1,_4i,_4n]);})];},_4p=function(_4q,_4r,_4s,_4t){var _4u=E(_4r);return A(_4u[1],[new T(function(){var _4v=E(_4q);return E(_4s);}),function(_4w){var _4x=new T(function(){return E(E(_4w)[2]);});return A(_4u[2],[new T(function(){return A(_4t,[new T(function(){var _4y=E(new T(function(){var _4z=E(_4q);return [0,coercionToken];})),_4A=E(_4w);return [0,_4A[1],new T(function(){return [0,E(_4x)[1]+1|0];}),_4A[3],_4A[4],_4A[5],_4A[6]];})]);}),new T(function(){return A(_4u[3],[[1,_4d,new T(function(){return _2l(_4k(0,E(_4x)[1],_a),new T(function(){return E(E(_4w)[1]);}));})]]);})]);}]);},_4B=new T(function(){return _4p(_1D,_4c,_1H,_1E);}),_4C=unCStr(" could be found!"),_4D=function(_4E){return err(unAppCStr("No element with ID ",new T(function(){return _2l(_4E,_4C);})));},_4F=function(_4G,_4H,_){var _4I=E(_4H),_4J=jsFind(toJSStr(_4I)),_4K=E(_4J);if(!_4K[0]){return _4D(_4I);}else{var _4L=E(_4K[1]),_4M=jsClearChildren(_4L[1]);return _l(_4G,_4L,_);}},_4N=function(_4O,_4P,_4Q,_4R,_4S,_4T,_4U,_4V,_4W,_){var _4X=E(_4U);return [0,_4X,[0,_4R,_4S,_4T,[0,function(_){return _4F(function(_4Y,_){var _4Z=A(_4O,[new T(function(){var _50=E(_4Y);return [0,_50[1],_4S,_50[3],_50[4],_50[5],_50[6]];}),_]);return [0,[0,_3C,E(E(_4Z)[1])[2]],_4Y];},_4Q,_);},function(_51,_){var _52=_4F(new T(function(){return A(_4P,[_51]);}),_4Q,_),_53=E(_52);return _53[0]==0?_b:A(_4X[2],[_53[1],_]);}],_4V,_4W]];},_54=function(_55,_56,_57,_){var _58=A(_4B,[_57,_]),_59=E(_58),_5a=_59[1],_5b=E(_59[2]),_5c=_4N(_55,_56,_5a,_5b[1],_5b[2],_5b[3],_5b[4],_5b[5],_5b[6],_),_5d=A(_55,[new T(function(){return E(E(_5c)[2]);}),_]),_5e=E(_5d),_5f=_5e[2],_5g=E(_5e[1]),_5h=_5g[1],_5i=new T(function(){return _1p(_1C,[1,[0,_1h,_5a],_a]);}),_5j=E(_5g[2]);if(!_5j[0]){return [0,[0,function(_5k,_){var _5l=A(_5h,[_5k,_]),_5m=A(_5i,[_5k,_]);return _5k;},_b],new T(function(){var _5n=E(_5f);return [0,_5n[1],_5n[2],_5n[3],new T(function(){return E(E(_5c)[1]);}),_5n[5],_5n[6]];})];}else{var _5o=A(_56,[_5j[1],new T(function(){var _5p=E(_5f);return [0,_5p[1],_5p[2],_5p[3],new T(function(){return E(E(_5c)[1]);}),_5p[5],_5p[6]];}),_]),_5q=E(_5o),_5r=E(_5q[1]);return [0,[0,function(_5s,_){var _5t=A(_5h,[_5s,_]),_5u=A(_5i,[_5s,_]),_5v=A(_5r[1],[_5u,_]);return _5s;},_5r[2]],_5q[2]];}},_5w=function(_5x,_5y,_5z){return A(_5x,[[1,_38,new T(function(){return A(_5y,[_5z]);})]]);},_5A=unCStr(": empty list"),_5B=unCStr("Prelude."),_5C=function(_5D){return err(_2l(_5B,new T(function(){return _2l(_5D,_5A);})));},_5E=unCStr("foldr1"),_5F=new T(function(){return _5C(_5E);}),_5G=function(_5H,_5I){var _5J=E(_5I);if(!_5J[0]){return E(_5F);}else{var _5K=_5J[1],_5L=E(_5J[2]);return _5L[0]==0?E(_5K):A(_5H,[_5K,new T(function(){return _5G(_5H,_5L);})]);}},_5M=[0,0],_5N=function(_5O){return E(E(_5O)[1]);},_5P=function(_5Q,_5R,_5S,_5T,_5U){return [1,_4j,new T(function(){return A(_5G,[_5w,[1,new T(function(){return A(_5N,[_5Q,_5M,_5S]);}),[1,new T(function(){return A(_5N,[_5R,_5M,_5T]);}),_a]],[1,_4i,_5U]]);})];},_5V=function(_5W,_5X,_5Y,_5Z){return _3b(function(_60,_61){var _62=E(_60);return _5P(_5W,_5X,_62[1],_62[2],_61);},_5Y,_5Z);},_63=function(_64){return _4k(0,E(_64)[1],_a);},_65=function(_66,_67){return _4k(0,E(_66)[1],_67);},_68=function(_69,_6a){return _3b(_65,_69,_6a);},_6b=function(_6c,_6d,_6e){return _4k(E(_6c)[1],E(_6d)[1],_6e);},_6f=[0,_6b,_63,_68],_6g=unCStr("fromList "),_6h=function(_6i,_6j){while(1){var _6k=(function(_6l,_6m){var _6n=E(_6m);switch(_6n[0]){case 0:_6i=new T(function(){return _6h(_6l,_6n[4]);});_6j=_6n[3];return null;case 1:return [1,[0,[0,_6n[1]],_6n[2]],_6l];default:return E(_6l);}})(_6i,_6j);if(_6k!=null){return _6k;}}},_6o=function(_6p){var _6q=E(_6p);if(!_6q[0]){var _6r=_6q[3],_6s=_6q[4];return _6q[2]>=0?_6h(new T(function(){return _6h(_a,_6s);}),_6r):_6h(new T(function(){return _6h(_a,_6r);}),_6s);}else{return _6h(_a,_6q);}},_6t=function(_6u,_6v,_6w){var _6x=new T(function(){return _6o(_6w);});return _6v<=10?function(_6y){return _2l(_6g,new T(function(){return _5V(_6f,_6u,_6x,_6y);}));}:function(_6z){return [1,_4j,new T(function(){return _2l(_6g,new T(function(){return _5V(_6f,_6u,_6x,[1,_4i,_6z]);}));})];};},_6A=function(_6B){var _6C=E(_6B);switch(_6C[0]){case 0:return _6A(_6C[3])+_6A(_6C[4])|0;case 1:return 1;default:return 0;}},_6D=true,_6E=function(_6F,_6G){while(1){var _6H=E(_6G);switch(_6H[0]){case 0:var _6I=_6H[3],_6J=_6E(_6F,_6H[4]);if(_6J[0]==2){_6G=_6I;continue;}else{var _6K=_6E(_6F,_6I);return _6K[0]==2?E(_6J):[0,_6H[1],_6H[2],E(_6K),E(_6J)];}break;case 1:return !A(_6F,[[0,_6H[1]],_6H[2]])?[2]:E(_6H);default:return [2];}}},_6L=[8,coercionToken],_6M=unCStr("clear-completed"),_6N=function(_6O,_6P,_){var _6Q=jsCreateTextNode(toJSStr(E(_6O))),_6R=jsAppendChild(_6Q,E(_6P)[1]);return [0,_6Q];},_6S=unCStr("button"),_6T=function(_6U,_6V){var _6W=new T(function(){return A(_6U,[_6V]);});return function(_6X,_){var _6Y=jsCreateElem(toJSStr(E(_6S))),_6Z=jsAppendChild(_6Y,E(_6X)[1]),_70=[0,_6Y],_71=A(_6W,[_70,_]);return _70;};},_72=unCStr("Clear completed"),_73=new T(function(){return _6T(_6N,_72);}),_74=function(_75,_){var _76=A(_73,[_75,_]),_77=A(_B,[_19,_76,_1b,_6M,_]);return _76;},_78=unCStr("keydown"),_79=unCStr("mousemove"),_7a=unCStr("blur"),_7b=unCStr("focus"),_7c=unCStr("change"),_7d=unCStr("unload"),_7e=unCStr("load"),_7f=unCStr("keyup"),_7g=unCStr("keypress"),_7h=unCStr("mouseup"),_7i=unCStr("mousedown"),_7j=unCStr("dblclick"),_7k=unCStr("click"),_7l=unCStr("mouseout"),_7m=unCStr("mouseover"),_7n=function(_7o){switch(E(_7o)[0]){case 0:return E(_7e);case 1:return E(_7d);case 2:return E(_7c);case 3:return E(_7b);case 4:return E(_7a);case 5:return E(_79);case 6:return E(_7m);case 7:return E(_7l);case 8:return E(_7k);case 9:return E(_7j);case 10:return E(_7i);case 11:return E(_7h);case 12:return E(_7g);case 13:return E(_7f);default:return E(_78);}},_7p=function(_7q,_7r){while(1){var _7s=E(_7q);if(!_7s[0]){return E(_7r)[0]==0?true:false;}else{var _7t=E(_7r);if(!_7t[0]){return false;}else{if(E(_7s[1])[1]!=E(_7t[1])[1]){return false;}else{_7q=_7s[2];_7r=_7t[2];continue;}}}}},_7u=[0],_7v=unCStr("Onload"),_7w=[0,_7v,_7u],_7x=unCStr("OnLoad"),_7y=[0,_7x,_7u],_7z=function(_){var _=0,_7A=newMVar(),_=putMVar(_7A,_7y);return [0,_7A];},_7B=new T(function(){return _2(_7z);}),_7C=function(_7D,_7E,_){var _7F=A(_7D,[_]);return die(_7E);},_7G=function(_7H,_7I,_7J,_){return _7C(function(_){var _=putMVar(_7H,_7I);return _A;},_7J,_);},_7K=function(_7L,_){var _7M=0;if(!E(_7M)){return (function(_){var _7N=E(_7B)[1],_7O=takeMVar(_7N),_7P=jsCatch(function(_){return (function(_){return _7L;})();},function(_1w,_){return _7G(_7N,_7O,_1w,_);}),_=putMVar(_7N,_7P);return _A;})();}else{var _7Q=E(_7B)[1],_7R=takeMVar(_7Q),_7S=jsCatch(function(_){return _7L;},function(_1w,_){return _7G(_7Q,_7R,_1w,_);}),_=putMVar(_7Q,_7S);return _A;}},_7T=function(_7U,_){var _7V=_7K(_7w,_);return [0,[0,_3C,[1,_7V]],new T(function(){var _7W=E(_7U);return [0,_7W[1],_7W[2],_7W[3],_7W[4],_6D,_7W[6]];})];},_7X=function(_){var _7Y=E(_7B)[1],_7Z=takeMVar(_7Y),_=putMVar(_7Y,_7Z);return _7Z;},_80=function(_81,_){var _82=0;if(!E(_82)){var _83=_7X();return [0,[0,_3C,[1,_83]],new T(function(){var _84=E(_81);return [0,_84[1],_84[2],_84[3],_84[4],_6D,_84[6]];})];}else{var _85=E(_7B)[1],_86=takeMVar(_85),_=putMVar(_85,_86);return [0,[0,_3C,[1,_86]],new T(function(){var _87=E(_81);return [0,_87[1],_87[2],_87[3],_87[4],_6D,_87[6]];})];}},_88=[0,_3C,_b],_89=function(_8a,_){var _8b=0;if(!E(_8b)){return (function(_){var _8c=E(_7B)[1],_8d=takeMVar(_8c),_8e=jsCatch(function(_){return (function(_){return _8a;})();},function(_1w,_){return _7G(_8c,_8d,_1w,_);}),_=putMVar(_8c,_8e);return _A;})();}else{var _8f=E(_7B)[1],_8g=takeMVar(_8f),_8h=jsCatch(function(_){return _8a;},function(_1w,_){return _7G(_8f,_8g,_1w,_);}),_=putMVar(_8f,_8h);return _A;}},_8i=unCStr("true"),_8j=new T(function(){return [0,"keydown"];}),_8k=new T(function(){return [0,"mousemove"];}),_8l=new T(function(){return [0,"blur"];}),_8m=new T(function(){return [0,"focus"];}),_8n=new T(function(){return [0,"change"];}),_8o=new T(function(){return [0,"unload"];}),_8p=new T(function(){return [0,"load"];}),_8q=new T(function(){return [0,"keyup"];}),_8r=new T(function(){return [0,"keypress"];}),_8s=new T(function(){return [0,"mouseup"];}),_8t=new T(function(){return [0,"mousedown"];}),_8u=new T(function(){return [0,"dblclick"];}),_8v=new T(function(){return [0,"click"];}),_8w=new T(function(){return [0,"mouseout"];}),_8x=new T(function(){return [0,"mouseover"];}),_8y=function(_8z){switch(E(_8z)[0]){case 0:return E(_8p);case 1:return E(_8o);case 2:return E(_8n);case 3:return E(_8m);case 4:return E(_8l);case 5:return E(_8k);case 6:return E(_8x);case 7:return E(_8w);case 8:return E(_8v);case 9:return E(_8u);case 10:return E(_8t);case 11:return E(_8s);case 12:return E(_8r);case 13:return E(_8q);default:return E(_8j);}},_8A=function(_8B,_8C,_8D){var _8E=new T(function(){return _7n(_8C);}),_8F=new T(function(){return _8y(_8C);});return function(_8G,_){var _8H=A(_8B,[_8G,_]),_8I=E(_8H),_8J=_8I[1],_8K=E(_8E),_8L=jsGetAttr(_8J,toJSStr(_8K));if(!_7p(fromJSStr(_8L),_8i)){var _8M=E(_8D),_8N=jsSetCB(_8J,E(_8F)[1],E([0,_8D])[1]),_8O=A(_B,[_19,_8I,_8K,_8i,_]);return _8I;}else{return _8I;}};},_8P=function(_8Q,_8R){var _8S=new T(function(){return _7n(_8R);}),_8T=[0,_8S,_7u];return function(_8U,_){var _8V=E(_8U),_8W=E(_8V[4]),_8X=_8W[1],_8Y=_8W[2],_8Z=A(_8Q,[_8V,_]),_90=E(_8Z),_91=E(_90[1]),_92=_91[1];return [0,[0,new T(function(){var _93=E(_8R);switch(_93[0]){case 0:return _8A(_92,_93,function(_){var _94=_89(_8T,_),_95=A(_8X,[_]),_96=E(_95);if(!_96[0]){return _A;}else{var _97=A(_8Y,[_96[1],_]);return _A;}});case 1:return _8A(_92,_93,function(_){var _98=_89(_8T,_),_99=A(_8X,[_]),_9a=E(_99);if(!_9a[0]){return _A;}else{var _9b=A(_8Y,[_9a[1],_]);return _A;}});case 2:return _8A(_92,_93,function(_){var _9c=_89(_8T,_),_9d=A(_8X,[_]),_9e=E(_9d);if(!_9e[0]){return _A;}else{var _9f=A(_8Y,[_9e[1],_]);return _A;}});case 3:return _8A(_92,_93,function(_){var _9g=_89(_8T,_),_9h=A(_8X,[_]),_9i=E(_9h);if(!_9i[0]){return _A;}else{var _9j=A(_8Y,[_9i[1],_]);return _A;}});case 4:return _8A(_92,_93,function(_){var _9k=_89(_8T,_),_9l=A(_8X,[_]),_9m=E(_9l);if(!_9m[0]){return _A;}else{var _9n=A(_8Y,[_9m[1],_]);return _A;}});case 5:return _8A(_92,_93,function(_9o,_){var _9p=_89([0,_8S,[2,E(_9o)]],_),_9q=A(_8X,[_]),_9r=E(_9q);if(!_9r[0]){return _A;}else{var _9s=A(_8Y,[_9r[1],_]);return _A;}});case 6:return _8A(_92,_93,function(_9t,_){var _9u=_89([0,_8S,[2,E(_9t)]],_),_9v=A(_8X,[_]),_9w=E(_9v);if(!_9w[0]){return _A;}else{var _9x=A(_8Y,[_9w[1],_]);return _A;}});case 7:return _8A(_92,_93,function(_){var _9y=A(_8X,[_]),_9z=E(_9y);if(!_9z[0]){return _A;}else{var _9A=A(_8Y,[_9z[1],_]);return _A;}});case 8:return _8A(_92,_93,function(_9B,_9C,_){var _9D=_89([0,_8S,[1,_9B,E(_9C)]],_),_9E=A(_8X,[_]),_9F=E(_9E);if(!_9F[0]){return _A;}else{var _9G=A(_8Y,[_9F[1],_]);return _A;}});case 9:return _8A(_92,_93,function(_9H,_9I,_){var _9J=_89([0,_8S,[1,_9H,E(_9I)]],_),_9K=A(_8X,[_]),_9L=E(_9K);if(!_9L[0]){return _A;}else{var _9M=A(_8Y,[_9L[1],_]);return _A;}});case 10:return _8A(_92,_93,function(_9N,_9O,_){var _9P=_89([0,_8S,[1,_9N,E(_9O)]],_),_9Q=A(_8X,[_]),_9R=E(_9Q);if(!_9R[0]){return _A;}else{var _9S=A(_8Y,[_9R[1],_]);return _A;}});case 11:return _8A(_92,_93,function(_9T,_9U,_){var _9V=_89([0,_8S,[1,_9T,E(_9U)]],_),_9W=A(_8X,[_]),_9X=E(_9W);if(!_9X[0]){return _A;}else{var _9Y=A(_8Y,[_9X[1],_]);return _A;}});case 12:return _8A(_92,_93,function(_9Z,_){var _a0=_89([0,_8S,[3,_9Z]],_),_a1=A(_8X,[_]),_a2=E(_a1);if(!_a2[0]){return _A;}else{var _a3=A(_8Y,[_a2[1],_]);return _A;}});case 13:return _8A(_92,_93,function(_a4,_){var _a5=_89([0,_8S,[3,_a4]],_),_a6=A(_8X,[_]),_a7=E(_a6);if(!_a7[0]){return _A;}else{var _a8=A(_8Y,[_a7[1],_]);return _A;}});default:return _8A(_92,_93,function(_a9,_){var _aa=_89([0,_8S,[3,_a9]],_),_ab=A(_8X,[_]),_ac=E(_ab);if(!_ac[0]){return _A;}else{var _ad=A(_8Y,[_ac[1],_]);return _A;}});}}),_91[2]],_90[2]];};},_ae=[1,_A],_af=function(_ag,_ah){var _ai=new T(function(){return _7n(_ah);}),_aj=new T(function(){return _8P(function(_ak,_){return [0,[0,_ag,_ae],_ak];},_ah);});return function(_al,_am){return _54(_7T,function(_an,_1w,_){return (function(_1w,_){return _54(_aj,function(_ao){return function(_1w,_){return _54(_80,function(_ap){var _aq=E(_ap);return (function(_ar,_as){var _at=new T(function(){return _7p(_ai,_ar);});return function(_au,_){return !E(_at)?[0,_88,_au]:[0,[0,_3C,[1,[0,_ar,_as]]],_au];};})(_aq[1],_aq[2]);},_1w,_);};},_1w,_);})(_1w,_);},_al,_am);};},_av=new T(function(){return _af(_74,_6L);}),_aw=[1,_A],_ax=[0,_3C,_b],_ay=function(_az,_){return [0,_ax,_az];},_aA=function(_aB,_aC){while(1){var _aD=E(_aC);switch(_aD[0]){case 0:var _aE=_aD[1],_aF=_aD[2],_aG=_aD[3],_aH=_aD[4],_aI=_aB>>>0,_aJ=_aF>>>0;if(((_aI&((_aJ-1>>>0^4.294967295e9)>>>0^_aJ)>>>0)>>>0&4.294967295e9)==_aE){if((_aI&_aJ)>>>0!=0){var _aK=_aA(_aB,_aH);if(_aK[0]==2){return E(_aG);}else{var _aL=E(_aG);return _aL[0]==2?E(_aK):[0,_aE,_aF,E(_aL),E(_aK)];}}else{var _aM=E(_aH);if(_aM[0]==2){_aC=_aG;continue;}else{var _aN=_aA(_aB,_aG);return _aN[0]==2?E(_aM):[0,_aE,_aF,E(_aN),E(_aM)];}}}else{return E(_aD);}break;case 1:return _aB!=_aD[1]?E(_aD):[2];default:return [2];}}},_aO=function(_aP,_aQ,_aR){var _aS=E(_aR);switch(_aS[0]){case 0:var _aT=_aS[1],_aU=_aS[2],_aV=_aS[3],_aW=_aS[4],_aX=_aP>>>0,_aY=_aU>>>0;if(((_aX&((_aY-1>>>0^4.294967295e9)>>>0^_aY)>>>0)>>>0&4.294967295e9)==_aT){return (_aX&_aY)>>>0!=0?[0,_aT,_aU,E(_aV),E(_aO(_aP,_aQ,_aW))]:[0,_aT,_aU,E(_aO(_aP,_aQ,_aV)),E(_aW)];}else{var _aZ=(_aX^_aT>>>0)>>>0,_b0=(_aZ|_aZ>>>1)>>>0,_b1=(_b0|_b0>>>2)>>>0,_b2=(_b1|_b1>>>4)>>>0,_b3=(_b2|_b2>>>8)>>>0,_b4=(_b3|_b3>>>16)>>>0,_b5=(_b4^_b4>>>1)>>>0&4.294967295e9,_b6=_b5>>>0,_b7=(_aX&((_b6-1>>>0^4.294967295e9)>>>0^_b6)>>>0)>>>0&4.294967295e9;return (_aX&_b6)>>>0!=0?[0,_b7,_b5,E(_aS),E([1,_aP,_aQ])]:[0,_b7,_b5,E([1,_aP,_aQ]),E(_aS)];}break;case 1:var _b8=_aS[1];if(_aP!=_b8){var _b9=_aP>>>0,_ba=(_b9^_b8>>>0)>>>0,_bb=(_ba|_ba>>>1)>>>0,_bc=(_bb|_bb>>>2)>>>0,_bd=(_bc|_bc>>>4)>>>0,_be=(_bd|_bd>>>8)>>>0,_bf=(_be|_be>>>16)>>>0,_bg=(_bf^_bf>>>1)>>>0&4.294967295e9,_bh=_bg>>>0,_bi=(_b9&((_bh-1>>>0^4.294967295e9)>>>0^_bh)>>>0)>>>0&4.294967295e9;return (_b9&_bh)>>>0!=0?[0,_bi,_bg,E(_aS),E([1,_aP,_aQ])]:[0,_bi,_bg,E([1,_aP,_aQ]),E(_aS)];}else{return [1,_aP,_aQ];}break;default:return [1,_aP,_aQ];}},_bj=1,_bk=0,_bl=[0,_3C,_aw],_bm=function(_bn,_bo,_){return E(E(_bn)[1])[0]==0?[0,_ax,_bo]:[0,_bl,_bo];},_bp=unCStr("destroy"),_bq=unCStr("class"),_br=function(_bs,_bt,_bu,_){var _bv=_1x(_bs,_bu,_),_bw=A(_bt,[_bv,_]);return _bv;},_bx=unCStr("()"),_by=unCStr("GHC.Tuple"),_bz=unCStr("ghc-prim"),_bA=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_bz,_by,_bx],_bB=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_bA,_a],_bC=function(_bD){return E(_bB);},_bE=unCStr("haste-perch-0.1.0.1"),_bF=unCStr("Haste.Perch"),_bG=unCStr("PerchM"),_bH=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_bE,_bF,_bG],_bI=[0,I_fromBits([2701112155,1279447594]),I_fromBits([4004215588,1086752342]),_bH,_a],_bJ=function(_bK){return E(_bI);},_bL=function(_bM){var _bN=E(_bM);return _bN[0]==0?[0]:_2l(_bN[1],new T(function(){return _bL(_bN[2]);}));},_bO=function(_bP,_bQ){var _bR=E(_bP);if(!_bR){return [0,_a,_bQ];}else{var _bS=E(_bQ);if(!_bS[0]){return [0,_a,_a];}else{var _bT=new T(function(){var _bU=_bO(_bR-1|0,_bS[2]);return [0,_bU[1],_bU[2]];});return [0,[1,_bS[1],new T(function(){return E(E(_bT)[1]);})],new T(function(){return E(E(_bT)[2]);})];}}},_bV=[0,120],_bW=[0,48],_bX=function(_bY){var _bZ=new T(function(){var _c0=_bO(8,new T(function(){var _c1=md5(toJSStr(E(_bY)));return fromJSStr(_c1);}));return [0,_c0[1],_c0[2]];}),_c2=parseInt([0,toJSStr([1,_bW,[1,_bV,new T(function(){return E(E(_bZ)[1]);})]])]),_c3=new T(function(){var _c4=_bO(8,new T(function(){return E(E(_bZ)[2]);}));return [0,_c4[1],_c4[2]];}),_c5=parseInt([0,toJSStr([1,_bW,[1,_bV,new T(function(){return E(E(_c3)[1]);})]])]),_c6=hs_mkWord64(_c2,_c5),_c7=parseInt([0,toJSStr([1,_bW,[1,_bV,new T(function(){return E(_bO(8,new T(function(){return E(E(_c3)[2]);}))[1]);})]])]),_c8=hs_mkWord64(_c7,_c7);return [0,_c6,_c8];},_c9=function(_ca,_cb){var _cc=E(_cb);return _cc[0]==0?[0]:[1,new T(function(){return A(_ca,[_cc[1]]);}),new T(function(){return _c9(_ca,_cc[2]);})];},_cd=function(_ce,_cf){var _cg=jsShowI(_ce),_ch=md5(_cg);return _2l(fromJSStr(_ch),new T(function(){var _ci=jsShowI(_cf),_cj=md5(_ci);return fromJSStr(_cj);}));},_ck=function(_cl){var _cm=E(_cl);return _cd(_cm[1],_cm[2]);},_cn=function(_co){var _cp=E(_co);if(!_cp[0]){return [0];}else{var _cq=E(_cp[1]);return [1,[0,_cq[1],_cq[2]],new T(function(){return _cn(_cp[2]);})];}},_cr=unCStr("Prelude.undefined"),_cs=new T(function(){return err(_cr);}),_ct=function(_cu,_cv){return function(_cw){return E(new T(function(){var _cx=A(_cu,[_cs]),_cy=E(_cx[3]),_cz=_cy[1],_cA=_cy[2],_cB=_2l(_cx[4],[1,new T(function(){return A(_cv,[_cs]);}),_a]);if(!_cB[0]){return [0,_cz,_cA,_cy,_a];}else{var _cC=_bX(new T(function(){return _bL(_c9(_ck,[1,[0,_cz,_cA],new T(function(){return _cn(_cB);})]));}));return [0,_cC[1],_cC[2],_cy,_cB];}}));};},_cD=new T(function(){return _ct(_bJ,_bC);}),_cE=unCStr("value"),_cF=unCStr("onclick"),_cG=unCStr("checked"),_cH=[0,_cG,_a],_cI=[1,_cH,_a],_cJ=unCStr("type"),_cK=unCStr("input"),_cL=function(_cM,_){return _1x(_cK,_cM,_);},_cN=function(_cO,_cP,_cQ,_cR,_cS){var _cT=new T(function(){var _cU=new T(function(){return _1p(_cL,[1,[0,_cJ,_cP],[1,[0,_1h,_cO],[1,[0,_cE,_cQ],_a]]]);});return !E(_cR)?E(_cU):_1p(_cU,_cI);}),_cV=E(_cS);return _cV[0]==0?E(_cT):_1p(_cT,[1,[0,_cF,_cV[1]],_a]);},_cW=unCStr("href"),_cX=[0,97],_cY=[1,_cX,_a],_cZ=function(_d0,_){return _1x(_cY,_d0,_);},_d1=function(_d2,_d3){var _d4=new T(function(){return _1p(_cZ,[1,[0,_cW,_d2],_a]);});return function(_d5,_){var _d6=A(_d4,[_d5,_]),_d7=A(_d3,[_d6,_]);return _d6;};},_d8=function(_d9){return _d1(_d9,function(_1w,_){return _6N(_d9,_1w,_);});},_da=unCStr("option"),_db=function(_dc,_){return _1x(_da,_dc,_);},_dd=unCStr("selected"),_de=[0,_dd,_a],_df=[1,_de,_a],_dg=function(_dh,_di,_dj){var _dk=new T(function(){return _1p(_db,[1,[0,_cE,_dh],_a]);}),_dl=function(_dm,_){var _dn=A(_dk,[_dm,_]),_do=A(_di,[_dn,_]);return _dn;};return !E(_dj)?E(_dl):_1p(_dl,_df);},_dp=function(_dq,_dr){return _dg(_dq,function(_1w,_){return _6N(_dq,_1w,_);},_dr);},_ds=unCStr("method"),_dt=unCStr("action"),_du=unCStr("UTF-8"),_dv=unCStr("acceptCharset"),_dw=[0,_dv,_du],_dx=unCStr("form"),_dy=function(_dz,_){return _1x(_dx,_dz,_);},_dA=function(_dB,_dC,_dD){var _dE=new T(function(){return _1p(_dy,[1,_dw,[1,[0,_dt,_dB],[1,[0,_ds,_dC],_a]]]);});return function(_dF,_){var _dG=A(_dE,[_dF,_]),_dH=A(_dD,[_dG,_]);return _dG;};},_dI=unCStr("select"),_dJ=function(_dK,_){return _1x(_dI,_dK,_);},_dL=function(_dM,_dN){var _dO=new T(function(){return _1p(_dJ,[1,[0,_1h,_dM],_a]);});return function(_dP,_){var _dQ=A(_dO,[_dP,_]),_dR=A(_dN,[_dQ,_]);return _dQ;};},_dS=unCStr("textarea"),_dT=function(_dU,_){return _1x(_dS,_dU,_);},_dV=function(_dW,_dX){var _dY=new T(function(){return _1p(_dT,[1,[0,_1h,_dW],_a]);});return function(_dZ,_){var _e0=A(_dY,[_dZ,_]),_e1=_6N(_dX,_e0,_);return _e0;};},_e2=unCStr("color:red"),_e3=unCStr("style"),_e4=[0,_e3,_e2],_e5=[1,_e4,_a],_e6=[0,98],_e7=[1,_e6,_a],_e8=function(_e9){return _1p(function(_ea,_){var _eb=_1x(_e7,_ea,_),_ec=A(_e9,[_eb,_]);return _eb;},_e5);},_ed=function(_ee,_ef,_){var _eg=E(_ee);if(!_eg[0]){return _ef;}else{var _eh=A(_eg[1],[_ef,_]),_ei=_ed(_eg[2],_ef,_);return _ef;}},_ej=function(_ek,_el,_em,_){var _en=A(_ek,[_em,_]),_eo=A(_el,[_em,_]);return _em;},_ep=[0,_3C,_ej,_ed],_eq=[0,_ep,_cD,_6N,_6N,_br,_e8,_d1,_d8,_cN,_dV,_dL,_dg,_dp,_dA,_1p],_er=[0,_3E,_19],_es=function(_et){return E(E(_et)[1]);},_eu=function(_ev){return E(E(_ev)[2]);},_ew=function(_ex,_ey){var _ez=new T(function(){return A(_eu,[_ex,_ey]);}),_eA=new T(function(){return _es(_ex);}),_eB=new T(function(){return _45(_eA);}),_eC=new T(function(){return _3F(_eA);});return function(_eD){return A(_eC,[_ez,function(_eE){return A(_eB,[[0,_eE,_eD]]);}]);};},_eF=1,_eG=function(_eH){return E(E(_eH)[9]);},_eI=function(_eJ,_eK){return A(_45,[_eJ,[0,_eK,_eK]]);},_eL=function(_eM,_eN,_){var _eO=jsGet(_eM,toJSStr(E(_eN)));return new T(function(){return fromJSStr(_eO);});},_eP=function(_eQ,_eR,_){return _eL(E(_eQ)[1],_eR,_);},_eS=unCStr("checkbox"),_eT=unCStr("true"),_eU=function(_eV,_eW,_eX){return A(_45,[_eV,[0,_A,_eW]]);},_eY=function(_eZ,_f0){var _f1=new T(function(){return _es(_f0);}),_f2=new T(function(){return _4p([0,coercionToken],_47(_f1),function(_f3){return _eI(_f1,_f3);},function(_f4,_f5){return _eU(_f1,_f4,_f5);});}),_f6=new T(function(){return _45(_f1);}),_f7=new T(function(){return _45(_f1);}),_f8=new T(function(){return _3F(_f1);}),_f9=new T(function(){return _3F(_f1);}),_fa=new T(function(){return _45(_f1);}),_fb=new T(function(){return _3F(_f1);}),_fc=new T(function(){return _45(_f1);}),_fd=new T(function(){return _3F(_f1);}),_fe=new T(function(){return _eG(_eZ);});return function(_ff,_fg){var _fh=new T(function(){return !E(_ff)?[0]:E(_eT);});return function(_fi){return A(_f8,[new T(function(){return A(_f2,[_fi]);}),function(_fj){var _fk=new T(function(){return E(E(_fj)[1]);}),_fl=new T(function(){return _ew(_f0,function(_){return jsFind(toJSStr(E(_fk)));});}),_fm=new T(function(){return A(_fe,[_fk,_eS,_fg,_ff,_b]);});return A(_fd,[new T(function(){var _fn=new T(function(){return E(E(_fj)[2]);});return A(_fc,[[0,_fn,_fn]]);}),function(_fo){return A(_fb,[new T(function(){return A(_fa,[[0,_A,new T(function(){var _fp=E(E(_fo)[1]);return [0,_fp[1],_fp[2],_eF,_fp[4],_fp[5],_fp[6]];})]]);}),function(_fq){return A(_f9,[new T(function(){return A(_fl,[new T(function(){return E(E(_fq)[2]);})]);}),function(_fr){return A(_f8,[new T(function(){var _fs=E(_fr),_ft=_fs[2],_fu=E(_fs[1]);return _fu[0]==0?A(_f7,[[0,_fh,_ft]]):A(_ew,[_f0,function(_){return _eP(_fu[1],_cG,_);},_ft]);}),function(_fv){return A(_f6,[[0,[0,_fm,[1,[0,new T(function(){return !_7p(E(_fv)[1],_eT)?[0]:E([1,_fg,_a]);})]]],new T(function(){return E(E(_fv)[2]);})]]);}]);}]);}]);}]);}]);};};},_fw=new T(function(){return _eY(_eq,_er);}),_fx=new T(function(){return A(_fw,[_0,_bp]);}),_fy=new T(function(){return _8P(_fx,_6L);}),_fz=function(_fA,_){var _fB=A(_fy,[_fA,_]),_fC=E(_fB),_fD=E(_fC[1]);return [0,[0,function(_fE,_){var _fF=A(_fD[1],[_fE,_]),_fG=A(_B,[_19,_fF,_bq,_bp,_]);return _fF;},_fD[2]],_fC[2]];},_fH=function(_fI){return E(E(_fI)[1]);},_fJ=function(_fK,_fL,_fM,_fN){return A(_fK,[new T(function(){return A(_fK,[new T(function(){return A(_fL,[[0,_fN,_fN]]);}),function(_fO){return A(_fL,[[0,_A,new T(function(){var _fP=E(E(_fO)[1]);return [0,_fP[1],_fP[2],_fP[3],_fP[4],_6D,_fP[6]];})]]);}]);}),function(_fQ){return A(_fM,[new T(function(){return E(E(_fQ)[2]);})]);}]);},_fR=function(_fS){return E(E(_fS)[1]);},_fT=function(_fU,_fV,_fW,_fX,_fY){var _fZ=new T(function(){return _fR(_fU);});return A(_fV,[new T(function(){return A(_fV,[_fX,function(_g0){return A(_fW,[[0,_g0,_fY]]);}]);}),function(_g1){return A(_fW,[[0,[0,_fZ,[1,new T(function(){return E(E(_g1)[1]);})]],new T(function(){return E(E(_g1)[2]);})]]);}]);},_g2=function(_g3,_g4,_g5){var _g6=new T(function(){return _es(_g4);});return function(_g7){var _g8=E(_g6);return _fJ(_g8[1],_g8[3],new T(function(){var _g9=new T(function(){return _fH(_g3);});return function(_ga){var _gb=E(_g6);return _fT(_g9,_gb[1],_gb[3],A(_eu,[_g4,_g5]),_ga);};}),_g7);};},_gc=function(_gd,_ge,_gf){var _gg=E(_gf);return _5P(_gd,_ge,_gg[1],_gg[2],_a);},_gh=function(_gi,_gj,_gk,_gl,_gm){var _gn=E(_gl);return _5P(_gi,_gj,_gn[1],_gn[2],_gm);},_go=function(_gp,_gq){return [0,function(_gr,_gs,_gt){return _gh(_gp,_gq,_gr,_gs,_gt);},function(_gt){return _gc(_gp,_gq,_gt);},function(_gs,_gt){return _5V(_gp,_gq,_gs,_gt);}];},_gu=unCStr("Completed"),_gv=unCStr("Active"),_gw=function(_gx){return E(_gx)==0?E(_gu):E(_gv);},_gy=function(_gz){return _2l(_gv,_gz);},_gA=function(_gz){return _2l(_gu,_gz);},_gB=function(_gC){return E(_gC)==0?E(_gA):E(_gy);},_gD=function(_gE,_gz){return _3b(_gB,_gE,_gz);},_gF=function(_gG,_gH){return E(_gH)==0?E(_gA):E(_gy);},_gI=[0,_gF,_gw,_gD],_gJ=unCStr("Prelude.(!!): negative index\n"),_gK=new T(function(){return err(_gJ);}),_gL=unCStr("Prelude.(!!): index too large\n"),_gM=new T(function(){return err(_gL);}),_gN=function(_gO,_gP){while(1){var _gQ=E(_gO);if(!_gQ[0]){return E(_gM);}else{var _gR=E(_gP);if(!_gR){return E(_gQ[1]);}else{_gO=_gQ[2];_gP=_gR-1|0;continue;}}}},_gS=unCStr("ACK"),_gT=unCStr("BEL"),_gU=unCStr("BS"),_gV=unCStr("SP"),_gW=[1,_gV,_a],_gX=unCStr("US"),_gY=[1,_gX,_gW],_gZ=unCStr("RS"),_h0=[1,_gZ,_gY],_h1=unCStr("GS"),_h2=[1,_h1,_h0],_h3=unCStr("FS"),_h4=[1,_h3,_h2],_h5=unCStr("ESC"),_h6=[1,_h5,_h4],_h7=unCStr("SUB"),_h8=[1,_h7,_h6],_h9=unCStr("EM"),_ha=[1,_h9,_h8],_hb=unCStr("CAN"),_hc=[1,_hb,_ha],_hd=unCStr("ETB"),_he=[1,_hd,_hc],_hf=unCStr("SYN"),_hg=[1,_hf,_he],_hh=unCStr("NAK"),_hi=[1,_hh,_hg],_hj=unCStr("DC4"),_hk=[1,_hj,_hi],_hl=unCStr("DC3"),_hm=[1,_hl,_hk],_hn=unCStr("DC2"),_ho=[1,_hn,_hm],_hp=unCStr("DC1"),_hq=[1,_hp,_ho],_hr=unCStr("DLE"),_hs=[1,_hr,_hq],_ht=unCStr("SI"),_hu=[1,_ht,_hs],_hv=unCStr("SO"),_hw=[1,_hv,_hu],_hx=unCStr("CR"),_hy=[1,_hx,_hw],_hz=unCStr("FF"),_hA=[1,_hz,_hy],_hB=unCStr("VT"),_hC=[1,_hB,_hA],_hD=unCStr("LF"),_hE=[1,_hD,_hC],_hF=unCStr("HT"),_hG=[1,_hF,_hE],_hH=[1,_gU,_hG],_hI=[1,_gT,_hH],_hJ=[1,_gS,_hI],_hK=unCStr("ENQ"),_hL=[1,_hK,_hJ],_hM=unCStr("EOT"),_hN=[1,_hM,_hL],_hO=unCStr("ETX"),_hP=[1,_hO,_hN],_hQ=unCStr("STX"),_hR=[1,_hQ,_hP],_hS=unCStr("SOH"),_hT=[1,_hS,_hR],_hU=unCStr("NUL"),_hV=[1,_hU,_hT],_hW=[0,92],_hX=unCStr("\\DEL"),_hY=unCStr("\\a"),_hZ=unCStr("\\\\"),_i0=unCStr("\\SO"),_i1=unCStr("\\r"),_i2=unCStr("\\f"),_i3=unCStr("\\v"),_i4=unCStr("\\n"),_i5=unCStr("\\t"),_i6=unCStr("\\b"),_i7=function(_i8,_i9){if(_i8<=127){var _ia=E(_i8);switch(_ia){case 92:return _2l(_hZ,_i9);case 127:return _2l(_hX,_i9);default:if(_ia<32){var _ib=E(_ia);switch(_ib){case 7:return _2l(_hY,_i9);case 8:return _2l(_i6,_i9);case 9:return _2l(_i5,_i9);case 10:return _2l(_i4,_i9);case 11:return _2l(_i3,_i9);case 12:return _2l(_i2,_i9);case 13:return _2l(_i1,_i9);case 14:return _2l(_i0,new T(function(){var _ic=E(_i9);return _ic[0]==0?[0]:E(E(_ic[1])[1])==72?unAppCStr("\\&",_ic):E(_ic);}));default:return _2l([1,_hW,new T(function(){var _id=_ib;return _id>=0?_gN(_hV,_id):E(_gK);})],_i9);}}else{return [1,[0,_ia],_i9];}}}else{return [1,_hW,new T(function(){var _ie=jsShowI(_i8);return _2l(fromJSStr(_ie),new T(function(){var _if=E(_i9);if(!_if[0]){return [0];}else{var _ig=E(_if[1])[1];return _ig<48?E(_if):_ig>57?E(_if):unAppCStr("\\&",_if);}}));})];}},_ih=[0,39],_ii=[1,_ih,_a],_ij=unCStr("\'\\\'\'"),_ik=function(_il){var _im=E(E(_il)[1]);return _im==39?E(_ij):[1,_ih,new T(function(){return _i7(_im,_ii);})];},_in=[0,34],_io=unCStr("\\\""),_ip=function(_iq,_ir){var _is=E(_iq);if(!_is[0]){return E(_ir);}else{var _it=_is[2],_iu=E(E(_is[1])[1]);return _iu==34?_2l(_io,new T(function(){return _ip(_it,_ir);})):_i7(_iu,new T(function(){return _ip(_it,_ir);}));}},_iv=function(_iw,_ix){return [1,_in,new T(function(){return _ip(_iw,[1,_in,_ix]);})];},_iy=function(_gt){return _2l(_ij,_gt);},_iz=function(_iA,_iB){var _iC=E(E(_iB)[1]);return _iC==39?E(_iy):function(_iD){return [1,_ih,new T(function(){return _i7(_iC,[1,_ih,_iD]);})];};},_iE=[0,_iz,_ik,_iv],_iF=function(_iG){return E(E(_iG)[3]);},_iH=function(_iI,_iJ){return A(_iF,[_iI,_iJ,_a]);},_iK=function(_iL,_iM,_iN){return _3b(new T(function(){return _iF(_iL);}),_iM,_iN);},_iO=function(_iP){var _iQ=new T(function(){return _iF(_iP);});return [0,function(_iR){return E(_iQ);},function(_gt){return _iH(_iP,_gt);},function(_gs,_gt){return _iK(_iP,_gs,_gt);}];},_iS=new T(function(){return _iO(_iE);}),_iT=new T(function(){return _go(_iS,_gI);}),_iU=unCStr("tasks"),_iV=function(_iW){return [0,toJSStr(E(_iW))];},_iX=function(_iY){return [1,new T(function(){return _iV(_iY);})];},_iZ=unCStr("Tried to deserialize a non-string to a Char"),_j0=[0,_iZ],_j1=unCStr("Tried to deserialize long string to a Char"),_j2=[0,_j1],_j3=function(_j4){var _j5=E(_j4);if(_j5[0]==1){var _j6=fromJSStr(E(_j5[1])[1]);return _j6[0]==0?E(_j2):E(_j6[2])[0]==0?[1,_j6[1]]:E(_j2);}else{return E(_j0);}},_j7=unCStr("Tried to deserialize a non-JSString to a JSString"),_j8=[0,_j7],_j9=function(_ja){return fromJSStr(E(_ja)[1]);},_jb=function(_jc){var _jd=E(_jc);return _jd[0]==1?[1,new T(function(){return _j9(_jd[1]);})]:E(_j8);},_je=function(_jf){return [1,new T(function(){return [0,toJSStr([1,_jf,_a])];})];},_jg=[0,_je,_iX,_j3,_jb],_jh=function(_ji){return E(E(_ji)[2]);},_jj=function(_jk,_jl){return [3,new T(function(){return _c9(new T(function(){return _jh(_jk);}),_jl);})];},_jm=[1,_a],_jn=unCStr("Tried to deserialie a non-array to a list!"),_jo=[0,_jn],_jp=function(_jq){return E(E(_jq)[4]);},_jr=function(_js,_jt){var _ju=E(_jt);if(_ju[0]==3){var _jv=new T(function(){return _jp(_js);}),_jw=function(_jx){var _jy=E(_jx);if(!_jy[0]){return E(_jm);}else{var _jz=A(_jv,[_jy[1]]);if(!_jz[0]){return [0,_jz[1]];}else{var _jA=_jw(_jy[2]);return _jA[0]==0?[0,_jA[1]]:[1,[1,_jz[1],_jA[1]]];}}};return _jw(_ju[1]);}else{return E(_jo);}},_jB=function(_jC){return [0,new T(function(){return _jh(_jC);}),function(_jD){return _jj(_jC,_jD);},new T(function(){return _jp(_jC);}),function(_jD){return _jr(_jC,_jD);}];},_jE=new T(function(){return _jB(_jg);}),_jF=new T(function(){return [0,toJSStr(_a)];}),_jG=[0,93],_jH=[1,_jG,_a],_jI=new T(function(){return [0,toJSStr(_jH)];}),_jJ=[0,125],_jK=[1,_jJ,_a],_jL=new T(function(){return [0,toJSStr(_jK)];}),_jM=[0,58],_jN=[1,_jM,_a],_jO=new T(function(){return [0,toJSStr(_jN)];}),_jP=[0,44],_jQ=[1,_jP,_a],_jR=new T(function(){return [0,toJSStr(_jQ)];}),_jS=new T(function(){return [0,"false"];}),_jT=function(_jU){var _jV=jsShow(E(_jU)[1]);return [0,_jV];},_jW=function(_jX){var _jY=jsStringify(E(_jX)[1]);return [0,_jY];},_jZ=[0,91],_k0=[1,_jZ,_a],_k1=new T(function(){return [0,toJSStr(_k0)];}),_k2=[0,123],_k3=[1,_k2,_a],_k4=new T(function(){return [0,toJSStr(_k3)];}),_k5=[0,34],_k6=[1,_k5,_a],_k7=new T(function(){return [0,toJSStr(_k6)];}),_k8=new T(function(){return [0,"true"];}),_k9=function(_ka,_kb){var _kc=E(_kb);switch(_kc[0]){case 0:return [1,new T(function(){return _jT(_kc[1]);}),_ka];case 1:return [1,new T(function(){return _jW(_kc[1]);}),_ka];case 2:return !E(_kc[1])?[1,_jS,_ka]:[1,_k8,_ka];case 3:var _kd=E(_kc[1]);return _kd[0]==0?[1,_k1,[1,_jI,_ka]]:[1,_k1,new T(function(){return _k9(new T(function(){var _ke=function(_kf){var _kg=E(_kf);return _kg[0]==0?E([1,_jI,_ka]):[1,_jR,new T(function(){return _k9(new T(function(){return _ke(_kg[2]);}),_kg[1]);})];};return _ke(_kd[2]);}),_kd[1]);})];default:var _kh=E(_kc[1]);if(!_kh[0]){return [1,_k4,[1,_jL,_ka]];}else{var _ki=E(_kh[1]);return [1,_k4,[1,new T(function(){return _jW(_ki[1]);}),[1,_jO,new T(function(){return _k9(new T(function(){var _kj=function(_kk){var _kl=E(_kk);if(!_kl[0]){return E([1,_jL,_ka]);}else{var _km=E(_kl[1]);return [1,_jR,[1,_k7,[1,_km[1],[1,_k7,[1,_jO,new T(function(){return _k9(new T(function(){return _kj(_kl[2]);}),_km[2]);})]]]]];}};return _kj(_kh[2]);}),_ki[2]);})]]];}}},_kn=new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}),_ko=function(_kp){return E(E(_kp)[1]);},_kq=function(_kr,_ks){var _kt=new T(function(){return A(_5,[_kn,E(toJSStr(E(_ks)))]);}),_ku=new T(function(){return _ko(_kr);});return function(_kv,_){var _kw=jsCat(new T(function(){return _k9(_a,A(_ku,[_kv]));}),E(_jF)[1]),_kx=A(_kt,[E(_kw),_]);return _A;};},_ky=new T(function(){return _kq(_jE,_iU);}),_kz=function(_kA){return _g2(_eq,_er,new T(function(){return A(_ky,[new T(function(){return A(_6t,[_iT,0,_kA,_a]);})]);}));},_kB=[9,coercionToken],_kC=[13,coercionToken],_kD=function(_kE,_){var _kF=0;if(!E(_kF)){var _kG=_7X();return [0,[0,_3C,[1,_kG]],new T(function(){var _kH=E(_kE);return [0,_kH[1],_kH[2],_kH[3],_kH[4],_6D,_kH[6]];})];}else{var _kI=E(_7B)[1],_kJ=takeMVar(_kI),_=putMVar(_kI,_kJ);return [0,[0,_3C,[1,_kJ]],new T(function(){var _kK=E(_kE);return [0,_kK[1],_kK[2],_kK[3],_kK[4],_6D,_kK[6]];})];}},_kL=function(_kM){return err(_kM);},_kN=unCStr("Pattern match failure in do expression at todo.hs:186:13-31"),_kO=new T(function(){return _kL(_kN);}),_kP=function(_kQ,_kR,_){return _54(_kD,function(_kS){var _kT=E(E(_kS)[2]);return _kT[0]==3?function(_kU,_){return E(E(_kT[1])[1])==13?[0,[0,_3C,[1,_kQ]],_kU]:[0,_ax,_kU];}:E(_kO);},_kR,_);},_kV=unCStr("edit"),_kW=unCStr("completed"),_kX=unCStr("text"),_kY=unCStr("label"),_kZ=function(_l0,_l1){var _l2=new T(function(){return A(_l0,[_l1]);});return function(_l3,_){var _l4=jsCreateElem(toJSStr(E(_kY))),_l5=jsAppendChild(_l4,E(_l3)[1]),_l6=[0,_l4],_l7=A(_l2,[_l6,_]);return _l6;};},_l8=function(_l9,_la){return [0,_l9,function(_lb){return _ew(_la,_lb);}];},_lc=function(_ld,_le){return [0,_ld,function(_lf){return _eI(_le,_lf);},function(_lg,_lh){return _eU(_le,_lg,_lh);}];},_li=function(_lj){return E(E(_lj)[1]);},_lk=function(_ll,_lm){return A(_ll,[function(_){return jsFind(toJSStr(E(_lm)));}]);},_ln=function(_lo){return E(E(_lo)[3]);},_lp=unCStr("GHC.Types"),_lq=unCStr("[]"),_lr=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_bz,_lp,_lq],_ls=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_lr,_a],_lt=function(_lu){return E(_ls);},_lv=unCStr("Char"),_lw=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_bz,_lp,_lv],_lx=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_lw,_a],_ly=function(_lz){return E(_lx);},_lA=new T(function(){return _ct(_lt,_ly);}),_lB=new T(function(){return A(_lA,[_cs]);}),_lC=new T(function(){return E(_cs);}),_lD=function(_lE){return E(E(_lE)[6]);},_lF=function(_lG){return E(E(_lG)[1]);},_lH=[0,32],_lI=[0,10],_lJ=function(_lK){var _lL=E(_lK);if(!_lL[0]){return E(_19);}else{var _lM=_lL[1],_lN=E(_lL[2]);if(!_lN[0]){return _lO(_lI,_lM);}else{var _lP=new T(function(){return _lJ(_lN);}),_lQ=new T(function(){return _lO(_lI,_lM);});return function(_lR){return A(_lQ,[[1,_lH,new T(function(){return A(_lP,[_lR]);})]]);};}}},_lS=unCStr("->"),_lT=[1,_lS,_a],_lU=[1,_lp,_lT],_lV=[1,_bz,_lU],_lW=[0,32],_lX=function(_lY){var _lZ=E(_lY);if(!_lZ[0]){return [0];}else{var _m0=_lZ[1],_m1=E(_lZ[2]);return _m1[0]==0?E(_m0):_2l(_m0,[1,_lW,new T(function(){return _lX(_m1);})]);}},_m2=new T(function(){return _lX(_lV);}),_m3=new T(function(){var _m4=_bX(_m2);return [0,_m4[1],_m4[2],_bz,_lp,_lS];}),_m5=function(_m6,_m7){var _m8=E(_m6);return _m8[0]==0?E(_m7):A(_m8[1],[new T(function(){return _m5(_m8[2],_m7);})]);},_m9=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_ma=[1,_bB,_a],_mb=function(_mc){var _md=E(_mc);if(!_md[0]){return [0];}else{var _me=E(_md[1]);return [1,[0,_me[1],_me[2]],new T(function(){return _mb(_md[2]);})];}},_mf=new T(function(){var _mg=_2l(_a,_ma);if(!_mg[0]){return E(_lr);}else{var _mh=_bX(new T(function(){return _bL(_c9(_ck,[1,_m9,new T(function(){return _mb(_mg);})]));}));return E(_lr);}}),_mi=[0,40],_mj=function(_mk){return _lO(_lI,_mk);},_ml=[0,8],_mm=unCStr(" -> "),_mn=[0,9],_mo=[0,93],_mp=[0,91],_mq=[0,41],_mr=[0,44],_ms=function(_mk){return [1,_mr,_mk];},_mt=function(_mu,_mv){var _mw=E(_mv);return _mw[0]==0?[0]:[1,_mu,[1,_mw[1],new T(function(){return _mt(_mu,_mw[2]);})]];},_lO=function(_mx,_my){var _mz=E(_my),_mA=_mz[3],_mB=E(_mz[4]);if(!_mB[0]){return function(_mC){return _2l(E(_mA)[5],_mC);};}else{var _mD=_mB[1],_mE=new T(function(){var _mF=E(_mA)[5],_mG=new T(function(){return _lJ(_mB);}),_mH=new T(function(){return E(_mx)[1]<=9?function(_mI){return _2l(_mF,[1,_lH,new T(function(){return A(_mG,[_mI]);})]);}:function(_mJ){return [1,_4j,new T(function(){return _2l(_mF,[1,_lH,new T(function(){return A(_mG,[[1,_4i,_mJ]]);})]);})];};}),_mK=E(_mF);if(!_mK[0]){return E(_mH);}else{if(E(E(_mK[1])[1])==40){var _mL=E(_mK[2]);return _mL[0]==0?E(_mH):E(E(_mL[1])[1])==44?function(_mM){return [1,_mi,new T(function(){return A(new T(function(){var _mN=_c9(_mj,_mB);if(!_mN[0]){return E(_19);}else{var _mO=new T(function(){return _mt(_ms,_mN[2]);});return function(_al){return _m5([1,_mN[1],_mO],_al);};}}),[[1,_mq,_mM]]);})];}:E(_mH);}else{return E(_mH);}}}),_mP=E(_mB[2]);if(!_mP[0]){var _mQ=E(_mA),_mR=E(_mf),_mS=hs_eqWord64(_mQ[1],_mR[1]);if(!E(_mS)){return E(_mE);}else{var _mT=hs_eqWord64(_mQ[2],_mR[2]);if(!E(_mT)){return E(_mE);}else{var _mU=new T(function(){return _lO(_5M,_mD);});return function(_mV){return [1,_mp,new T(function(){return A(_mU,[[1,_mo,_mV]]);})];};}}}else{if(!E(_mP[2])[0]){var _mW=E(_mA),_mX=E(_m3),_mY=hs_eqWord64(_mW[1],_mX[1]);if(!E(_mY)){return E(_mE);}else{var _mZ=hs_eqWord64(_mW[2],_mX[2]);if(!E(_mZ)){return E(_mE);}else{var _n0=new T(function(){return _lO(_ml,_mP[1]);}),_n1=new T(function(){return _lO(_mn,_mD);});return E(_mx)[1]<=8?function(_n2){return A(_n1,[new T(function(){return _2l(_mm,new T(function(){return A(_n0,[_n2]);}));})]);}:function(_n3){return [1,_4j,new T(function(){return A(_n1,[new T(function(){return _2l(_mm,new T(function(){return A(_n0,[[1,_4i,_n3]]);}));})]);})];};}}}else{return E(_mE);}}}},_n4=function(_n5,_n6,_n7,_n8){var _n9=new T(function(){return _45(_n5);}),_na=new T(function(){return _ln(_n8);}),_nb=new T(function(){return _lD(_n8);}),_nc=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_lO,[_5M,A(_n6,[_lC]),_a]);}));}),_nd=new T(function(){return A(_lF,[_n7,_g]);});return function(_ne){if(!E(new T(function(){var _nf=A(_n6,[_lC]),_ng=E(_lB),_nh=hs_eqWord64(_nf[1],_ng[1]);if(!E(_nh)){return false;}else{var _ni=hs_eqWord64(_nf[2],_ng[2]);return E(_ni)==0?false:true;}}))){var _nj=new T(function(){return A(_n9,[[1,_ne,new T(function(){return A(_nb,[new T(function(){return A(_na,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _2l(_ne,_nc);}));})]);})]);})]]);}),_nk=A(_nd,[_ne]);if(!_nk[0]){return E(_nj);}else{var _nl=E(_nk[1]);return E(_nl[2])[0]==0?E(_nk[2])[0]==0?A(_n9,[[2,_nl[1]]]):E(_nj):E(_nj);}}else{return A(_n9,[[2,_ne]]);}};},_nm=[0],_nn=new T(function(){return [0,"value"];}),_no=function(_np,_nq,_nr,_ns,_nt,_nu){var _nv=E(_np),_nw=_nv[1],_nx=new T(function(){return A(_nv[3],[_nm]);}),_ny=new T(function(){return _n4(_nv,_nr,_ns,_nt);});return A(_nw,[new T(function(){return _lk(_nq,_nu);}),function(_nz){var _nA=E(_nz);return _nA[0]==0?E(_nx):A(_nw,[new T(function(){return A(_nq,[function(_){var _nB=jsGet(E(_nA[1])[1],E(_nn)[1]);return [1,new T(function(){return fromJSStr(_nB);})];}]);}),function(_nC){var _nD=E(_nC);return _nD[0]==0?E(_nx):A(_ny,[_nD[1]]);}]);}]);},_nE=function(_nF){return E(E(_nF)[2]);},_nG=function(_nH){return E(E(_nH)[2]);},_nI=function(_nJ){return E(E(_nJ)[3]);},_nK=function(_nL){return E(E(_nL)[2]);},_nM=function(_nN,_nO,_nP,_nQ,_nR,_nS,_nT,_nU,_nV,_nW,_nX,_nY){var _nZ=_li(_nS),_o0=_nZ[1],_o1=_nZ[3],_o2=new T(function(){return _fH(_nU);}),_o3=new T(function(){return _nG(_o2);}),_o4=new T(function(){return _nI(_nS);}),_o5=new T(function(){return _nK(_nO);}),_o6=new T(function(){return _eG(_nU);});return A(_o0,[new T(function(){var _o7=E(_nW);if(!_o7[0]){var _o8=E(_nS);return _4p(_nV,_o8[1],_o8[2],_o8[3]);}else{return A(_o1,[_o7[1]]);}}),function(_o9){return A(_o0,[new T(function(){var _oa=E(_nV);return _nE(_nS);}),function(_ob){return A(_nZ[2],[new T(function(){return A(_o4,[new T(function(){var _oc=E(new T(function(){var _od=E(_nV);return [0,coercionToken];})),_oe=E(_ob);return [0,_oe[1],_oe[2],_eF,_oe[4],_oe[5],_oe[6]];})]);}),new T(function(){var _of=new T(function(){return A(_o1,[[0,new T(function(){return A(_o6,[_o9,_nX,new T(function(){var _og=E(_nY);if(!_og[0]){return [0];}else{var _oh=_og[1],_oi=_25(_nR,_lA,_oh);return _oi[0]==0?A(_nK,[_nP,_oh]):E(_oi[1]);}}),_0,_b]);}),_b]]);});return A(_o0,[new T(function(){var _oj=E(_nT);return _no(_oj[1],_oj[2],_nQ,_nN,_nU,_o9);}),function(_ok){var _ol=E(_ok);switch(_ol[0]){case 0:return E(_of);case 1:return A(_o1,[[0,new T(function(){return A(_o3,[new T(function(){return A(_o6,[_o9,_nX,_ol[1],_0,_b]);}),_ol[2]]);}),_b]]);default:var _om=_ol[1];return A(_o1,[[0,new T(function(){return A(_o6,[_o9,_nX,new T(function(){var _on=_25(_nQ,_lA,_om);return _on[0]==0?A(_o5,[_om]):E(_on[1]);}),_0,_b]);}),[1,_om]]]);}}]);})]);}]);}]);},_oo=function(_op,_oq,_or,_os,_ot){var _ou=new T(function(){return _es(_oq);}),_ov=new T(function(){return _47(_ou);}),_ow=new T(function(){return _l8(_ov,_oq);}),_ox=new T(function(){return _lc(_ov,_ou);});return function(_al,_am,_oy){return _nM(_ot,_os,_os,_or,_or,_ox,_ow,_op,[0,coercionToken],_al,_am,_oy);};},_oz=unCStr("base"),_oA=unCStr("Control.Exception.Base"),_oB=unCStr("PatternMatchFail"),_oC=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_oz,_oA,_oB],_oD=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_oC,_a],_oE=function(_oF){return E(_oD);},_oG=function(_oH){var _oI=E(_oH);return _25(_21(_oI[1]),_oE,_oI[2]);},_oJ=function(_oK){return E(E(_oK)[1]);},_oL=function(_oM,_oN){return _2l(E(_oM)[1],_oN);},_oO=function(_oP,_oQ){return _3b(_oL,_oP,_oQ);},_oR=function(_oS,_oT,_oU){return _2l(E(_oT)[1],_oU);},_oV=[0,_oR,_oJ,_oO],_oW=new T(function(){return [0,_oE,_oV,_oX,_oG];}),_oX=function(_oY){return [0,_oW,_oY];},_oZ=unCStr("Non-exhaustive patterns in"),_p0=function(_p1,_p2){return die(new T(function(){return A(_p2,[_p1]);}));},_p3=function(_p4,_p5){var _p6=E(_p5);if(!_p6[0]){return [0,_a,_a];}else{var _p7=_p6[1];if(!A(_p4,[_p7])){return [0,_a,_p6];}else{var _p8=new T(function(){var _p9=_p3(_p4,_p6[2]);return [0,_p9[1],_p9[2]];});return [0,[1,_p7,new T(function(){return E(E(_p8)[1]);})],new T(function(){return E(E(_p8)[2]);})];}}},_pa=[0,32],_pb=[0,10],_pc=[1,_pb,_a],_pd=function(_pe){return E(E(_pe)[1])==124?false:true;},_pf=function(_pg,_ph){var _pi=_p3(_pd,unCStr(_pg)),_pj=_pi[1],_pk=function(_pl,_pm){return _2l(_pl,new T(function(){return unAppCStr(": ",new T(function(){return _2l(_ph,new T(function(){return _2l(_pm,_pc);}));}));}));},_pn=E(_pi[2]);return _pn[0]==0?_pk(_pj,_a):E(E(_pn[1])[1])==124?_pk(_pj,[1,_pa,_pn[2]]):_pk(_pj,_a);},_po=function(_pp){return _p0([0,new T(function(){return _pf(_pp,_oZ);})],_oX);},_pq=new T(function(){return _po("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_pr=function(_ps,_pt){while(1){var _pu=(function(_pv,_pw){var _px=E(_pv);switch(_px[0]){case 0:var _py=E(_pw);if(!_py[0]){return [0];}else{_ps=A(_px[1],[_py[1]]);_pt=_py[2];return null;}break;case 1:var _pz=A(_px[1],[_pw]),_pA=_pw;_ps=_pz;_pt=_pA;return null;case 2:return [0];case 3:return [1,[0,_px[1],_pw],new T(function(){return _pr(_px[2],_pw);})];default:return E(_px[1]);}})(_ps,_pt);if(_pu!=null){return _pu;}}},_pB=function(_pC,_pD){var _pE=new T(function(){var _pF=E(_pD);if(_pF[0]==3){return [3,_pF[1],new T(function(){return _pB(_pC,_pF[2]);})];}else{var _pG=E(_pC);if(_pG[0]==2){return E(_pF);}else{var _pH=E(_pF);if(_pH[0]==2){return E(_pG);}else{var _pI=new T(function(){var _pJ=E(_pH);if(_pJ[0]==4){return [1,function(_pK){return [4,new T(function(){return _2l(_pr(_pG,_pK),_pJ[1]);})];}];}else{var _pL=E(_pG);if(_pL[0]==1){var _pM=_pL[1],_pN=E(_pJ);return _pN[0]==0?[1,function(_pO){return _pB(A(_pM,[_pO]),_pN);}]:[1,function(_pP){return _pB(A(_pM,[_pP]),new T(function(){return A(_pN[1],[_pP]);}));}];}else{var _pQ=E(_pJ);return _pQ[0]==0?E(_pq):[1,function(_pR){return _pB(_pL,new T(function(){return A(_pQ[1],[_pR]);}));}];}}}),_pS=E(_pG);switch(_pS[0]){case 1:var _pT=E(_pH);return _pT[0]==4?[1,function(_pU){return [4,new T(function(){return _2l(_pr(A(_pS[1],[_pU]),_pU),_pT[1]);})];}]:E(_pI);case 4:var _pV=_pS[1],_pW=E(_pH);switch(_pW[0]){case 0:return [1,function(_pX){return [4,new T(function(){return _2l(_pV,new T(function(){return _pr(_pW,_pX);}));})];}];case 1:return [1,function(_pY){return [4,new T(function(){return _2l(_pV,new T(function(){return _pr(A(_pW[1],[_pY]),_pY);}));})];}];default:return [4,new T(function(){return _2l(_pV,_pW[1]);})];}break;default:return E(_pI);}}}}}),_pZ=E(_pC);switch(_pZ[0]){case 0:var _q0=E(_pD);return _q0[0]==0?[0,function(_q1){return _pB(A(_pZ[1],[_q1]),new T(function(){return A(_q0[1],[_q1]);}));}]:E(_pE);case 3:return [3,_pZ[1],new T(function(){return _pB(_pZ[2],_pD);})];default:return E(_pE);}},_q2=function(_q3,_q4){return E(_q3)[1]!=E(_q4)[1];},_q5=function(_q6,_q7){return E(_q6)[1]==E(_q7)[1];},_q8=[0,_q5,_q2],_q9=function(_qa){return E(E(_qa)[1]);},_qb=function(_qc,_qd,_qe){while(1){var _qf=E(_qd);if(!_qf[0]){return E(_qe)[0]==0?true:false;}else{var _qg=E(_qe);if(!_qg[0]){return false;}else{if(!A(_q9,[_qc,_qf[1],_qg[1]])){return false;}else{_qd=_qf[2];_qe=_qg[2];continue;}}}}},_qh=function(_qi,_qj,_qk){return !_qb(_qi,_qj,_qk)?true:false;},_ql=function(_qm){return [0,function(_qn,_qo){return _qb(_qm,_qn,_qo);},function(_qn,_qo){return _qh(_qm,_qn,_qo);}];},_qp=new T(function(){return _ql(_q8);}),_qq=function(_qr,_qs){var _qt=E(_qr);switch(_qt[0]){case 0:return [0,function(_qu){return _qq(A(_qt[1],[_qu]),_qs);}];case 1:return [1,function(_qv){return _qq(A(_qt[1],[_qv]),_qs);}];case 2:return [2];case 3:return _pB(A(_qs,[_qt[1]]),new T(function(){return _qq(_qt[2],_qs);}));default:var _qw=function(_qx){var _qy=E(_qx);if(!_qy[0]){return [0];}else{var _qz=E(_qy[1]);return _2l(_pr(A(_qs,[_qz[1]]),_qz[2]),new T(function(){return _qw(_qy[2]);}));}},_qA=_qw(_qt[1]);return _qA[0]==0?[2]:[4,_qA];}},_qB=[2],_qC=function(_qD){return [3,_qD,_qB];},_qE=function(_qF,_qG){var _qH=E(_qF);if(!_qH){return A(_qG,[_A]);}else{var _qI=new T(function(){return _qE(_qH-1|0,_qG);});return [0,function(_qJ){return E(_qI);}];}},_qK=function(_qL,_qM,_qN){var _qO=new T(function(){return A(_qL,[_qC]);});return [1,function(_qP){return A(function(_qQ,_qR,_qS){while(1){var _qT=(function(_qU,_qV,_qW){var _qX=E(_qU);switch(_qX[0]){case 0:var _qY=E(_qV);if(!_qY[0]){return E(_qM);}else{_qQ=A(_qX[1],[_qY[1]]);_qR=_qY[2];var _qZ=_qW+1|0;_qS=_qZ;return null;}break;case 1:var _r0=A(_qX[1],[_qV]),_r1=_qV,_qZ=_qW;_qQ=_r0;_qR=_r1;_qS=_qZ;return null;case 2:return E(_qM);case 3:return function(_r2){var _r3=new T(function(){return _qq(_qX,_r2);});return _qE(_qW,function(_r4){return E(_r3);});};default:return function(_al){return _qq(_qX,_al);};}})(_qQ,_qR,_qS);if(_qT!=null){return _qT;}}},[_qO,_qP,0,_qN]);}];},_r5=[6],_r6=unCStr("valDig: Bad base"),_r7=new T(function(){return err(_r6);}),_r8=function(_r9,_ra){var _rb=function(_rc,_rd){var _re=E(_rc);if(!_re[0]){var _rf=new T(function(){return A(_rd,[_a]);});return function(_rg){return A(_rg,[_rf]);};}else{var _rh=E(_re[1])[1],_ri=function(_rj){var _rk=new T(function(){return _rb(_re[2],function(_rl){return A(_rd,[[1,_rj,_rl]]);});});return function(_rm){var _rn=new T(function(){return A(_rk,[_rm]);});return [0,function(_ro){return E(_rn);}];};};switch(E(E(_r9)[1])){case 8:if(48>_rh){var _rp=new T(function(){return A(_rd,[_a]);});return function(_rq){return A(_rq,[_rp]);};}else{if(_rh>55){var _rr=new T(function(){return A(_rd,[_a]);});return function(_rs){return A(_rs,[_rr]);};}else{return _ri([0,_rh-48|0]);}}break;case 10:if(48>_rh){var _rt=new T(function(){return A(_rd,[_a]);});return function(_ru){return A(_ru,[_rt]);};}else{if(_rh>57){var _rv=new T(function(){return A(_rd,[_a]);});return function(_rw){return A(_rw,[_rv]);};}else{return _ri([0,_rh-48|0]);}}break;case 16:var _rx=new T(function(){return 97>_rh?65>_rh?[0]:_rh>70?[0]:[1,[0,(_rh-65|0)+10|0]]:_rh>102?65>_rh?[0]:_rh>70?[0]:[1,[0,(_rh-65|0)+10|0]]:[1,[0,(_rh-97|0)+10|0]];});if(48>_rh){var _ry=E(_rx);if(!_ry[0]){var _rz=new T(function(){return A(_rd,[_a]);});return function(_rA){return A(_rA,[_rz]);};}else{return _ri(_ry[1]);}}else{if(_rh>57){var _rB=E(_rx);if(!_rB[0]){var _rC=new T(function(){return A(_rd,[_a]);});return function(_rD){return A(_rD,[_rC]);};}else{return _ri(_rB[1]);}}else{return _ri([0,_rh-48|0]);}}break;default:return E(_r7);}}};return [1,function(_rE){return A(_rb,[_rE,_19,function(_rF){var _rG=E(_rF);return _rG[0]==0?[2]:A(_ra,[_rG]);}]);}];},_rH=[0,10],_rI=[0,1],_rJ=[0,2147483647],_rK=function(_rL,_rM){while(1){var _rN=E(_rL);if(!_rN[0]){var _rO=_rN[1],_rP=E(_rM);if(!_rP[0]){var _rQ=_rP[1],_rR=addC(_rO,_rQ);if(!E(_rR[2])){return [0,_rR[1]];}else{_rL=[1,I_fromInt(_rO)];_rM=[1,I_fromInt(_rQ)];continue;}}else{_rL=[1,I_fromInt(_rO)];_rM=_rP;continue;}}else{var _rS=E(_rM);if(!_rS[0]){_rL=_rN;_rM=[1,I_fromInt(_rS[1])];continue;}else{return [1,I_add(_rN[1],_rS[1])];}}}},_rT=new T(function(){return _rK(_rJ,_rI);}),_rU=function(_rV){var _rW=E(_rV);if(!_rW[0]){var _rX=E(_rW[1]);return _rX==(-2147483648)?E(_rT):[0, -_rX];}else{return [1,I_negate(_rW[1])];}},_rY=[0,10],_rZ=[0,0],_s0=function(_s1,_s2){while(1){var _s3=E(_s1);if(!_s3[0]){var _s4=_s3[1],_s5=E(_s2);if(!_s5[0]){var _s6=_s5[1];if(!(imul(_s4,_s6)|0)){return [0,imul(_s4,_s6)|0];}else{_s1=[1,I_fromInt(_s4)];_s2=[1,I_fromInt(_s6)];continue;}}else{_s1=[1,I_fromInt(_s4)];_s2=_s5;continue;}}else{var _s7=E(_s2);if(!_s7[0]){_s1=_s3;_s2=[1,I_fromInt(_s7[1])];continue;}else{return [1,I_mul(_s3[1],_s7[1])];}}}},_s8=function(_s9,_sa,_sb){while(1){var _sc=E(_sb);if(!_sc[0]){return E(_sa);}else{var _sd=_rK(_s0(_sa,_s9),_sc[1]);_sb=_sc[2];_sa=_sd;continue;}}},_se=function(_sf){var _sg=new T(function(){return _pB(_pB([0,function(_sh){return E(E(_sh)[1])==45?_r8(_rH,function(_si){return A(_sf,[[1,new T(function(){return _rU(_s8(_rY,_rZ,_si));})]]);}):[2];}],[0,function(_sj){return E(E(_sj)[1])==43?_r8(_rH,function(_sk){return A(_sf,[[1,new T(function(){return _s8(_rY,_rZ,_sk);})]]);}):[2];}]),new T(function(){return _r8(_rH,function(_sl){return A(_sf,[[1,new T(function(){return _s8(_rY,_rZ,_sl);})]]);});}));});return _pB([0,function(_sm){return E(E(_sm)[1])==101?E(_sg):[2];}],[0,function(_sn){return E(E(_sn)[1])==69?E(_sg):[2];}]);},_so=function(_sp){return A(_sp,[_b]);},_sq=function(_sr){return A(_sr,[_b]);},_ss=function(_st){var _su=new T(function(){return _r8(_rH,function(_sv){return A(_st,[[1,_sv]]);});});return [0,function(_sw){return E(E(_sw)[1])==46?E(_su):[2];}];},_sx=function(_sy){return _r8(_rH,function(_sz){return _qK(_ss,_so,function(_sA){return _qK(_se,_sq,function(_sB){return A(_sy,[[5,[1,_sz,_sA,_sB]]]);});});});},_sC=function(_sD,_sE,_sF){while(1){var _sG=E(_sF);if(!_sG[0]){return false;}else{if(!A(_q9,[_sD,_sE,_sG[1]])){_sF=_sG[2];continue;}else{return true;}}}},_sH=unCStr("!@#$%&*+./<=>?\\^|:-~"),_sI=function(_sJ){return _sC(_q8,_sJ,_sH);},_sK=[0,8],_sL=[0,16],_sM=function(_sN){var _sO=new T(function(){return _r8(_sL,function(_sP){return A(_sN,[[5,[0,_sL,_sP]]]);});}),_sQ=new T(function(){return _r8(_sK,function(_sR){return A(_sN,[[5,[0,_sK,_sR]]]);});}),_sS=new T(function(){return _r8(_sL,function(_sT){return A(_sN,[[5,[0,_sL,_sT]]]);});}),_sU=new T(function(){return _r8(_sK,function(_sV){return A(_sN,[[5,[0,_sK,_sV]]]);});});return [0,function(_sW){return E(E(_sW)[1])==48?E([0,function(_sX){switch(E(E(_sX)[1])){case 79:return E(_sU);case 88:return E(_sS);case 111:return E(_sQ);case 120:return E(_sO);default:return [2];}}]):[2];}];},_sY=function(_sZ){var _t0=new T(function(){return A(_sZ,[_sL]);}),_t1=new T(function(){return A(_sZ,[_sK]);}),_t2=new T(function(){return A(_sZ,[_sL]);}),_t3=new T(function(){return A(_sZ,[_sK]);});return [0,function(_t4){switch(E(E(_t4)[1])){case 79:return E(_t3);case 88:return E(_t2);case 111:return E(_t1);case 120:return E(_t0);default:return [2];}}];},_t5=function(_t6){return A(_t6,[_rH]);},_t7=function(_t8){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _4k(9,_t8,_a);})));},_t9=function(_ta){var _tb=E(_ta);return _tb[0]==0?E(_tb[1]):I_toInt(_tb[1]);},_tc=function(_td,_te){var _tf=E(_td);if(!_tf[0]){var _tg=_tf[1],_th=E(_te);return _th[0]==0?_tg<=_th[1]:I_compareInt(_th[1],_tg)>=0;}else{var _ti=_tf[1],_tj=E(_te);return _tj[0]==0?I_compareInt(_ti,_tj[1])<=0:I_compare(_ti,_tj[1])<=0;}},_tk=function(_tl){return [2];},_tm=function(_tn){var _to=E(_tn);if(!_to[0]){return E(_tk);}else{var _tp=_to[1],_tq=E(_to[2]);if(!_tq[0]){return E(_tp);}else{var _tr=new T(function(){return _tm(_tq);});return function(_ts){return _pB(A(_tp,[_ts]),new T(function(){return A(_tr,[_ts]);}));};}}},_tt=unCStr("NUL"),_tu=function(_tv){return [2];},_tw=function(_tx){return _tu(_tx);},_ty=function(_tz,_tA){var _tB=function(_tC,_tD){var _tE=E(_tC);if(!_tE[0]){return function(_tF){return A(_tF,[_tz]);};}else{var _tG=E(_tD);if(!_tG[0]){return E(_tu);}else{if(E(_tE[1])[1]!=E(_tG[1])[1]){return E(_tw);}else{var _tH=new T(function(){return _tB(_tE[2],_tG[2]);});return function(_tI){var _tJ=new T(function(){return A(_tH,[_tI]);});return [0,function(_tK){return E(_tJ);}];};}}}};return [1,function(_tL){return A(_tB,[_tz,_tL,_tA]);}];},_tM=[0,0],_tN=function(_tO){var _tP=new T(function(){return A(_tO,[_tM]);});return _ty(_tt,function(_tQ){return E(_tP);});},_tR=unCStr("STX"),_tS=[0,2],_tT=function(_tU){var _tV=new T(function(){return A(_tU,[_tS]);});return _ty(_tR,function(_tW){return E(_tV);});},_tX=unCStr("ETX"),_tY=[0,3],_tZ=function(_u0){var _u1=new T(function(){return A(_u0,[_tY]);});return _ty(_tX,function(_u2){return E(_u1);});},_u3=unCStr("EOT"),_u4=[0,4],_u5=function(_u6){var _u7=new T(function(){return A(_u6,[_u4]);});return _ty(_u3,function(_u8){return E(_u7);});},_u9=unCStr("ENQ"),_ua=[0,5],_ub=function(_uc){var _ud=new T(function(){return A(_uc,[_ua]);});return _ty(_u9,function(_ue){return E(_ud);});},_uf=unCStr("ACK"),_ug=[0,6],_uh=function(_ui){var _uj=new T(function(){return A(_ui,[_ug]);});return _ty(_uf,function(_uk){return E(_uj);});},_ul=unCStr("BEL"),_um=[0,7],_un=function(_uo){var _up=new T(function(){return A(_uo,[_um]);});return _ty(_ul,function(_uq){return E(_up);});},_ur=unCStr("BS"),_us=[0,8],_ut=function(_uu){var _uv=new T(function(){return A(_uu,[_us]);});return _ty(_ur,function(_uw){return E(_uv);});},_ux=unCStr("HT"),_uy=[0,9],_uz=function(_uA){var _uB=new T(function(){return A(_uA,[_uy]);});return _ty(_ux,function(_uC){return E(_uB);});},_uD=unCStr("LF"),_uE=[0,10],_uF=function(_uG){var _uH=new T(function(){return A(_uG,[_uE]);});return _ty(_uD,function(_uI){return E(_uH);});},_uJ=unCStr("VT"),_uK=[0,11],_uL=function(_uM){var _uN=new T(function(){return A(_uM,[_uK]);});return _ty(_uJ,function(_uO){return E(_uN);});},_uP=unCStr("FF"),_uQ=[0,12],_uR=function(_uS){var _uT=new T(function(){return A(_uS,[_uQ]);});return _ty(_uP,function(_uU){return E(_uT);});},_uV=unCStr("CR"),_uW=[0,13],_uX=function(_uY){var _uZ=new T(function(){return A(_uY,[_uW]);});return _ty(_uV,function(_v0){return E(_uZ);});},_v1=unCStr("SI"),_v2=[0,15],_v3=function(_v4){var _v5=new T(function(){return A(_v4,[_v2]);});return _ty(_v1,function(_v6){return E(_v5);});},_v7=unCStr("DLE"),_v8=[0,16],_v9=function(_va){var _vb=new T(function(){return A(_va,[_v8]);});return _ty(_v7,function(_vc){return E(_vb);});},_vd=unCStr("DC1"),_ve=[0,17],_vf=function(_vg){var _vh=new T(function(){return A(_vg,[_ve]);});return _ty(_vd,function(_vi){return E(_vh);});},_vj=unCStr("DC2"),_vk=[0,18],_vl=function(_vm){var _vn=new T(function(){return A(_vm,[_vk]);});return _ty(_vj,function(_vo){return E(_vn);});},_vp=unCStr("DC3"),_vq=[0,19],_vr=function(_vs){var _vt=new T(function(){return A(_vs,[_vq]);});return _ty(_vp,function(_vu){return E(_vt);});},_vv=unCStr("DC4"),_vw=[0,20],_vx=function(_vy){var _vz=new T(function(){return A(_vy,[_vw]);});return _ty(_vv,function(_vA){return E(_vz);});},_vB=unCStr("NAK"),_vC=[0,21],_vD=function(_vE){var _vF=new T(function(){return A(_vE,[_vC]);});return _ty(_vB,function(_vG){return E(_vF);});},_vH=unCStr("SYN"),_vI=[0,22],_vJ=function(_vK){var _vL=new T(function(){return A(_vK,[_vI]);});return _ty(_vH,function(_vM){return E(_vL);});},_vN=unCStr("ETB"),_vO=[0,23],_vP=function(_vQ){var _vR=new T(function(){return A(_vQ,[_vO]);});return _ty(_vN,function(_vS){return E(_vR);});},_vT=unCStr("CAN"),_vU=[0,24],_vV=function(_vW){var _vX=new T(function(){return A(_vW,[_vU]);});return _ty(_vT,function(_vY){return E(_vX);});},_vZ=unCStr("EM"),_w0=[0,25],_w1=function(_w2){var _w3=new T(function(){return A(_w2,[_w0]);});return _ty(_vZ,function(_w4){return E(_w3);});},_w5=unCStr("SUB"),_w6=[0,26],_w7=function(_w8){var _w9=new T(function(){return A(_w8,[_w6]);});return _ty(_w5,function(_wa){return E(_w9);});},_wb=unCStr("ESC"),_wc=[0,27],_wd=function(_we){var _wf=new T(function(){return A(_we,[_wc]);});return _ty(_wb,function(_wg){return E(_wf);});},_wh=unCStr("FS"),_wi=[0,28],_wj=function(_wk){var _wl=new T(function(){return A(_wk,[_wi]);});return _ty(_wh,function(_wm){return E(_wl);});},_wn=unCStr("GS"),_wo=[0,29],_wp=function(_wq){var _wr=new T(function(){return A(_wq,[_wo]);});return _ty(_wn,function(_ws){return E(_wr);});},_wt=unCStr("RS"),_wu=[0,30],_wv=function(_ww){var _wx=new T(function(){return A(_ww,[_wu]);});return _ty(_wt,function(_wy){return E(_wx);});},_wz=unCStr("US"),_wA=[0,31],_wB=function(_wC){var _wD=new T(function(){return A(_wC,[_wA]);});return _ty(_wz,function(_wE){return E(_wD);});},_wF=unCStr("SP"),_wG=[0,32],_wH=function(_wI){var _wJ=new T(function(){return A(_wI,[_wG]);});return _ty(_wF,function(_wK){return E(_wJ);});},_wL=unCStr("DEL"),_wM=[0,127],_wN=function(_wO){var _wP=new T(function(){return A(_wO,[_wM]);});return _ty(_wL,function(_wQ){return E(_wP);});},_wR=[1,_wN,_a],_wS=[1,_wH,_wR],_wT=[1,_wB,_wS],_wU=[1,_wv,_wT],_wV=[1,_wp,_wU],_wW=[1,_wj,_wV],_wX=[1,_wd,_wW],_wY=[1,_w7,_wX],_wZ=[1,_w1,_wY],_x0=[1,_vV,_wZ],_x1=[1,_vP,_x0],_x2=[1,_vJ,_x1],_x3=[1,_vD,_x2],_x4=[1,_vx,_x3],_x5=[1,_vr,_x4],_x6=[1,_vl,_x5],_x7=[1,_vf,_x6],_x8=[1,_v9,_x7],_x9=[1,_v3,_x8],_xa=[1,_uX,_x9],_xb=[1,_uR,_xa],_xc=[1,_uL,_xb],_xd=[1,_uF,_xc],_xe=[1,_uz,_xd],_xf=[1,_ut,_xe],_xg=[1,_un,_xf],_xh=[1,_uh,_xg],_xi=[1,_ub,_xh],_xj=[1,_u5,_xi],_xk=[1,_tZ,_xj],_xl=[1,_tT,_xk],_xm=[1,_tN,_xl],_xn=unCStr("SOH"),_xo=[0,1],_xp=function(_xq){var _xr=new T(function(){return A(_xq,[_xo]);});return _ty(_xn,function(_xs){return E(_xr);});},_xt=unCStr("SO"),_xu=[0,14],_xv=function(_xw){var _xx=new T(function(){return A(_xw,[_xu]);});return _ty(_xt,function(_xy){return E(_xx);});},_xz=function(_xA){return _qK(_xp,_xv,_xA);},_xB=[1,_xz,_xm],_xC=new T(function(){return _tm(_xB);}),_xD=[0,1114111],_xE=[0,34],_xF=[0,_xE,_6D],_xG=[0,39],_xH=[0,_xG,_6D],_xI=[0,92],_xJ=[0,_xI,_6D],_xK=[0,_um,_6D],_xL=[0,_us,_6D],_xM=[0,_uQ,_6D],_xN=[0,_uE,_6D],_xO=[0,_uW,_6D],_xP=[0,_uy,_6D],_xQ=[0,_uK,_6D],_xR=[0,_tM,_6D],_xS=[0,_xo,_6D],_xT=[0,_tS,_6D],_xU=[0,_tY,_6D],_xV=[0,_u4,_6D],_xW=[0,_ua,_6D],_xX=[0,_ug,_6D],_xY=[0,_um,_6D],_xZ=[0,_us,_6D],_y0=[0,_uy,_6D],_y1=[0,_uE,_6D],_y2=[0,_uK,_6D],_y3=[0,_uQ,_6D],_y4=[0,_uW,_6D],_y5=[0,_xu,_6D],_y6=[0,_v2,_6D],_y7=[0,_v8,_6D],_y8=[0,_ve,_6D],_y9=[0,_vk,_6D],_ya=[0,_vq,_6D],_yb=[0,_vw,_6D],_yc=[0,_vC,_6D],_yd=[0,_vI,_6D],_ye=[0,_vO,_6D],_yf=[0,_vU,_6D],_yg=[0,_w0,_6D],_yh=[0,_w6,_6D],_yi=[0,_wc,_6D],_yj=[0,_wi,_6D],_yk=[0,_wo,_6D],_yl=[0,_wu,_6D],_ym=[0,_wA,_6D],_yn=function(_yo){return [0,_yo];},_yp=function(_yq){var _yr=new T(function(){return A(_yq,[_xQ]);}),_ys=new T(function(){return A(_yq,[_xP]);}),_yt=new T(function(){return A(_yq,[_xO]);}),_yu=new T(function(){return A(_yq,[_xN]);}),_yv=new T(function(){return A(_yq,[_xM]);}),_yw=new T(function(){return A(_yq,[_xL]);}),_yx=new T(function(){return A(_yq,[_xK]);}),_yy=new T(function(){return A(_yq,[_xJ]);}),_yz=new T(function(){return A(_yq,[_xH]);}),_yA=new T(function(){return A(_yq,[_xF]);});return _pB([0,function(_yB){switch(E(E(_yB)[1])){case 34:return E(_yA);case 39:return E(_yz);case 92:return E(_yy);case 97:return E(_yx);case 98:return E(_yw);case 102:return E(_yv);case 110:return E(_yu);case 114:return E(_yt);case 116:return E(_ys);case 118:return E(_yr);default:return [2];}}],new T(function(){return _pB(_qK(_sY,_t5,function(_yC){var _yD=new T(function(){return _yn(E(_yC)[1]);});return _r8(_yC,function(_yE){var _yF=_s8(_yD,_rZ,_yE);return !_tc(_yF,_xD)?[2]:A(_yq,[[0,new T(function(){var _yG=_t9(_yF);return _yG>>>0>1114111?_t7(_yG):[0,_yG];}),_6D]]);});}),new T(function(){var _yH=new T(function(){return A(_yq,[_ym]);}),_yI=new T(function(){return A(_yq,[_yl]);}),_yJ=new T(function(){return A(_yq,[_yk]);}),_yK=new T(function(){return A(_yq,[_yj]);}),_yL=new T(function(){return A(_yq,[_yi]);}),_yM=new T(function(){return A(_yq,[_yh]);}),_yN=new T(function(){return A(_yq,[_yg]);}),_yO=new T(function(){return A(_yq,[_yf]);}),_yP=new T(function(){return A(_yq,[_ye]);}),_yQ=new T(function(){return A(_yq,[_yd]);}),_yR=new T(function(){return A(_yq,[_yc]);}),_yS=new T(function(){return A(_yq,[_yb]);}),_yT=new T(function(){return A(_yq,[_ya]);}),_yU=new T(function(){return A(_yq,[_y9]);}),_yV=new T(function(){return A(_yq,[_y8]);}),_yW=new T(function(){return A(_yq,[_y7]);}),_yX=new T(function(){return A(_yq,[_y6]);}),_yY=new T(function(){return A(_yq,[_y5]);}),_yZ=new T(function(){return A(_yq,[_y4]);}),_z0=new T(function(){return A(_yq,[_y3]);}),_z1=new T(function(){return A(_yq,[_y2]);}),_z2=new T(function(){return A(_yq,[_y1]);}),_z3=new T(function(){return A(_yq,[_y0]);}),_z4=new T(function(){return A(_yq,[_xZ]);}),_z5=new T(function(){return A(_yq,[_xY]);}),_z6=new T(function(){return A(_yq,[_xX]);}),_z7=new T(function(){return A(_yq,[_xW]);}),_z8=new T(function(){return A(_yq,[_xV]);}),_z9=new T(function(){return A(_yq,[_xU]);}),_za=new T(function(){return A(_yq,[_xT]);}),_zb=new T(function(){return A(_yq,[_xS]);}),_zc=new T(function(){return A(_yq,[_xR]);});return _pB([0,function(_zd){return E(E(_zd)[1])==94?E([0,function(_ze){switch(E(E(_ze)[1])){case 64:return E(_zc);case 65:return E(_zb);case 66:return E(_za);case 67:return E(_z9);case 68:return E(_z8);case 69:return E(_z7);case 70:return E(_z6);case 71:return E(_z5);case 72:return E(_z4);case 73:return E(_z3);case 74:return E(_z2);case 75:return E(_z1);case 76:return E(_z0);case 77:return E(_yZ);case 78:return E(_yY);case 79:return E(_yX);case 80:return E(_yW);case 81:return E(_yV);case 82:return E(_yU);case 83:return E(_yT);case 84:return E(_yS);case 85:return E(_yR);case 86:return E(_yQ);case 87:return E(_yP);case 88:return E(_yO);case 89:return E(_yN);case 90:return E(_yM);case 91:return E(_yL);case 92:return E(_yK);case 93:return E(_yJ);case 94:return E(_yI);case 95:return E(_yH);default:return [2];}}]):[2];}],new T(function(){return A(_xC,[function(_zf){return A(_yq,[[0,_zf,_6D]]);}]);}));}));}));},_zg=function(_zh){return A(_zh,[_A]);},_zi=function(_zj){var _zk=E(_zj);if(!_zk[0]){return E(_zg);}else{var _zl=_zk[2],_zm=E(E(_zk[1])[1]);switch(_zm){case 9:var _zn=new T(function(){return _zi(_zl);});return function(_zo){var _zp=new T(function(){return A(_zn,[_zo]);});return [0,function(_zq){return E(_zp);}];};case 10:var _zr=new T(function(){return _zi(_zl);});return function(_zs){var _zt=new T(function(){return A(_zr,[_zs]);});return [0,function(_zu){return E(_zt);}];};case 11:var _zv=new T(function(){return _zi(_zl);});return function(_zw){var _zx=new T(function(){return A(_zv,[_zw]);});return [0,function(_zy){return E(_zx);}];};case 12:var _zz=new T(function(){return _zi(_zl);});return function(_zA){var _zB=new T(function(){return A(_zz,[_zA]);});return [0,function(_zC){return E(_zB);}];};case 13:var _zD=new T(function(){return _zi(_zl);});return function(_zE){var _zF=new T(function(){return A(_zD,[_zE]);});return [0,function(_zG){return E(_zF);}];};case 32:var _zH=new T(function(){return _zi(_zl);});return function(_zI){var _zJ=new T(function(){return A(_zH,[_zI]);});return [0,function(_zK){return E(_zJ);}];};case 160:var _zL=new T(function(){return _zi(_zl);});return function(_zM){var _zN=new T(function(){return A(_zL,[_zM]);});return [0,function(_zO){return E(_zN);}];};default:var _zP=u_iswspace(_zm);if(!E(_zP)){return E(_zg);}else{var _zQ=new T(function(){return _zi(_zl);});return function(_zR){var _zS=new T(function(){return A(_zQ,[_zR]);});return [0,function(_zT){return E(_zS);}];};}}}},_zU=function(_zV){var _zW=new T(function(){return _yp(_zV);}),_zX=new T(function(){return _zU(_zV);}),_zY=[1,function(_zZ){return A(_zi,[_zZ,function(_A0){return E([0,function(_A1){return E(E(_A1)[1])==92?E(_zX):[2];}]);}]);}];return _pB([0,function(_A2){return E(E(_A2)[1])==92?E([0,function(_A3){var _A4=E(E(_A3)[1]);switch(_A4){case 9:return E(_zY);case 10:return E(_zY);case 11:return E(_zY);case 12:return E(_zY);case 13:return E(_zY);case 32:return E(_zY);case 38:return E(_zX);case 160:return E(_zY);default:var _A5=u_iswspace(_A4);return E(_A5)==0?[2]:E(_zY);}}]):[2];}],[0,function(_A6){var _A7=E(_A6);return E(_A7[1])==92?E(_zW):A(_zV,[[0,_A7,_0]]);}]);},_A8=function(_A9,_Aa){var _Ab=new T(function(){return A(_Aa,[[1,new T(function(){return A(_A9,[_a]);})]]);});return _zU(function(_Ac){var _Ad=E(_Ac),_Ae=E(_Ad[1]);return E(_Ae[1])==34?!E(_Ad[2])?E(_Ab):_A8(function(_Af){return A(_A9,[[1,_Ae,_Af]]);},_Aa):_A8(function(_Ag){return A(_A9,[[1,_Ae,_Ag]]);},_Aa);});},_Ah=unCStr("_\'"),_Ai=function(_Aj){var _Ak=u_iswalnum(_Aj);return E(_Ak)==0?_sC(_q8,[0,_Aj],_Ah):true;},_Al=function(_Am){return _Ai(E(_Am)[1]);},_An=unCStr(",;()[]{}`"),_Ao=function(_Ap){return A(_Ap,[_a]);},_Aq=function(_Ar,_As){var _At=function(_Au){var _Av=E(_Au);if(!_Av[0]){return E(_Ao);}else{var _Aw=_Av[1];if(!A(_Ar,[_Aw])){return E(_Ao);}else{var _Ax=new T(function(){return _At(_Av[2]);});return function(_Ay){var _Az=new T(function(){return A(_Ax,[function(_AA){return A(_Ay,[[1,_Aw,_AA]]);}]);});return [0,function(_AB){return E(_Az);}];};}}};return [1,function(_AC){return A(_At,[_AC,_As]);}];},_AD=unCStr(".."),_AE=unCStr("::"),_AF=unCStr("->"),_AG=[0,64],_AH=[1,_AG,_a],_AI=[0,126],_AJ=[1,_AI,_a],_AK=unCStr("=>"),_AL=[1,_AK,_a],_AM=[1,_AJ,_AL],_AN=[1,_AH,_AM],_AO=[1,_AF,_AN],_AP=unCStr("<-"),_AQ=[1,_AP,_AO],_AR=[0,124],_AS=[1,_AR,_a],_AT=[1,_AS,_AQ],_AU=[1,_xI,_a],_AV=[1,_AU,_AT],_AW=[0,61],_AX=[1,_AW,_a],_AY=[1,_AX,_AV],_AZ=[1,_AE,_AY],_B0=[1,_AD,_AZ],_B1=function(_B2){var _B3=new T(function(){return A(_B2,[_r5]);});return _pB([1,function(_B4){return E(_B4)[0]==0?E(_B3):[2];}],new T(function(){var _B5=new T(function(){return _yp(function(_B6){var _B7=E(_B6);return (function(_B8,_B9){var _Ba=new T(function(){return A(_B2,[[0,_B8]]);});return !E(_B9)?E(E(_B8)[1])==39?[2]:[0,function(_Bb){return E(E(_Bb)[1])==39?E(_Ba):[2];}]:[0,function(_Bc){return E(E(_Bc)[1])==39?E(_Ba):[2];}];})(_B7[1],_B7[2]);});});return _pB([0,function(_Bd){return E(E(_Bd)[1])==39?E([0,function(_Be){var _Bf=E(_Be);switch(E(_Bf[1])){case 39:return [2];case 92:return E(_B5);default:var _Bg=new T(function(){return A(_B2,[[0,_Bf]]);});return [0,function(_Bh){return E(E(_Bh)[1])==39?E(_Bg):[2];}];}}]):[2];}],new T(function(){var _Bi=new T(function(){return _A8(_19,_B2);});return _pB([0,function(_Bj){return E(E(_Bj)[1])==34?E(_Bi):[2];}],new T(function(){return _pB([0,function(_Bk){return !_sC(_q8,_Bk,_An)?[2]:A(_B2,[[2,[1,_Bk,_a]]]);}],new T(function(){return _pB([0,function(_Bl){return !_sC(_q8,_Bl,_sH)?[2]:_Aq(_sI,function(_Bm){var _Bn=[1,_Bl,_Bm];return !_sC(_qp,_Bn,_B0)?A(_B2,[[4,_Bn]]):A(_B2,[[2,_Bn]]);});}],new T(function(){return _pB([0,function(_Bo){var _Bp=E(_Bo),_Bq=_Bp[1],_Br=u_iswalpha(_Bq);return E(_Br)==0?E(_Bq)==95?_Aq(_Al,function(_Bs){return A(_B2,[[3,[1,_Bp,_Bs]]]);}):[2]:_Aq(_Al,function(_Bt){return A(_B2,[[3,[1,_Bp,_Bt]]]);});}],new T(function(){return _qK(_sM,_sx,_B2);}));}));}));}));}));}));},_Bu=function(_Bv){var _Bw=new T(function(){return _B1(_Bv);});return [1,function(_Bx){return A(_zi,[_Bx,function(_By){return E(_Bw);}]);}];},_Bz=[0,0],_BA=function(_BB,_BC){var _BD=new T(function(){return A(_BB,[_Bz,function(_BE){var _BF=new T(function(){return A(_BC,[_BE]);});return _Bu(function(_BG){var _BH=E(_BG);if(_BH[0]==2){var _BI=E(_BH[1]);return _BI[0]==0?[2]:E(E(_BI[1])[1])==41?E(_BI[2])[0]==0?E(_BF):[2]:[2];}else{return [2];}});}]);});return _Bu(function(_BJ){var _BK=E(_BJ);if(_BK[0]==2){var _BL=E(_BK[1]);return _BL[0]==0?[2]:E(E(_BL[1])[1])==40?E(_BL[2])[0]==0?E(_BD):[2]:[2];}else{return [2];}});},_BM=function(_BN){return _pB(_Bu(function(_BO){var _BP=E(_BO);return _BP[0]==0?A(_BN,[_BP[1]]):[2];}),new T(function(){return _BA(_BQ,_BN);}));},_BQ=function(_BR,_BS){return _BM(_BS);},_BT=function(_BU,_BV){var _BW=function(_BX,_BY){var _BZ=new T(function(){return A(_BY,[_a]);}),_C0=new T(function(){return A(_BU,[_Bz,function(_C1){return _BW(_6D,function(_C2){return A(_BY,[[1,_C1,_C2]]);});}]);});return _Bu(function(_C3){var _C4=E(_C3);if(_C4[0]==2){var _C5=E(_C4[1]);if(!_C5[0]){return [2];}else{var _C6=_C5[2];switch(E(E(_C5[1])[1])){case 44:return E(_C6)[0]==0?!E(_BX)?[2]:E(_C0):[2];case 93:return E(_C6)[0]==0?E(_BZ):[2];default:return [2];}}}else{return [2];}});},_C7=function(_C8){var _C9=new T(function(){return _pB(_BW(_0,_C8),new T(function(){return A(_BU,[_Bz,function(_Ca){return _BW(_6D,function(_Cb){return A(_C8,[[1,_Ca,_Cb]]);});}]);}));});return _pB(_Bu(function(_Cc){var _Cd=E(_Cc);if(_Cd[0]==2){var _Ce=E(_Cd[1]);return _Ce[0]==0?[2]:E(E(_Ce[1])[1])==91?E(_Ce[2])[0]==0?E(_C9):[2]:[2];}else{return [2];}}),new T(function(){return _BA(function(_Cf,_Cg){return _C7(_Cg);},_C8);}));};return _C7(_BV);},_Ch=function(_Ci){return _pB(_pB(_Bu(function(_Cj){var _Ck=E(_Cj);return _Ck[0]==1?A(_Ci,[_Ck[1]]):[2];}),new T(function(){return _BT(_BQ,_Ci);})),new T(function(){return _BA(_Cl,_Ci);}));},_Cl=function(_Cm,_Cn){return _Ch(_Cn);},_Co=new T(function(){return _BA(_Cl,_qC);}),_Cp=new T(function(){return _BT(_BQ,_qC);}),_Cq=function(_Cr){var _Cs=E(_Cr);return _Cs[0]==1?[3,_Cs[1],_qB]:[2];},_Ct=new T(function(){return _B1(_Cq);}),_Cu=function(_Cv){return E(_Ct);},_Cw=function(_Cx){return A(_zi,[_Cx,_Cu]);},_Cy=[1,_Cw],_Cz=new T(function(){return _pB(_Cy,_Cp);}),_CA=new T(function(){return _pB(_Cz,_Co);}),_CB=function(_CC){return _pr(_CA,_CC);},_CD=new T(function(){return _BM(_qC);}),_CE=function(_CC){return _pr(_CD,_CC);},_CF=function(_CG){return E(_CE);},_CH=[0,_CF,_CB,_BQ,_Cl],_CI=function(_CJ){return E(E(_CJ)[4]);},_CK=function(_CL,_CM,_CN){return _BT(new T(function(){return _CI(_CL);}),_CN);},_CO=function(_CP){var _CQ=new T(function(){return _BT(new T(function(){return _CI(_CP);}),_qC);});return function(_al){return _pr(_CQ,_al);};},_CR=function(_CS,_CT){var _CU=new T(function(){return A(_CI,[_CS,_CT,_qC]);});return function(_al){return _pr(_CU,_al);};},_CV=function(_CW){return [0,function(_CC){return _CR(_CW,_CC);},new T(function(){return _CO(_CW);}),new T(function(){return _CI(_CW);}),function(_CX,_CC){return _CK(_CW,_CX,_CC);}];},_CY=new T(function(){return _CV(_CH);}),_CZ=new T(function(){return _iO(_iE);}),_D0=new T(function(){return _oo(_eq,_er,_lA,_CZ,_CY);}),_D1=function(_1w,_){return _1x(_J,_1w,_);},_D2=function(_D3,_D4,_D5,_){var _D6=A(_D4,[_D5,_]),_D7=E(_D6),_D8=E(_D7[1]);return [0,[0,function(_D9,_){var _Da=_1x(_J,_D9,_),_Db=A(_B,[_19,_Da,_1h,_D3,_]),_Dc=A(_D8[1],[_Da,_]);return _Da;},_D8[2]],_D7[2]];},_Dd=new T(function(){return _4p(_1D,_4c,_1H,_1E);}),_De=new T(function(){return _4p(_1D,_4c,_1H,_1E);}),_Df=function(_Dg,_Dh,_Di,_){var _Dj=A(_Dd,[_Di,_]),_Dk=A(_De,[new T(function(){return E(E(_Dj)[2]);}),_]),_Dl=E(_Dk),_Dm=_Dl[1],_Dn=E(_Dl[2]),_Do=_Dn[2],_Dp=E(_Dn[4]),_Dq=new T(function(){return E(E(_Dj)[1]);}),_Dr=function(_Ds){var _Dt=new T(function(){return A(_Dh,[_Ds]);});return function(_Du,_){var _Dv=A(_Dt,[_Du,_]),_Dw=E(_Dv),_Dx=E(_Dw[1]);return [0,[0,function(_Dy,_){var _Dz=E(_Dq),_DA=jsFind(toJSStr(_Dz)),_DB=E(_DA);if(!_DB[0]){return _4D(_Dz);}else{var _DC=E(_DB[1]),_DD=A(_7,[E(_DC[1]),_]),_DE=jsKillChild(E(_DC)[1],_DD),_DF=A(_Dx[1],[_Dy,_]);return _Dy;}},_Dx[2]],_Dw[2]];};},_DG=_D2(_Dq,_Dg,[0,_Dn[1],_Do,_Dn[3],[0,function(_){return _4F(function(_DH,_){var _DI=_D2(_Dq,_Dg,new T(function(){var _DJ=E(_DH);return [0,_DJ[1],_Do,_DJ[3],_DJ[4],_DJ[5],_DJ[6]];}),_);return [0,[0,_3C,E(E(_DI)[1])[2]],_DH];},_Dm,_);},function(_DK,_){var _DL=_4F(new T(function(){return _Dr(_DK);}),_Dm,_),_DM=E(_DL);return _DM[0]==0?_b:A(_Dp[2],[_DM[1],_]);}],_Dn[5],_Dn[6]],_),_DN=E(_DG),_DO=_DN[2],_DP=E(_DN[1]),_DQ=_DP[1],_DR=new T(function(){return _1p(_D1,[1,[0,_1h,_Dm],_a]);}),_DS=E(_DP[2]);if(!_DS[0]){return [0,[0,function(_DT,_){var _DU=A(_DQ,[_DT,_]),_DV=A(_DR,[_DT,_]);return _DT;},_b],new T(function(){var _DW=E(_DO);return [0,_DW[1],_DW[2],_DW[3],_Dp,_DW[5],_DW[6]];})];}else{var _DX=A(_Dr,[_DS[1],new T(function(){var _DY=E(_DO);return [0,_DY[1],_DY[2],_DY[3],_Dp,_DY[5],_DY[6]];}),_]),_DZ=E(_DX),_E0=E(_DZ[1]);return [0,[0,function(_E1,_){var _E2=A(_DQ,[_E1,_]),_E3=A(_DR,[_E1,_]),_E4=A(_E0[1],[_E3,_]);return _E1;},_E0[2]],_DZ[2]];}},_E5=function(_E6,_E7){var _E8=new T(function(){return _8P(new T(function(){return A(_D0,[_b,_kX,[1,_E7]]);}),_kC);}),_E9=new T(function(){return _af(new T(function(){if(!E(_E6)){var _Ea=new T(function(){return _kZ(_6N,_E7);});return function(_Eb,_){var _Ec=A(_Ea,[_Eb,_]),_Ed=A(_B,[_19,_Ec,_bq,_kW,_]);return _Ec;};}else{return _kZ(_6N,_E7);}}),_kB);});return function(_al,_am){return _Df(_E9,function(_Ee,_gE,_){return (function(_gE,_){return _Df(function(_gE,_){return _54(function(_Ef,_){var _Eg=A(_E8,[_Ef,_]),_Eh=E(_Eg),_Ei=E(_Eh[1]);return [0,[0,function(_Ej,_){var _Ek=A(_Ei[1],[_Ej,_]),_El=A(_B,[_19,_Ek,_bq,_kV,_]);return _Ek;},_Ei[2]],_Eh[2]];},_kP,_gE,_);},function(_gz){return _E5(_E6,_gz);},_gE,_);})(_gE,_);},_al,_am);};},_Em=function(_gE,_){return _K(_1d,_H,_En,_gE,_);},_Eo=function(_Ep){return E(_Em);},_Eq=function(_gz){return _Eo(_gz);},_Er=function(_Es,_Et){while(1){var _Eu=(function(_Ev,_Ew){var _Ex=E(_Ew);if(!_Ex[0]){return E(_Ev);}else{_Es=function(_Ey,_){var _Ez=A(_Ev,[_Ey,_]),_EA=E(_Ez),_EB=E(_EA[1]),_EC=A(_Ex[1],[_EA[2],_]),_ED=E(_EC),_EE=E(_ED[1]);return [0,[0,function(_EF,_){var _EG=A(_EB[1],[_EF,_]),_EH=A(_EE[1],[_EF,_]);return _EF;},new T(function(){var _EI=E(_EB[2]);return _EI[0]==0?E(_EE[2]):E(_EI);})],_ED[2]];};_Et=_Ex[2];return null;}})(_Es,_Et);if(_Eu!=null){return _Eu;}}},_EJ=unCStr("li"),_EK=function(_EL,_EM){var _EN=new T(function(){return A(_EL,[_EM]);});return function(_EO,_){var _EP=jsCreateElem(toJSStr(E(_EJ))),_EQ=jsAppendChild(_EP,E(_EO)[1]),_ER=[0,_EP],_ES=A(_EN,[_ER,_]);return _ER;};},_ET=unCStr("check"),_EU=unCStr("todo-list"),_EV=unCStr("active"),_EW=[0,_a],_EX=[1,_EW],_EY=function(_EZ,_){return _EZ;},_F0=unCStr("Main"),_F1=unCStr("PresentationMode"),_F2=[0,I_fromBits([2632422978,3726471947]),I_fromBits([3374154759,1744714295]),_z,_F0,_F1],_F3=[0,I_fromBits([2632422978,3726471947]),I_fromBits([3374154759,1744714295]),_F2,_a],_F4=function(_F5){return E(_F3);},_F6=function(_F7,_F8){var _F9=hs_leWord64(_F7,_F8);return E(_F9)==0?false:true;},_Fa=function(_Fb,_Fc,_Fd,_Fe){var _Ff=hs_eqWord64(_Fb,_Fd);if(!E(_Ff)){var _Fg=hs_leWord64(_Fb,_Fd);return E(_Fg)==0?false:true;}else{return _F6(_Fc,_Fe);}},_Fh=function(_Fi,_Fj){var _Fk=E(_Fi),_Fl=_Fk[1],_Fm=_Fk[2],_Fn=E(_Fj),_Fo=_Fn[1],_Fp=_Fn[2],_Fq=hs_eqWord64(_Fl,_Fo);if(!E(_Fq)){return !_Fa(_Fl,_Fm,_Fo,_Fp)?2:0;}else{var _Fr=hs_eqWord64(_Fm,_Fp);return E(_Fr)==0?!_Fa(_Fl,_Fm,_Fo,_Fp)?2:0:1;}},_Fs=function(_Ft,_Fu){while(1){var _Fv=E(_Ft),_Fw=E(_Fu);if(!_Fw[0]){switch(_Fh(_Fv,_Fw[2])){case 0:_Ft=_Fv;_Fu=_Fw[4];continue;case 1:return [1,_Fw[3]];default:_Ft=_Fv;_Fu=_Fw[5];continue;}}else{return [0];}}},_Fx=function(_Fy,_Fz,_FA,_FB){var _FC=E(_Fz),_FD=_FC[1],_FE=_FC[3],_FF=new T(function(){return A(_FB,[_lC]);}),_FG=new T(function(){return A(_FE,[_b]);});return A(_FD,[new T(function(){return A(_FD,[_FA,function(_FH){return A(_FE,[new T(function(){var _FI=E(_Fy);return E(E(_FH)[6]);})]);}]);}),function(_FJ){var _FK=_Fs(_FF,_FJ);return _FK[0]==0?E(_FG):A(_FE,[[1,_FK[1]]]);}]);},_FL=new T(function(){return _Fx(_1D,_4c,_1H,_F4);}),_FM=function(_FN,_){var _FO=A(_FL,[_FN,_]);return [0,[0,_EY,new T(function(){var _FP=E(E(_FO)[1]);return _FP[0]==0?E(_EX):E(_FP);})],new T(function(){return E(E(_FO)[2]);})];},_FQ=function(_FR,_FS,_){return _54(_FM,function(_FT){var _FU=E(_FT)[1];return function(_FV,_){return [0,[0,_3C,[1,new T(function(){var _FW=new T(function(){return _7p(_FU,_EV);}),_FX=new T(function(){return _7p(_FU,_a);});return _6E(function(_FY,_FZ){var _G0=E(_FZ)[2];return !E(_FX)?!E(_FW)?E(_G0)==0?true:false:E(_G0)==0?false:true:true;},_FR);})]],_FV];};},_FS,_);},_G1=unCStr("toggle"),_G2=function(_G3,_G4){while(1){var _G5=E(_G3);if(!_G5[0]){return E(_G4);}else{_G3=_G5[2];var _G6=[1,_G5[1],_G4];_G4=_G6;continue;}}},_G7=function(_G8){return E(_G8);},_G9=function(_Ga,_Gb,_){var _Gc=function(_Gd,_Ge){while(1){var _Gf=(function(_Gg,_Gh){var _Gi=E(_Gh);switch(_Gi[0]){case 0:_Gd=new T(function(){return _Gc(_Gg,_Gi[4]);});_Ge=_Gi[3];return null;case 1:var _Gj=_Gi[1];return [1,new T(function(){var _Gk=E(_Gi[2]),_Gl=_Gk[1],_Gm=new T(function(){return _E5(_bk,_Gl);}),_Gn=new T(function(){return _kz(new T(function(){return _aO(_Gj,[0,_Gl,_bk],_Ga);}));}),_Go=new T(function(){return _E5(_bj,_Gl);}),_Gp=new T(function(){return _kz(new T(function(){return _aO(_Gj,[0,_Gl,_bj],_Ga);}));}),_Gq=function(_gE,_){return _54(function(_gE,_){return _54(_Gp,_Eo,_gE,_);},function(_Gr){return E(_Go);},_gE,_);},_Gs=new T(function(){return _8P(new T(function(){return A(_fw,[new T(function(){return E(_Gk[2])==0?true:false;}),_ET]);}),_6L);});return function(_al,_am){return _Df(function(_Gt,_){var _Gu=_54(function(_Gv,_){var _Gw=A(_Gs,[_Gv,_]),_Gx=E(_Gw),_Gy=E(_Gx[1]);return [0,[0,function(_Gz,_){var _GA=A(_Gy[1],[_Gz,_]),_GB=A(_B,[_19,_GA,_bq,_G1,_]);return _GA;},_Gy[2]],_Gx[2]];},function(_GC){return (function(_GD){var _GE=E(_GD);return _GE[0]==0?E(_Gq):!_7p(_GE[1],_ET)?E(_Gq):E(_GE[2])[0]==0?function(_gE,_){return _54(function(_gE,_){return _54(_Gn,_Eq,_gE,_);},function(_GF){return E(_Gm);},_gE,_);}:E(_Gq);})(E(_GC)[1]);},_Gt,_),_GG=E(_Gu),_GH=E(_GG[1]),_GI=_54(_fz,_bm,_GG[2],_),_GJ=E(_GI);return [0,[0,new T(function(){return _EK(_G7,function(_GK,_){var _GL=A(_GH[1],[_GK,_]),_GM=A(E(_GJ[1])[1],[_GK,_]);return _GK;});}),_GH[2]],_GJ[2]];},function(_GN,_gE,_){return (function(_GO,_){return A(new T(function(){var _GP=A(_ky,[new T(function(){return A(_6t,[_iT,0,new T(function(){return _aA(_Gj,_Ga);}),_a]);})]);return function(_GQ,_){var _GR=A(_GP,[_]);return [0,[0,_3C,[1,_GR]],_GQ];};}),[new T(function(){var _GS=E(_GO);return [0,_GS[1],_GS[2],_GS[3],_GS[4],_6D,_GS[6]];}),_]);})(_gE,_);},_al,_am);};}),_Gg];default:return E(_Gg);}})(_Gd,_Ge);if(_Gf!=null){return _Gf;}}};return _54(function(_gE,_){return _FQ(_Ga,_gE,_);},function(_GT){return function(_al,_am){return _K(_EU,_H,new T(function(){var _GU=E(_GT);if(!_GU[0]){var _GV=_GU[3],_GW=_GU[4];return _GU[2]>=0?_Er(_ay,_G2(_Gc(new T(function(){return _Gc(_a,_GW);}),_GV),_a)):_Er(_ay,_G2(_Gc(new T(function(){return _Gc(_a,_GV);}),_GW),_a));}else{return _Er(_ay,_G2(_Gc(_a,_GU),_a));}}),_al,_am);};},_Gb,_);},_GX=[2],_GY=[1,_GX],_GZ=unCStr("Prelude.read: ambiguous parse"),_H0=new T(function(){return err(_GZ);}),_H1=unCStr("Prelude.read: no parse"),_H2=new T(function(){return err(_H1);}),_H3=unCStr("fromList"),_H4=function(_H5){return E(E(_H5)[3]);},_H6=function(_H7,_H8,_H9){var _Ha=new T(function(){return A(_H4,[_H7,_H9]);}),_Hb=new T(function(){return _H4(_H8);});return function(_Hc){return A(_Ha,[function(_Hd){var _He=new T(function(){return A(_Hb,[_H9,function(_Hf){return A(_Hc,[[0,_Hd,_Hf]]);}]);});return _Bu(function(_Hg){var _Hh=E(_Hg);if(_Hh[0]==2){var _Hi=E(_Hh[1]);return _Hi[0]==0?[2]:E(E(_Hi[1])[1])==44?E(_Hi[2])[0]==0?E(_He):[2]:[2];}else{return [2];}});}]);};},_Hj=function(_Hk,_Hl,_Hm){var _Hn=function(_Ho,_Hp){var _Hq=new T(function(){return _B1(function(_Hr){return A(_Hk,[_Hr,_Ho,function(_Hs){return A(_Hp,[new T(function(){return [0, -E(_Hs)[1]];})]);}]);});});return _pB(_Bu(function(_Ht){var _Hu=E(_Ht);if(_Hu[0]==4){var _Hv=E(_Hu[1]);return _Hv[0]==0?A(_Hk,[_Hu,_Ho,_Hp]):E(E(_Hv[1])[1])==45?E(_Hv[2])[0]==0?E([1,function(_Hw){return A(_zi,[_Hw,function(_Hx){return E(_Hq);}]);}]):A(_Hk,[_Hu,_Ho,_Hp]):A(_Hk,[_Hu,_Ho,_Hp]);}else{return A(_Hk,[_Hu,_Ho,_Hp]);}}),new T(function(){return _BA(_Hn,_Hp);}));};return _Hn(_Hl,_Hm);},_Hy=function(_Hz,_HA){return [2];},_HB=function(_CX,_CC){return _Hy(_CX,_CC);},_HC=function(_HD){var _HE=E(_HD);return _HE[0]==0?[1,new T(function(){return _s8(new T(function(){return _yn(E(_HE[1])[1]);}),_rZ,_HE[2]);})]:E(_HE[2])[0]==0?E(_HE[3])[0]==0?[1,new T(function(){return _s8(_rY,_rZ,_HE[1]);})]:[0]:[0];},_HF=function(_HG){var _HH=E(_HG);if(_HH[0]==5){var _HI=_HC(_HH[1]);if(!_HI[0]){return E(_Hy);}else{var _HJ=new T(function(){return [0,_t9(_HI[1])];});return function(_HK,_HL){return A(_HL,[_HJ]);};}}else{return E(_HB);}},_HM=function(_CX,_CC){return _Hj(_HF,_CX,_CC);},_HN=function(_HO,_HP){return _BT(_HM,_HP);},_HQ=new T(function(){return _BT(_HM,_qC);}),_HR=function(_CC){return _pr(_HQ,_CC);},_HS=function(_HT){var _HU=new T(function(){return _Hj(_HF,_HT,_qC);});return function(_al){return _pr(_HU,_al);};},_HV=[0,_HS,_HR,_HM,_HN],_HW=function(_HX,_HY){var _HZ=function(_I0){return _pB(_BA(_HX,_I0),new T(function(){return _BA(function(_I1,_I2){return _HZ(_I2);},_I0);}));};return _HZ(_HY);},_I3=function(_I4,_I5){while(1){var _I6=E(_I5);if(!_I6[0]){return E(_I4);}else{var _I7=E(_I6[1]),_I8=_aO(E(_I7[1])[1],_I7[2],_I4);_I5=_I6[2];_I4=_I8;continue;}}},_I9=function(_Ia){return _I3(_GX,_Ia);},_Ib=function(_Ic,_Id){var _Ie=function(_If){var _Ig=new T(function(){return A(_Ic,[_If]);});return function(_Ih){return _pB(A(_Ig,[_Ih]),new T(function(){return _BA(_Ie,_Ih);}));};};return _Ie(_Id);},_Ii=function(_Ij,_Ik){return _Ib(function(_Il){return E(_Il)[1]>10?E(_tk):function(_Im){var _In=new T(function(){return _BT(function(_Io,_Ip){return _HW(function(_Iq){return _H6(_HV,_Ij,_Iq);},_Ip);},function(_Ir){return A(_Im,[new T(function(){return _I9(_Ir);})]);});});return _Bu(function(_Is){var _It=E(_Is);return _It[0]==3?!_7p(_It[1],_H3)?[2]:E(_In):[2];});};},_Ik);},_Iu=function(_Iv,_Iw,_Ix,_Iy){return _HW(function(_CC){return _H6(_Iv,_Iw,_CC);},_Iy);},_Iz=function(_IA,_IB,_IC,_ID){return _BT(function(_CX,_CC){return _Iu(_IA,_IB,_CX,_CC);},_ID);},_IE=function(_IF,_IG){var _IH=new T(function(){return _BT(function(_CX,_CC){return _Iu(_IF,_IG,_CX,_CC);},_qC);});return function(_al){return _pr(_IH,_al);};},_II=function(_IJ,_IK,_IL){var _IM=new T(function(){return _HW(function(_CC){return _H6(_IJ,_IK,_CC);},_qC);});return function(_al){return _pr(_IM,_al);};},_IN=function(_IO,_IP){return [0,function(_CC){return _II(_IO,_IP,_CC);},new T(function(){return _IE(_IO,_IP);}),function(_CX,_CC){return _Iu(_IO,_IP,_CX,_CC);},function(_CX,_CC){return _Iz(_IO,_IP,_CX,_CC);}];},_IQ=function(_IR,_IS){return A(_IS,[_bj]);},_IT=[0,_gv,_IQ],_IU=[1,_IT,_a],_IV=function(_IW,_IX){return A(_IX,[_bk]);},_IY=[0,_gu,_IV],_IZ=[1,_IY,_IU],_J0=function(_J1,_J2,_J3){var _J4=E(_J1);if(!_J4[0]){return [2];}else{var _J5=E(_J4[1]),_J6=_J5[1],_J7=new T(function(){return A(_J5[2],[_J2,_J3]);});return _pB(_Bu(function(_J8){var _J9=E(_J8);switch(_J9[0]){case 3:return !_7p(_J6,_J9[1])?[2]:E(_J7);case 4:return !_7p(_J6,_J9[1])?[2]:E(_J7);default:return [2];}}),new T(function(){return _J0(_J4[2],_J2,_J3);}));}},_Ja=function(_Jb,_Jc){return _J0(_IZ,_Jb,_Jc);},_Jd=function(_gz){return _Ib(_Ja,_gz);},_Je=function(_Jf,_Jg){return _BT(_Jd,_Jg);},_Jh=new T(function(){return _BT(_Jd,_qC);}),_Ji=function(_gz){return _pr(_Jh,_gz);},_Jj=function(_Jk){var _Jl=new T(function(){return A(_Ib,[_Ja,_Jk,_qC]);});return function(_al){return _pr(_Jl,_al);};},_Jm=[0,_Jj,_Ji,_Jd,_Je],_Jn=new T(function(){return _CV(_CH);}),_Jo=new T(function(){return _IN(_Jn,_Jm);}),_Jp=function(_Jq){return [1,function(_Jr){return A(_zi,[_Jr,function(_Js){return E([3,_Jq,_qB]);}]);}];},_Jt=new T(function(){return A(_Ii,[_Jo,_Bz,_Jp]);}),_Ju=function(_Jv){while(1){var _Jw=(function(_Jx){var _Jy=E(_Jx);if(!_Jy[0]){return [0];}else{var _Jz=_Jy[2],_JA=E(_Jy[1]);if(!E(_JA[2])[0]){return [1,_JA[1],new T(function(){return _Ju(_Jz);})];}else{_Jv=_Jz;return null;}}})(_Jv);if(_Jw!=null){return _Jw;}}},_JB=function(_JC){var _JD=E(_JC);return _JD[0]==0?E(_ay):function(_JE,_){return [0,[0,_3C,[1,new T(function(){var _JF=_Ju(_pr(_Jt,_JD[1]));return _JF[0]==0?E(_H2):E(_JF[2])[0]==0?E(_JF[1]):E(_H0);})]],_JE];};},_JG=function(_JH){return _JH>0;},_JI=new T(function(){return [0,"(function(x) {return x === null;})"];}),_JJ=new T(function(){return _5(_JI);}),_JK=unCStr("No such value"),_JL=[0,_JK],_JM=unCStr("Invalid JSON!"),_JN=[0,_JM],_JO=new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}),_JP=function(_JQ){return E(E(_JQ)[3]);},_JR=function(_JS,_JT,_){var _JU=A(_5,[_JO,E(toJSStr(E(_JT))),_]);return new T(function(){if(!_2(function(_){var _=0,_JV=A(_JJ,[E(_JU),_]);return new T(function(){return _JG(_JV);});})){var _JW=String(_JU),_JX=jsParseJSON(_JW),_JY=E(_JX);return _JY[0]==0?E(_JN):A(_JP,[_JS,_JY[1]]);}else{return E(_JL);}});},_JZ=function(_K0,_){var _K1=_JR(_jE,_iU,_);return [0,[0,_3C,[1,_K1]],new T(function(){var _K2=E(_K0);return [0,_K2[1],_K2[2],_K2[3],_K2[4],_6D,_K2[6]];})];},_K3=function(_K4,_){var _K5=_54(_JZ,_JB,_K4,_),_K6=E(_K5),_K7=E(_K6[1]);return [0,[0,function(_K8,_){var _K9=A(_K7[1],[_K8,_]);return _K8;},new T(function(){var _Ka=E(_K7[2]);return _Ka[0]==0?E(_GY):E(_Ka);})],_K6[2]];},_Kb=function(_Kc,_){var _Kd=_54(_K3,_G9,_Kc,_),_Ke=E(_Kd);return [0,[0,function(_Kf,_){var _Kg=A(E(_Ke[1])[1],[_Kf,_]);return _Kf;},_aw],_Ke[2]];},_Kh=function(_Ki,_Kj,_){return _Kb(_Kj,_);},_Kk=function(_Kl,_Km){return E(E(_Km)[2])==0?false:true;},_Kn=[0,_3C,_aw],_Ko=function(_Kp,_){return [0,_Kn,_Kp];},_Kq=function(_Kr,_Ks){return E(E(_Ks)[2])==0?true:false;},_Kt=function(_Ku,_Kv,_Kw,_){return _7C(function(_){var _=putMVar(_Kv,_Ku);return _A;},_Kw,_);},_Kx=[0,_7v,_7u],_Ky=function(_){return _Kx;},_Kz=function(_){return _Ky();},_KA=function(_){var _KB=E(_7B)[1],_KC=takeMVar(_KB),_KD=jsCatch(_Kz,function(_1w,_){return _Kt(_KC,_KB,_1w,_);}),_=putMVar(_KB,_KD);return _A;},_KE=[0,_3C,_ae],_KF=[0,_7v,_7u],_KG=function(_){return _KF;},_KH=function(_KI,_){var _KJ=0;if(!E(_KJ)){var _KK=_KA();return [0,[0,_3C,[1,_KK]],new T(function(){var _KL=E(_KI);return [0,_KL[1],_KL[2],_KL[3],_KL[4],_6D,_KL[6]];})];}else{var _KM=E(_7B)[1],_KN=takeMVar(_KM),_KO=jsCatch(_KG,function(_1w,_){return _Kt(_KN,_KM,_1w,_);}),_=putMVar(_KM,_KO);return [0,_KE,new T(function(){var _KP=E(_KI);return [0,_KP[1],_KP[2],_KP[3],_KP[4],_6D,_KP[6]];})];}},_KQ=function(_KR){return _6A(_6E(_Kq,_KR))<=0?E(_Ko):function(_al,_am){return _54(_KH,function(_KS,_gE,_){return (function(_gE,_){return _54(_av,function(_KT){return function(_gE,_){return _54(function(_KU,_){return A(new T(function(){var _KV=A(_ky,[new T(function(){return A(_6t,[_iT,0,new T(function(){return _6E(_Kk,_KR);}),_a]);})]);return function(_KW,_){var _KX=A(_KV,[_]);return [0,[0,_3C,[1,_KX]],_KW];};}),[new T(function(){var _KY=E(_KU);return [0,_KY[1],_KY[2],_KY[3],_KY[4],_6D,_KY[6]];}),_]);},_Kh,_gE,_);};},_gE,_);})(_gE,_);},_al,_am);};},_En=function(_gE,_){return _54(_K3,_KQ,_gE,_);},_KZ=unCStr("info"),_L0=unCStr(" item left"),_L1=[0,49],_L2=[1,_L1,_a],_L3=unCStr("strong"),_L4=function(_L5,_L6){var _L7=new T(function(){return A(_L5,[_L6]);});return function(_L8,_){var _L9=jsCreateElem(toJSStr(E(_L3))),_La=jsAppendChild(_L9,E(_L8)[1]),_Lb=[0,_L9],_Lc=A(_L7,[_Lb,_]);return _Lb;};},_Ld=new T(function(){return _L4(_6N,_L2);}),_Le=function(_Lf,_){var _Lg=A(_Ld,[_Lf,_]),_Lh=_6N(_L0,_Lf,_);return _Lf;},_Li=unCStr(" items left"),_Lj=function(_Lk){return function(_Ll,_){return [0,[0,new T(function(){var _Lm=E(E(_Lk)[1]);if(_Lm==1){return E(_Le);}else{var _Ln=new T(function(){return _L4(_6N,new T(function(){return _4k(0,_Lm,_a);}));});return function(_Lo,_){var _Lp=A(_Ln,[_Lo,_]),_Lq=_6N(_Li,_Lo,_);return _Lo;};}}),_ae],_Ll];};},_Lr=function(_Ls){var _Lt=new T(function(){return [0,_6A(_6E(_Kk,_Ls))];});return function(_Lu,_){return [0,[0,_3C,[1,_Lt]],_Lu];};},_Lv=function(_gE,_){return _54(_K3,_Lr,_gE,_);},_Lw=function(_gE,_){return _54(_Lv,_Lj,_gE,_);},_Lx=unCStr("selected"),_Ly=function(_gE,_){return _6N(_gu,_gE,_);},_Lz=new T(function(){return _ct(_lt,_ly);}),_LA=function(_LB,_LC,_){var _LD=jsWriteHandle(E(_LB)[1],toJSStr(E(_LC)));return _A;},_LE=[0,10],_LF=[1,_LE,_a],_LG=function(_LH,_LI,_){var _LJ=E(_LH),_LK=jsWriteHandle(_LJ[1],toJSStr(E(_LI)));return _LA(_LJ,_LF,_);},_LL=[0,97],_LM=[1,_LL,_a],_LN=function(_LO,_LP){var _LQ=new T(function(){return A(_LO,[_LP]);});return function(_LR,_){var _LS=jsCreateElem(toJSStr(_LM)),_LT=jsAppendChild(_LS,E(_LR)[1]),_LU=[0,_LS],_LV=A(_LQ,[_LU,_]);return _LU;};},_LW=unCStr("href"),_LX=function(_){var _=0,_LY=newMVar(),_=putMVar(_LY,_b);return [0,_LY];},_LZ=new T(function(){return _2(_LX);}),_M0=new T(function(){return _4p(_1D,_4c,_1H,_1E);}),_M1=new T(function(){return A(_lA,[_cs]);}),_M2=unCStr("EMPTY"),_M3=[1,_in,_a],_M4=new T(function(){return _ip(_M2,_M3);}),_M5=[1,_in,_M4],_M6=function(_M7,_M8,_){var _=putMVar(E(_M7)[1],_M8);return _A;},_M9=function(_Ma,_Mb,_){return _7C(function(_){return _M6(_LZ,_Ma,_);},_Mb,_);},_Mc=function(_){var _Md=E(_LZ)[1],_Me=takeMVar(_Md),_=putMVar(_Md,_Me);return _Me;},_Mf=function(_){var _=0,_Mg=jsMkStdout();return [0,_Mg];},_Mh=new T(function(){return _2(_Mf);}),_Mi=function(_Mj,_Mk,_Ml,_Mm){var _Mn=new T(function(){return _LN(_G7,_Mm);}),_Mo=new T(function(){return unAppCStr("#/",new T(function(){var _Mp=A(_Mk,[_Ml]),_Mq=E(_M1),_Mr=hs_eqWord64(_Mp[1],_Mq[1]);if(!E(_Mr)){return A(_nK,[_Mj,_Ml]);}else{var _Ms=hs_eqWord64(_Mp[2],_Mq[2]);return E(_Ms)==0?A(_nK,[_Mj,_Ml]):E(_Ml);}}));});return function(_Mt,_){var _Mu=A(_M0,[_Mt,_]),_Mv=0,_Mw=function(_,_Mx,_My){var _Mz=new T(function(){return E(E(_Mu)[1]);}),_MA=function(_MB,_){var _MC=A(_Mn,[_MB,_]),_MD=A(_B,[_19,_MC,_LW,_Mo,_]),_ME=E(_MC),_MF=jsSetCB(_ME[1],E(_8v)[1],E([0,function(_MG,_MH,_){return (function(_){var _MI=0;if(!E(_MI)){return (function(_){var _MJ=takeMVar(E(_LZ)[1]),_MK=jsCatch(function(_){return (function(_){return [1,_Mz];})();},function(_1w,_){return _M9(_MJ,_1w,_);});return _M6(_LZ,_MK,_);})();}else{var _ML=takeMVar(E(_LZ)[1]),_MM=jsCatch(function(_){return [1,_Mz];},function(_1w,_){return _M9(_ML,_1w,_);});return _M6(_LZ,_MM,_);}})(_);}])[1]);return _ME;},_MN=E(_Mx);if(!_MN[0]){var _MO=_LG(_Mh,_M5,_);return [0,[0,_MA,_b],_My];}else{if(!_7p(_MN[1],_Mz)){var _MP=_LG(_Mh,_M5,_);return [0,[0,_MA,_b],_My];}else{return [0,[0,_MA,[1,_Ml]],_My];}}};if(!E(_Mv)){var _MQ=_Mc();return _Mw(_,_MQ,new T(function(){return E(E(_Mu)[2]);}));}else{var _MR=E(_LZ)[1],_MS=takeMVar(_MR),_=putMVar(_MR,_MS);return _Mw(_,_MS,new T(function(){return E(E(_Mu)[2]);}));}};},_MT=new T(function(){return _Mi(_iS,_Lz,_kW,_Ly);}),_MU=function(_gE,_){return _6N(_gv,_gE,_);},_MV=new T(function(){return _Mi(_iS,_Lz,_EV,_MU);}),_MW=unCStr("All"),_MX=function(_gE,_){return _6N(_MW,_gE,_);},_MY=new T(function(){return _Mi(_iS,_Lz,_a,_MX);}),_MZ=unCStr("Failure in Data.Map.balanceL"),_N0=new T(function(){return err(_MZ);}),_N1=function(_N2,_N3,_N4,_N5){var _N6=E(_N5);if(!_N6[0]){var _N7=_N6[1],_N8=E(_N4);if(!_N8[0]){var _N9=_N8[1],_Na=_N8[2],_Nb=_N8[3];if(_N9<=(imul(3,_N7)|0)){return [0,(1+_N9|0)+_N7|0,E(E(_N2)),_N3,E(_N8),E(_N6)];}else{var _Nc=E(_N8[4]);if(!_Nc[0]){var _Nd=_Nc[1],_Ne=E(_N8[5]);if(!_Ne[0]){var _Nf=_Ne[1],_Ng=_Ne[2],_Nh=_Ne[3],_Ni=_Ne[4];if(_Nf>=(imul(2,_Nd)|0)){var _Nj=function(_Nk){var _Nl=E(_Ne[5]);return _Nl[0]==0?[0,(1+_N9|0)+_N7|0,E(_Ng),_Nh,E([0,(1+_Nd|0)+_Nk|0,E(_Na),_Nb,E(_Nc),E(_Ni)]),E([0,(1+_N7|0)+_Nl[1]|0,E(E(_N2)),_N3,E(_Nl),E(_N6)])]:[0,(1+_N9|0)+_N7|0,E(_Ng),_Nh,E([0,(1+_Nd|0)+_Nk|0,E(_Na),_Nb,E(_Nc),E(_Ni)]),E([0,1+_N7|0,E(E(_N2)),_N3,E(_9),E(_N6)])];},_Nm=E(_Ni);return _Nm[0]==0?_Nj(_Nm[1]):_Nj(0);}else{return [0,(1+_N9|0)+_N7|0,E(_Na),_Nb,E(_Nc),E([0,(1+_N7|0)+_Nf|0,E(E(_N2)),_N3,E(_Ne),E(_N6)])];}}else{return E(_N0);}}else{return E(_N0);}}}else{return [0,1+_N7|0,E(E(_N2)),_N3,E(_9),E(_N6)];}}else{var _Nn=E(_N4);if(!_Nn[0]){var _No=_Nn[1],_Np=_Nn[2],_Nq=_Nn[3],_Nr=_Nn[5],_Ns=E(_Nn[4]);if(!_Ns[0]){var _Nt=_Ns[1],_Nu=E(_Nr);if(!_Nu[0]){var _Nv=_Nu[1],_Nw=_Nu[2],_Nx=_Nu[3],_Ny=_Nu[4];if(_Nv>=(imul(2,_Nt)|0)){var _Nz=function(_NA){var _NB=E(_Nu[5]);return _NB[0]==0?[0,1+_No|0,E(_Nw),_Nx,E([0,(1+_Nt|0)+_NA|0,E(_Np),_Nq,E(_Ns),E(_Ny)]),E([0,1+_NB[1]|0,E(E(_N2)),_N3,E(_NB),E(_9)])]:[0,1+_No|0,E(_Nw),_Nx,E([0,(1+_Nt|0)+_NA|0,E(_Np),_Nq,E(_Ns),E(_Ny)]),E([0,1,E(E(_N2)),_N3,E(_9),E(_9)])];},_NC=E(_Ny);return _NC[0]==0?_Nz(_NC[1]):_Nz(0);}else{return [0,1+_No|0,E(_Np),_Nq,E(_Ns),E([0,1+_Nv|0,E(E(_N2)),_N3,E(_Nu),E(_9)])];}}else{return [0,3,E(_Np),_Nq,E(_Ns),E([0,1,E(E(_N2)),_N3,E(_9),E(_9)])];}}else{var _ND=E(_Nr);return _ND[0]==0?[0,3,E(_ND[2]),_ND[3],E([0,1,E(_Np),_Nq,E(_9),E(_9)]),E([0,1,E(E(_N2)),_N3,E(_9),E(_9)])]:[0,2,E(E(_N2)),_N3,E(_Nn),E(_9)];}}else{return [0,1,E(E(_N2)),_N3,E(_9),E(_9)];}}},_NE=unCStr("Failure in Data.Map.balanceR"),_NF=new T(function(){return err(_NE);}),_NG=function(_NH,_NI,_NJ,_NK){var _NL=E(_NJ);if(!_NL[0]){var _NM=_NL[1],_NN=E(_NK);if(!_NN[0]){var _NO=_NN[1],_NP=_NN[2],_NQ=_NN[3];if(_NO<=(imul(3,_NM)|0)){return [0,(1+_NM|0)+_NO|0,E(E(_NH)),_NI,E(_NL),E(_NN)];}else{var _NR=E(_NN[4]);if(!_NR[0]){var _NS=_NR[1],_NT=_NR[2],_NU=_NR[3],_NV=_NR[4],_NW=E(_NN[5]);if(!_NW[0]){var _NX=_NW[1];if(_NS>=(imul(2,_NX)|0)){var _NY=function(_NZ){var _O0=E(_NH),_O1=E(_NR[5]);return _O1[0]==0?[0,(1+_NM|0)+_NO|0,E(_NT),_NU,E([0,(1+_NM|0)+_NZ|0,E(_O0),_NI,E(_NL),E(_NV)]),E([0,(1+_NX|0)+_O1[1]|0,E(_NP),_NQ,E(_O1),E(_NW)])]:[0,(1+_NM|0)+_NO|0,E(_NT),_NU,E([0,(1+_NM|0)+_NZ|0,E(_O0),_NI,E(_NL),E(_NV)]),E([0,1+_NX|0,E(_NP),_NQ,E(_9),E(_NW)])];},_O2=E(_NV);return _O2[0]==0?_NY(_O2[1]):_NY(0);}else{return [0,(1+_NM|0)+_NO|0,E(_NP),_NQ,E([0,(1+_NM|0)+_NS|0,E(E(_NH)),_NI,E(_NL),E(_NR)]),E(_NW)];}}else{return E(_NF);}}else{return E(_NF);}}}else{return [0,1+_NM|0,E(E(_NH)),_NI,E(_NL),E(_9)];}}else{var _O3=E(_NK);if(!_O3[0]){var _O4=_O3[1],_O5=_O3[2],_O6=_O3[3],_O7=_O3[5],_O8=E(_O3[4]);if(!_O8[0]){var _O9=_O8[1],_Oa=_O8[2],_Ob=_O8[3],_Oc=_O8[4],_Od=E(_O7);if(!_Od[0]){var _Oe=_Od[1];if(_O9>=(imul(2,_Oe)|0)){var _Of=function(_Og){var _Oh=E(_NH),_Oi=E(_O8[5]);return _Oi[0]==0?[0,1+_O4|0,E(_Oa),_Ob,E([0,1+_Og|0,E(_Oh),_NI,E(_9),E(_Oc)]),E([0,(1+_Oe|0)+_Oi[1]|0,E(_O5),_O6,E(_Oi),E(_Od)])]:[0,1+_O4|0,E(_Oa),_Ob,E([0,1+_Og|0,E(_Oh),_NI,E(_9),E(_Oc)]),E([0,1+_Oe|0,E(_O5),_O6,E(_9),E(_Od)])];},_Oj=E(_Oc);return _Oj[0]==0?_Of(_Oj[1]):_Of(0);}else{return [0,1+_O4|0,E(_O5),_O6,E([0,1+_O9|0,E(E(_NH)),_NI,E(_9),E(_O8)]),E(_Od)];}}else{return [0,3,E(_Oa),_Ob,E([0,1,E(E(_NH)),_NI,E(_9),E(_9)]),E([0,1,E(_O5),_O6,E(_9),E(_9)])];}}else{var _Ok=E(_O7);return _Ok[0]==0?[0,3,E(_O5),_O6,E([0,1,E(E(_NH)),_NI,E(_9),E(_9)]),E(_Ok)]:[0,2,E(E(_NH)),_NI,E(_9),E(_O3)];}}else{return [0,1,E(E(_NH)),_NI,E(_9),E(_9)];}}},_Ol=function(_Om,_On,_Oo){var _Op=E(_Om),_Oq=E(_Oo);if(!_Oq[0]){var _Or=_Oq[2],_Os=_Oq[3],_Ot=_Oq[4],_Ou=_Oq[5];switch(_Fh(_Op,_Or)){case 0:return _N1(_Or,_Os,_Ol(_Op,_On,_Ot),_Ou);case 1:return [0,_Oq[1],E(_Op),_On,E(_Ot),E(_Ou)];default:return _NG(_Or,_Os,_Ot,_Ol(_Op,_On,_Ou));}}else{return [0,1,E(_Op),_On,E(_9),E(_9)];}},_Ov=function(_Ow,_){var _Ox=_54(_K3,_G9,_Ow,_),_Oy=E(_Ox);return [0,[0,function(_Oz,_){var _OA=A(E(_Oy[1])[1],[_Oz,_]);return _Oz;},_aw],_Oy[2]];},_OB=function(_OC,_){return [0,[0,_3C,[1,_OC]],_OC];},_OD=[0,_3C,_aw],_OE=[1,_in,_a],_OF=function(_OG){var _OH=new T(function(){return _OI(_OG);}),_OJ=new T(function(){return _g2(_eq,_er,function(_){return _LG(_Mh,[1,_in,new T(function(){return _ip(_OG,_OE);})],_);});});return function(_al,_am){return _54(_OJ,function(_OK,_gE,_){return (function(_gE,_){return _54(function(_gE,_){return _54(_OB,function(_OL){return function(_OM,_){return [0,_OD,new T(function(){var _ON=E(_OL);return [0,_ON[1],_ON[2],_ON[3],_ON[4],_ON[5],new T(function(){return _Ol(_F3,[0,_OG],_ON[6]);})];})];};},_gE,_);},function(_OO){return function(_gE,_){return _54(_Ov,function(_OP){return E(_OH);},_gE,_);};},_gE,_);})(_gE,_);},_al,_am);};},_OQ=unCStr("unsel"),_OI=function(_OR){return function(_al,_am){return _Df(new T(function(){var _OS=new T(function(){return !_7p(_OR,_kW)?E(_OQ):E(_Lx);}),_OT=new T(function(){return !_7p(_OR,_EV)?E(_OQ):E(_Lx);}),_OU=new T(function(){return !_7p(_OR,_a)?E(_OQ):E(_Lx);});return _8P(function(_OV,_){var _OW=A(_MY,[_OV,_]),_OX=E(_OW),_OY=E(_OX[1]),_OZ=A(_MV,[_OX[2],_]),_P0=E(_OZ),_P1=E(_P0[1]),_P2=A(_MT,[_P0[2],_]),_P3=E(_P2),_P4=E(_P3[1]),_P5=new T(function(){return _EK(_G7,_P4[1]);}),_P6=new T(function(){return _EK(_G7,_P1[1]);}),_P7=new T(function(){return _EK(_G7,_OY[1]);});return [0,[0,function(_P8,_){var _P9=A(_P7,[_P8,_]),_Pa=A(_B,[_19,_P9,_bq,_OU,_]),_Pb=A(_P6,[_P8,_]),_Pc=A(_B,[_19,_Pb,_bq,_OT,_]),_Pd=A(_P5,[_P8,_]),_Pe=A(_B,[_19,_Pd,_bq,_OS,_]);return _P8;},new T(function(){var _Pf=E(_OY[2]);if(!_Pf[0]){var _Pg=E(_P1[2]);return _Pg[0]==0?E(_P4[2]):E(_Pg);}else{return E(_Pf);}})],_P3[2]];},_6L);}),_OF,_al,_am);};},_Ph=new T(function(){return _OI(_a);}),_Pi=function(_gE,_){return _K(_1f,_H,_Lw,_gE,_);},_Pj=function(_Pk){return E(_Pi);},_Pl=function(_gE,_){return _54(_Kb,_Pj,_gE,_);},_Pm=function(_Pn){return E(_Pl);},_Po=function(_Pp,_Pq){var _Pr=E(_Pq);switch(_Pr[0]){case 0:return [0,_Pr[1],_Pr[2],E(_Po(_Pp,_Pr[3])),E(_Po(_Pp,_Pr[4]))];case 1:return [1,_Pr[1],new T(function(){return A(_Pp,[_Pr[2]]);})];default:return [2];}},_Ps=function(_Pt,_Pu,_Pv){var _Pw=E(_Pv);switch(_Pw[0]){case 0:var _Px=_Pw[1],_Py=_Pw[2],_Pz=_Pw[3],_PA=_Pw[4],_PB=_Pu>>>0,_PC=_Py>>>0;if(((_PB&((_PC-1>>>0^4.294967295e9)>>>0^_PC)>>>0)>>>0&4.294967295e9)==_Px){return (_PB&_PC)>>>0!=0?[0,_Px,_Py,E(_Pz),E(_Ps(_Pt,_Pu,_PA))]:[0,_Px,_Py,E(_Ps(_Pt,_Pu,_Pz)),E(_PA)];}else{var _PD=E(_Pt);if(_PD[0]==2){return E(_Pw);}else{var _PE=(_PB^_Px>>>0)>>>0,_PF=(_PE|_PE>>>1)>>>0,_PG=(_PF|_PF>>>2)>>>0,_PH=(_PG|_PG>>>4)>>>0,_PI=(_PH|_PH>>>8)>>>0,_PJ=(_PI|_PI>>>16)>>>0,_PK=(_PJ^_PJ>>>1)>>>0&4.294967295e9,_PL=_PK>>>0,_PM=(_PB&((_PL-1>>>0^4.294967295e9)>>>0^_PL)>>>0)>>>0&4.294967295e9;return (_PB&_PL)>>>0!=0?[0,_PM,_PK,E(_Pw),E(_PD)]:[0,_PM,_PK,E(_PD),E(_Pw)];}}break;case 1:var _PN=_Pw[1];if(_Pu!=_PN){var _PO=E(_Pt);if(_PO[0]==2){return E(_Pw);}else{var _PP=_Pu>>>0,_PQ=(_PP^_PN>>>0)>>>0,_PR=(_PQ|_PQ>>>1)>>>0,_PS=(_PR|_PR>>>2)>>>0,_PT=(_PS|_PS>>>4)>>>0,_PU=(_PT|_PT>>>8)>>>0,_PV=(_PU|_PU>>>16)>>>0,_PW=(_PV^_PV>>>1)>>>0&4.294967295e9,_PX=_PW>>>0,_PY=(_PP&((_PX-1>>>0^4.294967295e9)>>>0^_PX)>>>0)>>>0&4.294967295e9;return (_PP&_PX)>>>0!=0?[0,_PY,_PW,E(_Pw),E(_PO)]:[0,_PY,_PW,E(_PO),E(_Pw)];}}else{return E(_Pt);}break;default:return E(_Pt);}},_PZ=function(_Q0,_Q1,_Q2){var _Q3=E(_Q2);switch(_Q3[0]){case 0:var _Q4=_Q3[1],_Q5=_Q3[2],_Q6=_Q3[3],_Q7=_Q3[4],_Q8=_Q1>>>0,_Q9=_Q5>>>0;if(((_Q8&((_Q9-1>>>0^4.294967295e9)>>>0^_Q9)>>>0)>>>0&4.294967295e9)==_Q4){return (_Q8&_Q9)>>>0!=0?[0,_Q4,_Q5,E(_Q6),E(_PZ(_Q0,_Q1,_Q7))]:[0,_Q4,_Q5,E(_PZ(_Q0,_Q1,_Q6)),E(_Q7)];}else{var _Qa=E(_Q0);if(_Qa[0]==2){return E(_Q3);}else{var _Qb=_Q4>>>0,_Qc=(_Qb^_Q8)>>>0,_Qd=(_Qc|_Qc>>>1)>>>0,_Qe=(_Qd|_Qd>>>2)>>>0,_Qf=(_Qe|_Qe>>>4)>>>0,_Qg=(_Qf|_Qf>>>8)>>>0,_Qh=(_Qg|_Qg>>>16)>>>0,_Qi=(_Qh^_Qh>>>1)>>>0&4.294967295e9,_Qj=_Qi>>>0,_Qk=(_Qb&((_Qj-1>>>0^4.294967295e9)>>>0^_Qj)>>>0)>>>0&4.294967295e9;return (_Qb&_Qj)>>>0!=0?[0,_Qk,_Qi,E(_Qa),E(_Q3)]:[0,_Qk,_Qi,E(_Q3),E(_Qa)];}}break;case 1:var _Ql=_Q3[1];if(_Ql!=_Q1){var _Qm=E(_Q0);if(_Qm[0]==2){return E(_Q3);}else{var _Qn=_Ql>>>0,_Qo=(_Qn^_Q1>>>0)>>>0,_Qp=(_Qo|_Qo>>>1)>>>0,_Qq=(_Qp|_Qp>>>2)>>>0,_Qr=(_Qq|_Qq>>>4)>>>0,_Qs=(_Qr|_Qr>>>8)>>>0,_Qt=(_Qs|_Qs>>>16)>>>0,_Qu=(_Qt^_Qt>>>1)>>>0&4.294967295e9,_Qv=_Qu>>>0,_Qw=(_Qn&((_Qv-1>>>0^4.294967295e9)>>>0^_Qv)>>>0)>>>0&4.294967295e9;return (_Qn&_Qv)>>>0!=0?[0,_Qw,_Qu,E(_Qm),E(_Q3)]:[0,_Qw,_Qu,E(_Q3),E(_Qm)];}}else{return E(_Q3);}break;default:return E(_Q0);}},_Qx=function(_Qy,_Qz,_QA,_QB,_QC){var _QD=E(_QC);switch(_QD[0]){case 0:var _QE=_QD[1],_QF=_QD[2],_QG=_QD[3],_QH=_QD[4],_QI=_Qz>>>0,_QJ=_QF>>>0;if(_QI<=_QJ){if(_QJ<=_QI){if(_Qy!=_QE){var _QK=_Qy>>>0,_QL=(_QK^_QE>>>0)>>>0,_QM=(_QL|_QL>>>1)>>>0,_QN=(_QM|_QM>>>2)>>>0,_QO=(_QN|_QN>>>4)>>>0,_QP=(_QO|_QO>>>8)>>>0,_QQ=(_QP|_QP>>>16)>>>0,_QR=(_QQ^_QQ>>>1)>>>0&4.294967295e9,_QS=_QR>>>0,_QT=(_QK&((_QS-1>>>0^4.294967295e9)>>>0^_QS)>>>0)>>>0&4.294967295e9;return (_QK&_QS)>>>0!=0?[0,_QT,_QR,E(_QD),E([0,_Qy,_Qz,E(_QA),E(_QB)])]:[0,_QT,_QR,E([0,_Qy,_Qz,E(_QA),E(_QB)]),E(_QD)];}else{return [0,_Qy,_Qz,E(_QU(_QA,_QG)),E(_QU(_QB,_QH))];}}else{var _QV=_Qy>>>0;if(((_QV&((_QJ-1>>>0^4.294967295e9)>>>0^_QJ)>>>0)>>>0&4.294967295e9)==_QE){return (_QV&_QJ)>>>0!=0?[0,_QE,_QF,E(_QG),E(_Qx(_Qy,_Qz,_QA,_QB,_QH))]:[0,_QE,_QF,E(_Qx(_Qy,_Qz,_QA,_QB,_QG)),E(_QH)];}else{var _QW=(_QV^_QE>>>0)>>>0,_QX=(_QW|_QW>>>1)>>>0,_QY=(_QX|_QX>>>2)>>>0,_QZ=(_QY|_QY>>>4)>>>0,_R0=(_QZ|_QZ>>>8)>>>0,_R1=(_R0|_R0>>>16)>>>0,_R2=(_R1^_R1>>>1)>>>0&4.294967295e9,_R3=_R2>>>0,_R4=(_QV&((_R3-1>>>0^4.294967295e9)>>>0^_R3)>>>0)>>>0&4.294967295e9;return (_QV&_R3)>>>0!=0?[0,_R4,_R2,E(_QD),E([0,_Qy,_Qz,E(_QA),E(_QB)])]:[0,_R4,_R2,E([0,_Qy,_Qz,E(_QA),E(_QB)]),E(_QD)];}}}else{var _R5=_QE>>>0;if(((_R5&((_QI-1>>>0^4.294967295e9)>>>0^_QI)>>>0)>>>0&4.294967295e9)==_Qy){return (_R5&_QI)>>>0!=0?[0,_Qy,_Qz,E(_QA),E(_R6(_QB,_QE,_QF,_QG,_QH))]:[0,_Qy,_Qz,E(_R6(_QA,_QE,_QF,_QG,_QH)),E(_QB)];}else{var _R7=_Qy>>>0,_R8=(_R7^_R5)>>>0,_R9=(_R8|_R8>>>1)>>>0,_Ra=(_R9|_R9>>>2)>>>0,_Rb=(_Ra|_Ra>>>4)>>>0,_Rc=(_Rb|_Rb>>>8)>>>0,_Rd=(_Rc|_Rc>>>16)>>>0,_Re=(_Rd^_Rd>>>1)>>>0&4.294967295e9,_Rf=_Re>>>0,_Rg=(_R7&((_Rf-1>>>0^4.294967295e9)>>>0^_Rf)>>>0)>>>0&4.294967295e9;return (_R7&_Rf)>>>0!=0?[0,_Rg,_Re,E(_QD),E([0,_Qy,_Qz,E(_QA),E(_QB)])]:[0,_Rg,_Re,E([0,_Qy,_Qz,E(_QA),E(_QB)]),E(_QD)];}}break;case 1:return _PZ(_QD,_QD[1],[0,_Qy,_Qz,E(_QA),E(_QB)]);default:return [0,_Qy,_Qz,E(_QA),E(_QB)];}},_R6=function(_Rh,_Ri,_Rj,_Rk,_Rl){var _Rm=E(_Rh);switch(_Rm[0]){case 0:var _Rn=_Rm[1],_Ro=_Rm[2],_Rp=_Rm[3],_Rq=_Rm[4],_Rr=_Ro>>>0,_Rs=_Rj>>>0;if(_Rr<=_Rs){if(_Rs<=_Rr){if(_Rn!=_Ri){var _Rt=_Rn>>>0,_Ru=(_Rt^_Ri>>>0)>>>0,_Rv=(_Ru|_Ru>>>1)>>>0,_Rw=(_Rv|_Rv>>>2)>>>0,_Rx=(_Rw|_Rw>>>4)>>>0,_Ry=(_Rx|_Rx>>>8)>>>0,_Rz=(_Ry|_Ry>>>16)>>>0,_RA=(_Rz^_Rz>>>1)>>>0&4.294967295e9,_RB=_RA>>>0,_RC=(_Rt&((_RB-1>>>0^4.294967295e9)>>>0^_RB)>>>0)>>>0&4.294967295e9;return (_Rt&_RB)>>>0!=0?[0,_RC,_RA,E([0,_Ri,_Rj,E(_Rk),E(_Rl)]),E(_Rm)]:[0,_RC,_RA,E(_Rm),E([0,_Ri,_Rj,E(_Rk),E(_Rl)])];}else{return [0,_Rn,_Ro,E(_QU(_Rp,_Rk)),E(_QU(_Rq,_Rl))];}}else{var _RD=_Rn>>>0;if(((_RD&((_Rs-1>>>0^4.294967295e9)>>>0^_Rs)>>>0)>>>0&4.294967295e9)==_Ri){return (_RD&_Rs)>>>0!=0?[0,_Ri,_Rj,E(_Rk),E(_Qx(_Rn,_Ro,_Rp,_Rq,_Rl))]:[0,_Ri,_Rj,E(_Qx(_Rn,_Ro,_Rp,_Rq,_Rk)),E(_Rl)];}else{var _RE=(_RD^_Ri>>>0)>>>0,_RF=(_RE|_RE>>>1)>>>0,_RG=(_RF|_RF>>>2)>>>0,_RH=(_RG|_RG>>>4)>>>0,_RI=(_RH|_RH>>>8)>>>0,_RJ=(_RI|_RI>>>16)>>>0,_RK=(_RJ^_RJ>>>1)>>>0&4.294967295e9,_RL=_RK>>>0,_RM=(_RD&((_RL-1>>>0^4.294967295e9)>>>0^_RL)>>>0)>>>0&4.294967295e9;return (_RD&_RL)>>>0!=0?[0,_RM,_RK,E([0,_Ri,_Rj,E(_Rk),E(_Rl)]),E(_Rm)]:[0,_RM,_RK,E(_Rm),E([0,_Ri,_Rj,E(_Rk),E(_Rl)])];}}}else{var _RN=_Ri>>>0;if(((_RN&((_Rr-1>>>0^4.294967295e9)>>>0^_Rr)>>>0)>>>0&4.294967295e9)==_Rn){return (_RN&_Rr)>>>0!=0?[0,_Rn,_Ro,E(_Rp),E(_R6(_Rq,_Ri,_Rj,_Rk,_Rl))]:[0,_Rn,_Ro,E(_R6(_Rp,_Ri,_Rj,_Rk,_Rl)),E(_Rq)];}else{var _RO=_Rn>>>0,_RP=(_RO^_RN)>>>0,_RQ=(_RP|_RP>>>1)>>>0,_RR=(_RQ|_RQ>>>2)>>>0,_RS=(_RR|_RR>>>4)>>>0,_RT=(_RS|_RS>>>8)>>>0,_RU=(_RT|_RT>>>16)>>>0,_RV=(_RU^_RU>>>1)>>>0&4.294967295e9,_RW=_RV>>>0,_RX=(_RO&((_RW-1>>>0^4.294967295e9)>>>0^_RW)>>>0)>>>0&4.294967295e9;return (_RO&_RW)>>>0!=0?[0,_RX,_RV,E([0,_Ri,_Rj,E(_Rk),E(_Rl)]),E(_Rm)]:[0,_RX,_RV,E(_Rm),E([0,_Ri,_Rj,E(_Rk),E(_Rl)])];}}break;case 1:return _Ps(_Rm,_Rm[1],[0,_Ri,_Rj,E(_Rk),E(_Rl)]);default:return [0,_Ri,_Rj,E(_Rk),E(_Rl)];}},_QU=function(_RY,_RZ){var _S0=E(_RY);switch(_S0[0]){case 0:var _S1=_S0[1],_S2=_S0[2],_S3=_S0[3],_S4=_S0[4],_S5=E(_RZ);switch(_S5[0]){case 0:var _S6=_S5[1],_S7=_S5[2],_S8=_S5[3],_S9=_S5[4],_Sa=_S2>>>0,_Sb=_S7>>>0;if(_Sa<=_Sb){if(_Sb<=_Sa){if(_S1!=_S6){var _Sc=_S1>>>0,_Sd=(_Sc^_S6>>>0)>>>0,_Se=(_Sd|_Sd>>>1)>>>0,_Sf=(_Se|_Se>>>2)>>>0,_Sg=(_Sf|_Sf>>>4)>>>0,_Sh=(_Sg|_Sg>>>8)>>>0,_Si=(_Sh|_Sh>>>16)>>>0,_Sj=(_Si^_Si>>>1)>>>0&4.294967295e9,_Sk=_Sj>>>0,_Sl=(_Sc&((_Sk-1>>>0^4.294967295e9)>>>0^_Sk)>>>0)>>>0&4.294967295e9;return (_Sc&_Sk)>>>0!=0?[0,_Sl,_Sj,E(_S5),E(_S0)]:[0,_Sl,_Sj,E(_S0),E(_S5)];}else{return [0,_S1,_S2,E(_QU(_S3,_S8)),E(_QU(_S4,_S9))];}}else{var _Sm=_S1>>>0;if(((_Sm&((_Sb-1>>>0^4.294967295e9)>>>0^_Sb)>>>0)>>>0&4.294967295e9)==_S6){return (_Sm&_Sb)>>>0!=0?[0,_S6,_S7,E(_S8),E(_Qx(_S1,_S2,_S3,_S4,_S9))]:[0,_S6,_S7,E(_Qx(_S1,_S2,_S3,_S4,_S8)),E(_S9)];}else{var _Sn=(_Sm^_S6>>>0)>>>0,_So=(_Sn|_Sn>>>1)>>>0,_Sp=(_So|_So>>>2)>>>0,_Sq=(_Sp|_Sp>>>4)>>>0,_Sr=(_Sq|_Sq>>>8)>>>0,_Ss=(_Sr|_Sr>>>16)>>>0,_St=(_Ss^_Ss>>>1)>>>0&4.294967295e9,_Su=_St>>>0,_Sv=(_Sm&((_Su-1>>>0^4.294967295e9)>>>0^_Su)>>>0)>>>0&4.294967295e9;return (_Sm&_Su)>>>0!=0?[0,_Sv,_St,E(_S5),E(_S0)]:[0,_Sv,_St,E(_S0),E(_S5)];}}}else{var _Sw=_S6>>>0;if(((_Sw&((_Sa-1>>>0^4.294967295e9)>>>0^_Sa)>>>0)>>>0&4.294967295e9)==_S1){return (_Sw&_Sa)>>>0!=0?[0,_S1,_S2,E(_S3),E(_R6(_S4,_S6,_S7,_S8,_S9))]:[0,_S1,_S2,E(_R6(_S3,_S6,_S7,_S8,_S9)),E(_S4)];}else{var _Sx=_S1>>>0,_Sy=(_Sx^_Sw)>>>0,_Sz=(_Sy|_Sy>>>1)>>>0,_SA=(_Sz|_Sz>>>2)>>>0,_SB=(_SA|_SA>>>4)>>>0,_SC=(_SB|_SB>>>8)>>>0,_SD=(_SC|_SC>>>16)>>>0,_SE=(_SD^_SD>>>1)>>>0&4.294967295e9,_SF=_SE>>>0,_SG=(_Sx&((_SF-1>>>0^4.294967295e9)>>>0^_SF)>>>0)>>>0&4.294967295e9;return (_Sx&_SF)>>>0!=0?[0,_SG,_SE,E(_S5),E(_S0)]:[0,_SG,_SE,E(_S0),E(_S5)];}}break;case 1:return _PZ(_S5,_S5[1],_S0);default:return E(_S0);}break;case 1:return _Ps(_S0,_S0[1],_RZ);default:return E(_RZ);}},_SH=function(_SI,_SJ,_){var _SK=new T(function(){return E(_SI)[0]==0?1:0;});return _54(_K3,function(_SL){return function(_al,_am){return _54(function(_gE,_){return _FQ(_SL,_gE,_);},function(_SM){var _SN=new T(function(){return _g2(_eq,_er,new T(function(){return A(_ky,[new T(function(){return A(_6t,[_iT,0,new T(function(){return _QU(_Po(function(_SO){return [0,E(_SO)[1],_SK];},_SM),_SL);}),_a]);})]);}));});return function(_al,_am){return _54(_SN,_Pm,_al,_am);};},_al,_am);};},_SJ,_);},_SP=function(_SQ,_SR,_){return _SH(E(_SQ)[1],_SR,_);},_SS=unCStr("toggle-all"),_ST=new T(function(){return A(_fw,[_0,_G1]);}),_SU=new T(function(){return _8P(_ST,_6L);}),_SV=function(_SW,_){var _SX=A(_SU,[_SW,_]),_SY=E(_SX),_SZ=E(_SY[1]);return [0,[0,function(_T0,_){var _T1=A(_SZ[1],[_T0,_]),_T2=A(_B,[_19,_T1,_bq,_SS,_]);return _T1;},_SZ[2]],_SY[2]];},_T3=function(_gE,_){return _54(_SV,_SP,_gE,_);},_T4=unCStr("h1"),_T5=function(_T6,_T7){var _T8=new T(function(){return A(_T6,[_T7]);});return function(_T9,_){var _Ta=jsCreateElem(toJSStr(E(_T4))),_Tb=jsAppendChild(_Ta,E(_T9)[1]),_Tc=[0,_Ta],_Td=A(_T8,[_Tc,_]);return _Tc;};},_Te=unCStr("todos"),_Tf=new T(function(){return _T5(_6N,_Te);}),_Tg=function(_Th,_Ti){while(1){var _Tj=E(_Th);if(!_Tj[0]){return E(_Ti)[0]==0?true:false;}else{var _Tk=E(_Ti);if(!_Tk[0]){return false;}else{if(E(_Tj[1])[1]!=E(_Tk[1])[1]){return false;}else{_Th=_Tj[2];_Ti=_Tk[2];continue;}}}}},_Tl=[1,_EW],_Tm=function(_Tn,_){var _To=A(_FL,[_Tn,_]);return [0,[0,_EY,new T(function(){var _Tp=E(E(_To)[1]);return _Tp[0]==0?E(_Tl):E(_Tp);})],new T(function(){return E(E(_To)[2]);})];},_Tq=function(_Tr,_Ts,_Tt,_Tu){return A(_Tr,[new T(function(){return function(_){var _Tv=jsSet(E(_Ts)[1],toJSStr(E(_Tt)),toJSStr(E(_Tu)));return _A;};})]);},_Tw=unCStr("text"),_Tx=unCStr("value"),_Ty=new T(function(){return _ct(_lt,_ly);}),_Tz=new T(function(){return A(_Ty,[_cs]);}),_TA=new T(function(){return A(_Ty,[_cs]);}),_TB=function(_TC,_TD){var _TE=_Ju(_pr(A(E(_TC)[3],[_Bz,_Jp]),_TD));return _TE[0]==0?err(_H1):E(_TE[2])[0]==0?E(_TE[1]):err(_GZ);},_TF=function(_TG,_TH,_TI,_TJ){var _TK=new T(function(){return _nK(_TH);}),_TL=new T(function(){return _oo(_eq,_er,_TI,_TH,_TG);});return [0,function(_TM){return A(_TL,[[1,_TJ],_Tw,_TM]);},function(_TN,_){var _TO=E(_TJ),_TP=jsFind(toJSStr(_TO)),_TQ=E(_TP);return _TQ[0]==0?_4D(_TO):A(_Tq,[_19,_TQ[1],_Tx,new T(function(){var _TR=A(_TI,[_TN]),_TS=E(_Tz),_TT=hs_eqWord64(_TR[1],_TS[1]);if(!E(_TT)){return A(_TK,[_TN]);}else{var _TU=hs_eqWord64(_TR[2],_TS[2]);return E(_TU)==0?A(_TK,[_TN]):E(_TN);}}),_]);},function(_){var _TV=E(_TJ),_TW=jsFind(toJSStr(_TV)),_TX=E(_TW);if(!_TX[0]){return _4D(_TV);}else{var _TY=_eL(E(_TX[1])[1],_Tx,_);return new T(function(){var _TZ=A(_Ty,[_TY]),_U0=E(_TA),_U1=hs_eqWord64(_TZ[1],_U0[1]);if(!E(_U1)){return _TB(_TG,_TY);}else{var _U2=hs_eqWord64(_TZ[2],_U0[2]);return E(_U2)==0?_TB(_TG,_TY):E(_TY);}});}}];},_U3=unCStr("new-todo"),_U4=new T(function(){var _U5=_TF(_Jn,_iS,_Lz,_U3);return [0,_U5[1],_U5[2],_U5[3]];}),_U6=new T(function(){var _U7=A(E(_U4)[2],[_a]);return function(_U8,_){var _U9=A(_U7,[_]);return [0,[0,_3C,[1,_U9]],_U8];};}),_Ua=function(_Ub,_){return A(_U6,[new T(function(){var _Uc=E(_Ub);return [0,_Uc[1],_Uc[2],_Uc[3],_Uc[4],_6D,_Uc[6]];}),_]);},_Ud=[0,_3C,_aw],_Ue=function(_Uf,_){return [0,_Ud,_Uf];},_Ug=[0,_3C,_aw],_Uh=function(_Ui,_Uj,_){return [0,_Ug,_Uj];},_Uk=[0,_3C,_aw],_Ul=function(_Um,_){return [0,_Uk,_Um];},_Un=[0,_3C,_aw],_Uo=function(_Up,_){return [0,_Un,_Up];},_Uq=[0,_3C,_aw],_Ur=function(_Us,_){return [0,_Uq,_Us];},_Ut=[0,_3C,_aw],_Uu=function(_Uv,_){return [0,_Ut,_Uv];},_Uw=function(_Ux,_Uy,_){return _54(_kD,function(_Uz){var _UA=E(E(_Uz)[2]);switch(_UA[0]){case 0:return E(_Ul);case 1:return E(_Uo);case 2:return E(_Ur);default:return E(E(_UA[1])[1])==13?function(_gE,_){return _54(_Ua,function(_UB){return function(_gE,_){return _54(_K3,function(_UC){var _UD=new T(function(){return [0,_6A(_UC)];}),_UE=new T(function(){return _aO(E(_UD)[1],[0,_Ux,_bj],_UC);}),_UF=new T(function(){return _E5(_bk,_Ux);}),_UG=new T(function(){return _kz(new T(function(){return _aO(E(_UD)[1],[0,_Ux,_bk],_UE);}));}),_UH=new T(function(){return _E5(_bj,_Ux);}),_UI=new T(function(){return _kz(new T(function(){return _aO(E(_UD)[1],[0,_Ux,_bj],_UE);}));}),_UJ=function(_gE,_){return _54(function(_gE,_){return _54(_UI,_Eo,_gE,_);},function(_UK){return E(_UH);},_gE,_);},_UL=new T(function(){return _8P(new T(function(){return A(_fw,[_0,_ET]);}),_6L);}),_UM=new T(function(){return _g2(_eq,_er,new T(function(){return A(_ky,[new T(function(){return A(_6t,[_iT,0,_UE,_a]);})]);}));});return function(_al,_am){return _54(_UM,function(_UN,_gE,_){return (function(_gE,_){return _54(_Pi,function(_UO){return function(_gE,_){return _54(_Tm,function(_UP){var _UQ=new T(function(){return !_Tg(E(_UP)[1],_kW)?function(_gE,_){return _K(_EU,_I,function(_gE,_){return _Df(function(_UR,_){var _US=_54(function(_UT,_){var _UU=A(_UL,[_UT,_]),_UV=E(_UU),_UW=E(_UV[1]);return [0,[0,function(_UX,_){var _UY=A(_UW[1],[_UX,_]),_UZ=A(_B,[_19,_UY,_bq,_G1,_]);return _UY;},_UW[2]],_UV[2]];},function(_V0){return (function(_V1){var _V2=E(_V1);return _V2[0]==0?E(_UJ):!_7p(_V2[1],_ET)?E(_UJ):E(_V2[2])[0]==0?function(_gE,_){return _54(function(_gE,_){return _54(_UG,_Eq,_gE,_);},function(_V3){return E(_UF);},_gE,_);}:E(_UJ);})(E(_V0)[1]);},_UR,_),_V4=E(_US),_V5=E(_V4[1]),_V6=_54(_fz,_bm,_V4[2],_),_V7=E(_V6);return [0,[0,new T(function(){return _EK(_G7,function(_V8,_){var _V9=A(_V5[1],[_V8,_]),_Va=A(E(_V7[1])[1],[_V8,_]);return _V8;});}),_V5[2]],_V7[2]];},function(_Vb,_Vc,_){return A(new T(function(){var _Vd=A(_ky,[new T(function(){return A(_6t,[_iT,0,new T(function(){return _aA(E(_UD)[1],_UE);}),_a]);})]);return function(_Ve,_){var _Vf=A(_Vd,[_]);return [0,[0,_3C,[1,_Vf]],_Ve];};}),[new T(function(){var _Vg=E(_Vc);return [0,_Vg[1],_Vg[2],_Vg[3],_Vg[4],_6D,_Vg[6]];}),_]);},_gE,_);},_gE,_);}:E(_Ue);});return function(_al,_am){return _54(_UQ,_Uh,_al,_am);};},_gE,_);};},_gE,_);})(_gE,_);},_al,_am);};},_gE,_);};},_gE,_);}:E(_Uu);}},_Uy,_);},_Vh=unCStr("autofocus"),_Vi=unCStr("What needs to be done?"),_Vj=unCStr("placeholder"),_Vk=new T(function(){return A(E(_U4)[1],[_b]);}),_Vl=new T(function(){return _8P(_Vk,_kC);}),_Vm=function(_Vn,_){var _Vo=A(_Vl,[_Vn,_]),_Vp=E(_Vo),_Vq=E(_Vp[1]);return [0,[0,function(_Vr,_){var _Vs=A(_Vq[1],[_Vr,_]),_Vt=A(_B,[_19,_Vs,_Vj,_Vi,_]),_Vu=A(_B,[_19,_Vs,_Vh,_a,_]);return _Vs;},_Vq[2]],_Vp[2]];},_Vv=function(_Vw,_){var _Vx=_54(_Vm,_Uw,_Vw,_),_Vy=E(_Vx),_Vz=E(_Vy[1]);return [0,[0,function(_VA,_){var _VB=A(_Tf,[_VA,_]),_VC=A(_Vz[1],[_VA,_]);return _VA;},_Vz[2]],_Vy[2]];},_VD=unCStr("footer"),_VE=function(_VF,_VG){var _VH=new T(function(){return A(_VF,[_VG]);});return function(_VI,_){var _VJ=jsCreateElem(toJSStr(E(_VD))),_VK=jsAppendChild(_VJ,E(_VI)[1]),_VL=[0,_VJ],_VM=A(_VH,[_VL,_]);return _VL;};},_VN=unCStr("Double-click to edit a todo"),_VO=[0,112],_VP=[1,_VO,_a],_VQ=function(_VR,_VS){var _VT=new T(function(){return A(_VR,[_VS]);});return function(_VU,_){var _VV=jsCreateElem(toJSStr(_VP)),_VW=jsAppendChild(_VV,E(_VU)[1]),_VX=[0,_VV],_VY=A(_VT,[_VX,_]);return _VX;};},_VZ=new T(function(){return _VQ(_6N,_VN);}),_W0=unCStr("http://twitter.com/agocorona"),_W1=unCStr("Created by "),_W2=unCStr("Alberto G. Corona"),_W3=new T(function(){return _LN(_6N,_W2);}),_W4=unCStr("http://todomvc.com"),_W5=unCStr("Part of "),_W6=unCStr("TodoMVC"),_W7=new T(function(){return _LN(_6N,_W6);}),_W8=function(_W9,_){var _Wa=_6N(_W5,_W9,_),_Wb=A(_W7,[_W9,_]),_Wc=A(_B,[_19,_Wb,_LW,_W4,_]);return _W9;},_Wd=new T(function(){return _VQ(_G7,_W8);}),_We=function(_Wf,_){var _Wg=_6N(_W1,_Wf,_),_Wh=A(_W3,[_Wf,_]),_Wi=A(_B,[_19,_Wh,_LW,_W0,_]),_Wj=A(_Wd,[_Wf,_]);return _Wf;},_Wk=new T(function(){return _VQ(_G7,_We);}),_Wl=function(_Wm,_){var _Wn=A(_VZ,[_Wm,_]),_Wo=A(_Wk,[_Wm,_]);return _Wm;},_Wp=new T(function(){return _VE(_G7,_Wl);}),_Wq=unCStr("footer"),_Wr=unCStr("ul"),_Ws=function(_Wt,_Wu){var _Wv=new T(function(){return A(_Wt,[_Wu]);});return function(_Ww,_){var _Wx=jsCreateElem(toJSStr(E(_Wr))),_Wy=jsAppendChild(_Wx,E(_Ww)[1]),_Wz=[0,_Wx],_WA=A(_Wv,[_Wz,_]);return _Wz;};},_WB=new T(function(){return _Ws(_G7,_3C);}),_WC=unCStr("span"),_WD=function(_WE,_WF){var _WG=new T(function(){return A(_WE,[_WF]);});return function(_WH,_){var _WI=jsCreateElem(toJSStr(E(_WC))),_WJ=jsAppendChild(_WI,E(_WH)[1]),_WK=[0,_WI],_WL=A(_WG,[_WK,_]);return _WK;};},_WM=new T(function(){return _WD(_G7,_3C);}),_WN=function(_WO,_){var _WP=A(_WM,[_WO,_]),_WQ=A(_B,[_19,_WP,_1b,_1f,_]),_WR=A(_WB,[_WO,_]),_WS=A(_B,[_19,_WR,_1b,_1e,_]),_WT=A(_WM,[_WO,_]),_WU=A(_B,[_19,_WT,_1b,_1d,_]);return _WO;},_WV=new T(function(){return _VE(_G7,_WN);}),_WW=function(_WX,_){var _WY=A(_WB,[_WX,_]),_WZ=A(_B,[_19,_WY,_1b,_EU,_]);return _WY;},_X0=unCStr("section"),_X1=function(_X2,_X3){var _X4=new T(function(){return A(_X2,[_X3]);});return function(_X5,_){var _X6=jsCreateElem(toJSStr(E(_X0))),_X7=jsAppendChild(_X6,E(_X5)[1]),_X8=[0,_X6],_X9=A(_X4,[_X8,_]);return _X8;};},_Xa=new T(function(){return _X1(_G7,_WW);}),_Xb=function(_Xc,_){var _Xd=_1x(_1g,_Xc,_),_Xe=A(_B,[_19,_Xd,_1b,_1g,_]),_Xf=A(_Xa,[_Xc,_]),_Xg=A(_B,[_19,_Xf,_1b,_z,_]),_Xh=A(_WV,[_Xc,_]),_Xi=A(_B,[_19,_Xh,_1b,_Wq,_]);return _Xc;},_Xj=new T(function(){return _X1(_G7,_Xb);}),_Xk=function(_Xl,_){var _Xm=_K(_1g,_H,_Vv,_Xl,_),_Xn=E(_Xm),_Xo=E(_Xn[1]),_Xp=_K(_z,_I,_T3,_Xn[2],_),_Xq=E(_Xp),_Xr=E(_Xq[1]),_Xs=_K(_1e,_H,_Ph,_Xq[2],_),_Xt=E(_Xs),_Xu=E(_Xt[1]),_Xv=_K(_1f,_H,_Lw,_Xt[2],_),_Xw=E(_Xv),_Xx=E(_Xw[1]),_Xy=_K(_1d,_H,_En,_Xw[2],_),_Xz=E(_Xy),_XA=E(_Xz[1]);return [0,[0,function(_XB,_){var _XC=A(_Xj,[_XB,_]),_XD=A(_B,[_19,_XC,_1b,_1c,_]),_XE=A(_Wp,[_XB,_]),_XF=A(_B,[_19,_XE,_1b,_KZ,_]),_XG=A(_Xo[1],[_XB,_]),_XH=A(_Xr[1],[_XB,_]),_XI=A(_Xu[1],[_XB,_]),_XJ=A(_Xx[1],[_XB,_]),_XK=A(_XA[1],[_XB,_]);return _XB;},new T(function(){return E(_Xo[2])[0]==0?[0]:E(_Xr[2])[0]==0?[0]:E(_Xu[2])[0]==0?[0]:E(_Xx[2])[0]==0?[0]:E(_XA[2]);})],_Xz[2]];},_XL=unCStr("idelem"),_XM=function(_){var _XN=E(_XL),_XO=jsFind(toJSStr(_XN)),_XP=E(_XO);return _XP[0]==0?_4D(_XN):_l(_Xk,_XP[1],_);},_XQ=function(_){return _XM(_);};
var hasteMain = function() {A(_XQ, [0]);};window.onload = hasteMain;