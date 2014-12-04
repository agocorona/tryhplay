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
    this.f = f;
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

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
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
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
        if(t.f != __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            t.x = f();
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


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
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
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
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
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
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
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
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

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
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
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
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
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
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
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

function jsQuerySelectorAll(elem, query) {
  var els = [0],
      len, nl, i;

  if (!elem || typeof elem.querySelectorAll !== 'function') {
    return els;
  }

  nl = elem.querySelectorAll(query);
  len = nl.length;

  for (i=len-1; i >= 0; --i) {
    els = [1, [0, nl[i]], els];
  }

  return els;
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
        } else if (obj == null) {
            return [5];
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

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
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
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
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

var _0=function(_1,_2,_){var _3=jsCreateTextNode(toJSStr(E(_1))),_4=_3,_5=jsAppendChild(_4,E(_2)[1]);return [0,_4];},_6=new T(function(){return B(unCStr("This widget sum recursively n numbers. When enters 0, present the result"));}),_7=new T(function(){return B(unCStr("id"));}),_8=0,_9=function(_a,_b,_c,_d){return new F(function(){return A(_a,[function(_){var _e=jsSetAttr(E(_b)[1],toJSStr(E(_c)),toJSStr(E(_d)));return _8;}]);});},_f=function(_g){return E(_g);},_h=function(_i,_j,_k,_){var _l=E(_j),_m=B(A(_i,[_k,_])),_n=_m,_o=B(A(_9,[_f,_n,_l[1],_l[2],_])),_p=_o;return _n;},_q=function(_r,_s){while(1){var _t=(function(_u,_v){var _w=E(_v);if(!_w[0]){return E(_u);}else{_r=function(_x,_){return new F(function(){return _h(_u,_w[1],_x,_);});};_s=_w[2];return null;}})(_r,_s);if(_t!=null){return _t;}}},_y=new T(function(){return B(unCStr("span"));}),_z=function(_A,_B,_){var _C=jsCreateElem(toJSStr(E(_A))),_D=_C,_E=jsAppendChild(_D,E(_B)[1]);return [0,_D];},_F=function(_x,_){return new F(function(){return _z(_y,_x,_);});},_G=false,_H=function(_I){var _J=B(A(_I,[_])),_K=_J;return E(_K);},_L=function(_M){return new F(function(){return _H(function(_){var _=0;return new F(function(){return eval(_M);});});});},_N=new T(function(){return B(_L("(function(e){return e.parentNode;})"));}),_O=[0,0],_P=[0],_Q=function(_R,_){return _P;},_S=function(_){return _P;},_T=[0,_S,_Q],_U=2,_V=[1],_W=[0],_X=[0,_W,_O,_U,_T,_G,_V],_Y=function(_){var _=0,_Z=newMVar(),_10=_Z,_=putMVar(_10,_X);return [0,_10];},_11=new T(function(){return B(_H(_Y));}),_12=function(_13,_14,_){var _15=E(_11)[1],_16=takeMVar(_15),_17=_16,_18=B(A(_13,[_17,_])),_19=_18,_1a=E(_19),_1b=E(_1a[1]),_1c=_1b[1],_1d=_1b[2],_=putMVar(_15,new T(function(){var _1e=E(_1a[2]);return [0,_1e[1],_1e[2],_1e[3],_1e[4],_G,_1e[6]];}));if(!E(E(_17)[5])){var _1f=B(A(_1c,[_14,_])),_1g=_1f;return _1d;}else{var _1h=B(A(_N,[E(E(_14)[1]),_])),_1i=_1h,_1j=B(A(_1c,[[0,_1i],_])),_1k=_1j;return _1d;}},_1l=function(_1m,_1n){var _1o=E(_1m);return _1o[0]==0?E(_1n):[1,_1o[1],new T(function(){return B(_1l(_1o[2],_1n));})];},_1p=new T(function(){return B(unCStr(" could be found!"));}),_1q=function(_1r){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_1l(_1r,_1p));}))));});},_1s=function(_1t,_1u,_){var _1v=E(_1u),_1w=jsFind(toJSStr(_1v)),_1x=_1w,_1y=E(_1x);if(!_1y[0]){return new F(function(){return _1q(_1v);});}else{var _1z=E(_1y[1]),_1A=jsClearChildren(_1z[1]);return new F(function(){return _12(_1t,_1z,_);});}},_1B=[0,112],_1C=function(_1D,_1E){var _1F=jsShowI(_1D),_1G=_1F;return new F(function(){return _1l(fromJSStr(_1G),_1E);});},_1H=[0,41],_1I=[0,40],_1J=function(_1K,_1L,_1M){if(_1L>=0){return new F(function(){return _1C(_1L,_1M);});}else{return _1K<=6?B(_1C(_1L,_1M)):[1,_1I,new T(function(){var _1N=jsShowI(_1L),_1O=_1N;return B(_1l(fromJSStr(_1O),[1,_1H,_1M]));})];}},_1P=function(_1Q){var _1R=new T(function(){return E(E(_1Q)[2]);});return function(_1S,_){return [0,[1,_1B,new T(function(){return B(_1l(B(_1J(0,E(_1R)[1],_W)),new T(function(){return E(E(_1Q)[1]);})));})],new T(function(){var _1T=E(_1Q);return [0,_1T[1],new T(function(){return [0,E(_1R)[1]+1|0];}),_1T[3],_1T[4],_1T[5],_1T[6]];})];};},_1U=function(_1V,_){return _1V;},_1W=function(_1X,_1Y,_1Z,_){var _20=B(A(_1P,[_1Z,_1Z,_])),_21=_20,_22=E(_21),_23=_22[1],_24=E(_22[2]),_25=_24[2],_26=E(_24[4]),_27=B(A(_1X,[[0,_24[1],_25,_24[3],[0,function(_){return new F(function(){return _1s(function(_28,_){var _29=B(A(_1X,[new T(function(){var _2a=E(_28);return [0,_2a[1],_25,_2a[3],_2a[4],_2a[5],_2a[6]];}),_])),_2b=_29;return [0,[0,_1U,E(E(_2b)[1])[2]],_28];},_23,_);});},function(_2c,_){var _2d=B(_1s(new T(function(){return B(A(_1Y,[_2c]));}),_23,_)),_2e=_2d,_2f=E(_2e);return _2f[0]==0?_P:B(A(_26[2],[_2f[1],_]));}],_24[5],_24[6]],_])),_2g=_27,_2h=E(_2g),_2i=_2h[2],_2j=E(_2h[1]),_2k=_2j[1],_2l=new T(function(){return B(_q(_F,[1,[0,_7,_23],_W]));}),_2m=E(_2j[2]);if(!_2m[0]){return [0,[0,function(_2n,_){var _2o=B(A(_2k,[_2n,_])),_2p=_2o,_2q=B(A(_2l,[_2n,_])),_2r=_2q;return _2n;},_P],new T(function(){var _2s=E(_2i);return [0,_2s[1],_2s[2],_2s[3],_26,_2s[5],_2s[6]];})];}else{var _2t=B(A(_1Y,[_2m[1],new T(function(){var _2u=E(_2i);return [0,_2u[1],_2u[2],_2u[3],_26,_2u[5],_2u[6]];}),_])),_2v=_2t,_2w=E(_2v),_2x=E(_2w[1]);return [0,[0,function(_2y,_){var _2z=B(A(_2k,[_2y,_])),_2A=_2z,_2B=B(A(_2l,[_2y,_])),_2C=_2B,_2D=B(A(_2x[1],[_2C,_])),_2E=_2D;return _2y;},_2x[2]],_2w[2]];}},_2F=[0,98],_2G=[1,_2F,_W],_2H=function(_2I,_2J,_2K,_){var _2L=jsCreateElem(toJSStr(_2G)),_2M=_2L,_2N=jsAppendChild(_2M,E(_2K)[1]),_2O=[0,_2M],_2P=B(A(_2I,[_2J,_2O,_])),_2Q=_2P;return _2O;},_2R=new T(function(){return B(unCStr("br"));}),_2S=function(_2T,_){var _2U=jsCreateElem(toJSStr(E(_2R))),_2V=_2U,_2W=jsAppendChild(_2V,E(_2T)[1]);return [0,_2V];},_2X=new T(function(){return B(unCStr("result: "));}),_2Y=[13,_],_2Z=new T(function(){return B(unCStr("text"));}),_30=function(_31,_32,_33,_){var _34=B(_z(_31,_33,_)),_35=_34,_36=B(A(_32,[_35,_])),_37=_36;return _35;},_38=new T(function(){return B(unCStr("()"));}),_39=new T(function(){return B(unCStr("GHC.Tuple"));}),_3a=new T(function(){return B(unCStr("ghc-prim"));}),_3b=new T(function(){var _3c=hs_wordToWord64(2170319554),_3d=_3c,_3e=hs_wordToWord64(26914641),_3f=_3e;return [0,_3d,_3f,[0,_3d,_3f,_3a,_39,_38],_W];}),_3g=function(_3h){return E(_3b);},_3i=new T(function(){return B(unCStr("PerchM"));}),_3j=new T(function(){return B(unCStr("Haste.Perch"));}),_3k=new T(function(){return B(unCStr("haste-perch-0.1.0.5"));}),_3l=new T(function(){var _3m=hs_wordToWord64(3076179652),_3n=_3m,_3o=hs_wordToWord64(696987590),_3p=_3o;return [0,_3n,_3p,[0,_3n,_3p,_3k,_3j,_3i],_W];}),_3q=function(_3r){return E(_3l);},_3s=function(_3t){var _3u=E(_3t);if(!_3u[0]){return [0];}else{var _3v=E(_3u[1]);return [1,[0,_3v[1],_3v[2]],new T(function(){return B(_3s(_3u[2]));})];}},_3w=function(_3x,_3y){var _3z=E(_3x);if(!_3z){return [0,_W,_3y];}else{var _3A=E(_3y);if(!_3A[0]){return [0,_W,_W];}else{var _3B=new T(function(){var _3C=B(_3w(_3z-1|0,_3A[2]));return [0,_3C[1],_3C[2]];});return [0,[1,_3A[1],new T(function(){return E(E(_3B)[1]);})],new T(function(){return E(E(_3B)[2]);})];}}},_3D=[0,120],_3E=[0,48],_3F=function(_3G){var _3H=new T(function(){var _3I=B(_3w(8,new T(function(){var _3J=md5(toJSStr(E(_3G))),_3K=_3J;return fromJSStr(_3K);})));return [0,_3I[1],_3I[2]];}),_3L=parseInt([0,toJSStr([1,_3E,[1,_3D,new T(function(){return E(E(_3H)[1]);})]])]),_3M=_3L,_3N=new T(function(){var _3O=B(_3w(8,new T(function(){return E(E(_3H)[2]);})));return [0,_3O[1],_3O[2]];}),_3P=parseInt([0,toJSStr([1,_3E,[1,_3D,new T(function(){return E(E(_3N)[1]);})]])]),_3Q=_3P,_3R=hs_mkWord64(_3M,_3Q),_3S=_3R,_3T=parseInt([0,toJSStr([1,_3E,[1,_3D,new T(function(){return E(B(_3w(8,new T(function(){return E(E(_3N)[2]);})))[1]);})]])]),_3U=_3T,_3V=hs_mkWord64(_3U,_3U),_3W=_3V;return [0,_3S,_3W];},_3X=function(_3Y){var _3Z=E(_3Y);if(!_3Z[0]){return [0];}else{return new F(function(){return _1l(_3Z[1],new T(function(){return B(_3X(_3Z[2]));}));});}},_40=function(_41,_42){var _43=E(_42);return _43[0]==0?[0]:[1,new T(function(){return B(A(_41,[_43[1]]));}),new T(function(){return B(_40(_41,_43[2]));})];},_44=function(_45,_46){var _47=jsShowI(_45),_48=_47,_49=md5(_48),_4a=_49;return new F(function(){return _1l(fromJSStr(_4a),new T(function(){var _4b=jsShowI(_46),_4c=_4b,_4d=md5(_4c),_4e=_4d;return fromJSStr(_4e);}));});},_4f=function(_4g){var _4h=E(_4g);return new F(function(){return _44(_4h[1],_4h[2]);});},_4i=function(_4j,_4k){return function(_4l){return E(new T(function(){var _4m=B(A(_4j,[_])),_4n=E(_4m[3]),_4o=_4n[1],_4p=_4n[2],_4q=B(_1l(_4m[4],[1,new T(function(){return B(A(_4k,[_]));}),_W]));if(!_4q[0]){var _4r=[0,_4o,_4p,_4n,_W];}else{var _4s=B(_3F(new T(function(){return B(_3X(B(_40(_4f,[1,[0,_4o,_4p],new T(function(){return B(_3s(_4q));})]))));}))),_4r=[0,_4s[1],_4s[2],_4n,_4q];}var _4t=_4r,_4u=_4t;return _4u;}));};},_4v=new T(function(){return B(_4i(_3q,_3g));}),_4w=new T(function(){return B(unCStr("value"));}),_4x=new T(function(){return B(unCStr("onclick"));}),_4y=new T(function(){return B(unCStr("checked"));}),_4z=[0,_4y,_W],_4A=[1,_4z,_W],_4B=new T(function(){return B(unCStr("type"));}),_4C=new T(function(){return B(unCStr("input"));}),_4D=function(_4E,_){return new F(function(){return _z(_4C,_4E,_);});},_4F=function(_4G,_4H,_4I,_4J,_4K){var _4L=new T(function(){var _4M=new T(function(){return B(_q(_4D,[1,[0,_4B,_4H],[1,[0,_7,_4G],[1,[0,_4w,_4I],_W]]]));});return !E(_4J)?E(_4M):B(_q(_4M,_4A));}),_4N=E(_4K);return _4N[0]==0?E(_4L):B(_q(_4L,[1,[0,_4x,_4N[1]],_W]));},_4O=new T(function(){return B(unCStr("href"));}),_4P=[0,97],_4Q=[1,_4P,_W],_4R=function(_4S,_){return new F(function(){return _z(_4Q,_4S,_);});},_4T=function(_4U,_4V){return function(_4W,_){var _4X=B(A(new T(function(){return B(_q(_4R,[1,[0,_4O,_4U],_W]));}),[_4W,_])),_4Y=_4X,_4Z=B(A(_4V,[_4Y,_])),_50=_4Z;return _4Y;};},_51=function(_52){return new F(function(){return _4T(_52,function(_x,_){return new F(function(){return _0(_52,_x,_);});});});},_53=new T(function(){return B(unCStr("option"));}),_54=function(_55,_){return new F(function(){return _z(_53,_55,_);});},_56=new T(function(){return B(unCStr("selected"));}),_57=[0,_56,_W],_58=[1,_57,_W],_59=function(_5a,_5b,_5c){var _5d=new T(function(){return B(_q(_54,[1,[0,_4w,_5a],_W]));});if(!E(_5c)){return function(_5e,_){var _5f=B(A(_5d,[_5e,_])),_5g=_5f,_5h=B(A(_5b,[_5g,_])),_5i=_5h;return _5g;};}else{return new F(function(){return _q(function(_5j,_){var _5k=B(A(_5d,[_5j,_])),_5l=_5k,_5m=B(A(_5b,[_5l,_])),_5n=_5m;return _5l;},_58);});}},_5o=function(_5p,_5q){return new F(function(){return _59(_5p,function(_x,_){return new F(function(){return _0(_5p,_x,_);});},_5q);});},_5r=new T(function(){return B(unCStr("method"));}),_5s=new T(function(){return B(unCStr("action"));}),_5t=new T(function(){return B(unCStr("UTF-8"));}),_5u=new T(function(){return B(unCStr("acceptCharset"));}),_5v=[0,_5u,_5t],_5w=new T(function(){return B(unCStr("form"));}),_5x=function(_5y,_){return new F(function(){return _z(_5w,_5y,_);});},_5z=function(_5A,_5B,_5C){return function(_5D,_){var _5E=B(A(new T(function(){return B(_q(_5x,[1,_5v,[1,[0,_5s,_5A],[1,[0,_5r,_5B],_W]]]));}),[_5D,_])),_5F=_5E,_5G=B(A(_5C,[_5F,_])),_5H=_5G;return _5F;};},_5I=new T(function(){return B(unCStr("select"));}),_5J=function(_5K,_){return new F(function(){return _z(_5I,_5K,_);});},_5L=function(_5M,_5N){return function(_5O,_){var _5P=B(A(new T(function(){return B(_q(_5J,[1,[0,_7,_5M],_W]));}),[_5O,_])),_5Q=_5P,_5R=B(A(_5N,[_5Q,_])),_5S=_5R;return _5Q;};},_5T=new T(function(){return B(unCStr("textarea"));}),_5U=function(_5V,_){return new F(function(){return _z(_5T,_5V,_);});},_5W=function(_5X,_5Y){return function(_5Z,_){var _60=B(A(new T(function(){return B(_q(_5U,[1,[0,_7,_5X],_W]));}),[_5Z,_])),_61=_60,_62=B(_0(_5Y,_61,_)),_63=_62;return _61;};},_64=new T(function(){return B(unCStr("color:red"));}),_65=new T(function(){return B(unCStr("style"));}),_66=[0,_65,_64],_67=[1,_66,_W],_68=[0,98],_69=[1,_68,_W],_6a=function(_6b){return new F(function(){return _q(function(_6c,_){var _6d=B(_z(_69,_6c,_)),_6e=_6d,_6f=B(A(_6b,[_6e,_])),_6g=_6f;return _6e;},_67);});},_6h=function(_6i,_6j,_){var _6k=E(_6i);if(!_6k[0]){return _6j;}else{var _6l=B(A(_6k[1],[_6j,_])),_6m=_6l,_6n=B(_6h(_6k[2],_6j,_)),_6o=_6n;return _6j;}},_6p=function(_6q,_6r,_){return new F(function(){return _6h(_6q,_6r,_);});},_6s=function(_6t,_6u,_6v,_){var _6w=B(A(_6t,[_6v,_])),_6x=_6w,_6y=B(A(_6u,[_6v,_])),_6z=_6y;return _6v;},_6A=[0,_1U,_6s,_6p],_6B=[0,_6A,_4v,_0,_0,_30,_6a,_4T,_51,_4F,_5W,_5L,_59,_5o,_5z,_q],_6C=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_6D=new T(function(){return B(unCStr("base"));}),_6E=new T(function(){return B(unCStr("IOException"));}),_6F=new T(function(){var _6G=hs_wordToWord64(4053623282),_6H=_6G,_6I=hs_wordToWord64(3693590983),_6J=_6I;return [0,_6H,_6J,[0,_6H,_6J,_6D,_6C,_6E],_W];}),_6K=function(_6L){return E(_6F);},_6M=function(_6N){return E(E(_6N)[1]);},_6O=function(_6P,_6Q,_6R){var _6S=B(A(_6P,[_])),_6T=B(A(_6Q,[_])),_6U=hs_eqWord64(_6S[1],_6T[1]),_6V=_6U;if(!E(_6V)){return [0];}else{var _6W=hs_eqWord64(_6S[2],_6T[2]),_6X=_6W;return E(_6X)==0?[0]:[1,_6R];}},_6Y=function(_6Z){var _70=E(_6Z);return new F(function(){return _6O(B(_6M(_70[1])),_6K,_70[2]);});},_71=new T(function(){return B(unCStr(": "));}),_72=[0,41],_73=new T(function(){return B(unCStr(" ("));}),_74=new T(function(){return B(unCStr("already exists"));}),_75=new T(function(){return B(unCStr("does not exist"));}),_76=new T(function(){return B(unCStr("protocol error"));}),_77=new T(function(){return B(unCStr("failed"));}),_78=new T(function(){return B(unCStr("invalid argument"));}),_79=new T(function(){return B(unCStr("inappropriate type"));}),_7a=new T(function(){return B(unCStr("hardware fault"));}),_7b=new T(function(){return B(unCStr("unsupported operation"));}),_7c=new T(function(){return B(unCStr("timeout"));}),_7d=new T(function(){return B(unCStr("resource vanished"));}),_7e=new T(function(){return B(unCStr("interrupted"));}),_7f=new T(function(){return B(unCStr("resource busy"));}),_7g=new T(function(){return B(unCStr("resource exhausted"));}),_7h=new T(function(){return B(unCStr("end of file"));}),_7i=new T(function(){return B(unCStr("illegal operation"));}),_7j=new T(function(){return B(unCStr("permission denied"));}),_7k=new T(function(){return B(unCStr("user error"));}),_7l=new T(function(){return B(unCStr("unsatisified constraints"));}),_7m=new T(function(){return B(unCStr("system error"));}),_7n=function(_7o,_7p){switch(E(_7o)){case 0:return new F(function(){return _1l(_74,_7p);});break;case 1:return new F(function(){return _1l(_75,_7p);});break;case 2:return new F(function(){return _1l(_7f,_7p);});break;case 3:return new F(function(){return _1l(_7g,_7p);});break;case 4:return new F(function(){return _1l(_7h,_7p);});break;case 5:return new F(function(){return _1l(_7i,_7p);});break;case 6:return new F(function(){return _1l(_7j,_7p);});break;case 7:return new F(function(){return _1l(_7k,_7p);});break;case 8:return new F(function(){return _1l(_7l,_7p);});break;case 9:return new F(function(){return _1l(_7m,_7p);});break;case 10:return new F(function(){return _1l(_76,_7p);});break;case 11:return new F(function(){return _1l(_77,_7p);});break;case 12:return new F(function(){return _1l(_78,_7p);});break;case 13:return new F(function(){return _1l(_79,_7p);});break;case 14:return new F(function(){return _1l(_7a,_7p);});break;case 15:return new F(function(){return _1l(_7b,_7p);});break;case 16:return new F(function(){return _1l(_7c,_7p);});break;case 17:return new F(function(){return _1l(_7d,_7p);});break;default:return new F(function(){return _1l(_7e,_7p);});}},_7q=[0,125],_7r=new T(function(){return B(unCStr("{handle: "));}),_7s=function(_7t,_7u,_7v,_7w,_7x,_7y){var _7z=new T(function(){var _7A=new T(function(){return B(_7n(_7u,new T(function(){var _7B=E(_7w);return _7B[0]==0?E(_7y):B(_1l(_73,new T(function(){return B(_1l(_7B,[1,_72,_7y]));})));})));}),_7C=E(_7v);return _7C[0]==0?E(_7A):B(_1l(_7C,new T(function(){return B(_1l(_71,_7A));})));}),_7D=E(_7x);if(!_7D[0]){var _7E=E(_7t);if(!_7E[0]){return E(_7z);}else{var _7F=E(_7E[1]);return _7F[0]==0?B(_1l(_7r,new T(function(){return B(_1l(_7F[1],[1,_7q,new T(function(){return B(_1l(_71,_7z));})]));}))):B(_1l(_7r,new T(function(){return B(_1l(_7F[1],[1,_7q,new T(function(){return B(_1l(_71,_7z));})]));})));}}else{return new F(function(){return _1l(_7D[1],new T(function(){return B(_1l(_71,_7z));}));});}},_7G=function(_7H){var _7I=E(_7H);return new F(function(){return _7s(_7I[1],_7I[2],_7I[3],_7I[4],_7I[6],_W);});},_7J=function(_7K,_7L){var _7M=E(_7K);return new F(function(){return _7s(_7M[1],_7M[2],_7M[3],_7M[4],_7M[6],_7L);});},_7N=[0,44],_7O=[0,93],_7P=[0,91],_7Q=function(_7R,_7S,_7T){var _7U=E(_7S);return _7U[0]==0?B(unAppCStr("[]",_7T)):[1,_7P,new T(function(){return B(A(_7R,[_7U[1],new T(function(){var _7V=function(_7W){var _7X=E(_7W);return _7X[0]==0?E([1,_7O,_7T]):[1,_7N,new T(function(){return B(A(_7R,[_7X[1],new T(function(){return B(_7V(_7X[2]));})]));})];};return B(_7V(_7U[2]));})]));})];},_7Y=function(_7Z,_80){return new F(function(){return _7Q(_7J,_7Z,_80);});},_81=function(_82,_83,_84){var _85=E(_83);return new F(function(){return _7s(_85[1],_85[2],_85[3],_85[4],_85[6],_84);});},_86=[0,_81,_7G,_7Y],_87=new T(function(){return [0,_6K,_86,_88,_6Y];}),_88=function(_89){return [0,_87,_89];},_8a=7,_8b=function(_8c){return [0,_P,_8a,_W,_8c,_P,_P];},_8d=function(_8e,_){return new F(function(){return die(new T(function(){return B(_88(new T(function(){return B(_8b(_8e));})));}));});},_8f=function(_8g,_){return new F(function(){return _8d(_8g,_);});},_8h=function(_8i,_){return new F(function(){return _8f(_8i,_);});},_8j=function(_8k,_){return new F(function(){return _8h(_8k,_);});},_8l=function(_8m,_8n,_){var _8o=B(A(_8m,[_])),_8p=_8o;return new F(function(){return A(_8n,[_8p,_]);});},_8q=function(_8r,_8s,_){var _8t=B(A(_8r,[_])),_8u=_8t;return new F(function(){return A(_8s,[_]);});},_8v=[0,_8l,_8q,_1U,_8j],_8w=[0,_8v,_f],_8x=new T(function(){return B(unCStr("Control.Exception.Base"));}),_8y=new T(function(){return B(unCStr("base"));}),_8z=new T(function(){return B(unCStr("PatternMatchFail"));}),_8A=new T(function(){var _8B=hs_wordToWord64(18445595),_8C=_8B,_8D=hs_wordToWord64(52003073),_8E=_8D;return [0,_8C,_8E,[0,_8C,_8E,_8y,_8x,_8z],_W];}),_8F=function(_8G){return E(_8A);},_8H=function(_8I){var _8J=E(_8I);return new F(function(){return _6O(B(_6M(_8J[1])),_8F,_8J[2]);});},_8K=function(_8L){return E(E(_8L)[1]);},_8M=function(_8N,_8O){return new F(function(){return _1l(E(_8N)[1],_8O);});},_8P=function(_8Q,_8R){return new F(function(){return _7Q(_8M,_8Q,_8R);});},_8S=function(_8T,_8U,_8V){return new F(function(){return _1l(E(_8U)[1],_8V);});},_8W=[0,_8S,_8K,_8P],_8X=new T(function(){return [0,_8F,_8W,_8Y,_8H];}),_8Y=function(_8Z){return [0,_8X,_8Z];},_90=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_91=function(_92,_93){return new F(function(){return die(new T(function(){return B(A(_93,[_92]));}));});},_94=function(_95,_96){var _97=E(_96);if(!_97[0]){return [0,_W,_W];}else{var _98=_97[1];if(!B(A(_95,[_98]))){return [0,_W,_97];}else{var _99=new T(function(){var _9a=B(_94(_95,_97[2]));return [0,_9a[1],_9a[2]];});return [0,[1,_98,new T(function(){return E(E(_99)[1]);})],new T(function(){return E(E(_99)[2]);})];}}},_9b=[0,32],_9c=[0,10],_9d=[1,_9c,_W],_9e=function(_9f){return E(E(_9f)[1])==124?false:true;},_9g=function(_9h,_9i){var _9j=B(_94(_9e,B(unCStr(_9h)))),_9k=_9j[1],_9l=function(_9m,_9n){return new F(function(){return _1l(_9m,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_1l(_9i,new T(function(){return B(_1l(_9n,_9d));})));})));}));});},_9o=E(_9j[2]);if(!_9o[0]){return new F(function(){return _9l(_9k,_W);});}else{return E(E(_9o[1])[1])==124?B(_9l(_9k,[1,_9b,_9o[2]])):B(_9l(_9k,_W));}},_9p=function(_9q){return new F(function(){return _91([0,new T(function(){return B(_9g(_9q,_90));})],_8Y);});},_9r=new T(function(){return B(_9p("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus"));}),_9s=function(_9t,_9u){while(1){var _9v=(function(_9w,_9x){var _9y=E(_9w);switch(_9y[0]){case 0:var _9z=E(_9x);if(!_9z[0]){return [0];}else{_9t=B(A(_9y[1],[_9z[1]]));_9u=_9z[2];return null;}break;case 1:var _9A=B(A(_9y[1],[_9x])),_9B=_9x;_9t=_9A;_9u=_9B;return null;case 2:return [0];case 3:return [1,[0,_9y[1],_9x],new T(function(){return B(_9s(_9y[2],_9x));})];default:return E(_9y[1]);}})(_9t,_9u);if(_9v!=null){return _9v;}}},_9C=function(_9D,_9E){var _9F=function(_9G){var _9H=E(_9E);if(_9H[0]==3){return [3,_9H[1],new T(function(){return B(_9C(_9D,_9H[2]));})];}else{var _9I=E(_9D);if(_9I[0]==2){return E(_9H);}else{var _9J=E(_9H);if(_9J[0]==2){return E(_9I);}else{var _9K=function(_9L){var _9M=E(_9J);if(_9M[0]==4){return [1,function(_9N){return [4,new T(function(){return B(_1l(B(_9s(_9I,_9N)),_9M[1]));})];}];}else{var _9O=E(_9I);if(_9O[0]==1){var _9P=_9O[1],_9Q=E(_9M);return _9Q[0]==0?[1,function(_9R){return new F(function(){return _9C(B(A(_9P,[_9R])),_9Q);});}]:[1,function(_9S){return new F(function(){return _9C(B(A(_9P,[_9S])),new T(function(){return B(A(_9Q[1],[_9S]));}));});}];}else{var _9T=E(_9M);return _9T[0]==0?E(_9r):[1,function(_9U){return new F(function(){return _9C(_9O,new T(function(){return B(A(_9T[1],[_9U]));}));});}];}}},_9V=E(_9I);switch(_9V[0]){case 1:var _9W=E(_9J);if(_9W[0]==4){return [1,function(_9X){return [4,new T(function(){return B(_1l(B(_9s(B(A(_9V[1],[_9X])),_9X)),_9W[1]));})];}];}else{return new F(function(){return _9K(_);});}break;case 4:var _9Y=_9V[1],_9Z=E(_9J);switch(_9Z[0]){case 0:return [1,function(_a0){return [4,new T(function(){return B(_1l(_9Y,new T(function(){return B(_9s(_9Z,_a0));})));})];}];case 1:return [1,function(_a1){return [4,new T(function(){return B(_1l(_9Y,new T(function(){return B(_9s(B(A(_9Z[1],[_a1])),_a1));})));})];}];default:return [4,new T(function(){return B(_1l(_9Y,_9Z[1]));})];}break;default:return new F(function(){return _9K(_);});}}}}},_a2=E(_9D);switch(_a2[0]){case 0:var _a3=E(_9E);if(!_a3[0]){return [0,function(_a4){return new F(function(){return _9C(B(A(_a2[1],[_a4])),new T(function(){return B(A(_a3[1],[_a4]));}));});}];}else{return new F(function(){return _9F(_);});}break;case 3:return [3,_a2[1],new T(function(){return B(_9C(_a2[2],_9E));})];default:return new F(function(){return _9F(_);});}},_a5=[0,41],_a6=[1,_a5,_W],_a7=[0,40],_a8=[1,_a7,_W],_a9=function(_aa,_ab){while(1){var _ac=E(_aa);if(!_ac[0]){return E(_ab)[0]==0?true:false;}else{var _ad=E(_ab);if(!_ad[0]){return false;}else{if(E(_ac[1])[1]!=E(_ad[1])[1]){return false;}else{_aa=_ac[2];_ab=_ad[2];continue;}}}}},_ae=function(_af,_ag){return E(_af)[1]!=E(_ag)[1];},_ah=function(_ai,_aj){return E(_ai)[1]==E(_aj)[1];},_ak=[0,_ah,_ae],_al=function(_am,_an){while(1){var _ao=E(_am);if(!_ao[0]){return E(_an)[0]==0?true:false;}else{var _ap=E(_an);if(!_ap[0]){return false;}else{if(E(_ao[1])[1]!=E(_ap[1])[1]){return false;}else{_am=_ao[2];_an=_ap[2];continue;}}}}},_aq=function(_ar,_as){return !B(_al(_ar,_as))?true:false;},_at=[0,_al,_aq],_au=function(_av,_aw){var _ax=E(_av);switch(_ax[0]){case 0:return [0,function(_ay){return new F(function(){return _au(B(A(_ax[1],[_ay])),_aw);});}];case 1:return [1,function(_az){return new F(function(){return _au(B(A(_ax[1],[_az])),_aw);});}];case 2:return [2];case 3:return new F(function(){return _9C(B(A(_aw,[_ax[1]])),new T(function(){return B(_au(_ax[2],_aw));}));});break;default:var _aA=function(_aB){var _aC=E(_aB);if(!_aC[0]){return [0];}else{var _aD=E(_aC[1]);return new F(function(){return _1l(B(_9s(B(A(_aw,[_aD[1]])),_aD[2])),new T(function(){return B(_aA(_aC[2]));}));});}},_aE=B(_aA(_ax[1]));return _aE[0]==0?[2]:[4,_aE];}},_aF=[2],_aG=function(_aH){return [3,_aH,_aF];},_aI=function(_aJ,_aK){var _aL=E(_aJ);if(!_aL){return new F(function(){return A(_aK,[_8]);});}else{return [0,function(_aM){return E(new T(function(){return B(_aI(_aL-1|0,_aK));}));}];}},_aN=function(_aO,_aP,_aQ){return function(_aR){return new F(function(){return A(function(_aS,_aT,_aU){while(1){var _aV=(function(_aW,_aX,_aY){var _aZ=E(_aW);switch(_aZ[0]){case 0:var _b0=E(_aX);if(!_b0[0]){return E(_aP);}else{_aS=B(A(_aZ[1],[_b0[1]]));_aT=_b0[2];var _b1=_aY+1|0;_aU=_b1;return null;}break;case 1:var _b2=B(A(_aZ[1],[_aX])),_b3=_aX,_b1=_aY;_aS=_b2;_aT=_b3;_aU=_b1;return null;case 2:return E(_aP);case 3:return function(_b4){return new F(function(){return _aI(_aY,function(_b5){return E(new T(function(){return B(_au(_aZ,_b4));}));});});};default:return function(_b6){return new F(function(){return _au(_aZ,_b6);});};}})(_aS,_aT,_aU);if(_aV!=null){return _aV;}}},[new T(function(){return B(A(_aO,[_aG]));}),_aR,0,_aQ]);});};},_b7=function(_b8){return new F(function(){return A(_b8,[_W]);});},_b9=function(_ba,_bb){var _bc=function(_bd){var _be=E(_bd);if(!_be[0]){return E(_b7);}else{var _bf=_be[1];return !B(A(_ba,[_bf]))?E(_b7):function(_bg){return [0,function(_bh){return E(new T(function(){return B(A(new T(function(){return B(_bc(_be[2]));}),[function(_bi){return new F(function(){return A(_bg,[[1,_bf,_bi]]);});}]));}));}];};}};return function(_bj){return new F(function(){return A(_bc,[_bj,_bb]);});};},_bk=[6],_bl=new T(function(){return B(unCStr("valDig: Bad base"));}),_bm=new T(function(){return B(err(_bl));}),_bn=function(_bo,_bp){var _bq=function(_br,_bs){var _bt=E(_br);if(!_bt[0]){return function(_bu){return new F(function(){return A(_bu,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{var _bv=E(_bt[1])[1],_bw=function(_bx){return function(_by){return [0,function(_bz){return E(new T(function(){return B(A(new T(function(){return B(_bq(_bt[2],function(_bA){return new F(function(){return A(_bs,[[1,_bx,_bA]]);});}));}),[_by]));}));}];};};switch(E(E(_bo)[1])){case 8:if(48>_bv){return function(_bB){return new F(function(){return A(_bB,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{if(_bv>55){return function(_bC){return new F(function(){return A(_bC,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{return new F(function(){return _bw([0,_bv-48|0]);});}}break;case 10:if(48>_bv){return function(_bD){return new F(function(){return A(_bD,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{if(_bv>57){return function(_bE){return new F(function(){return A(_bE,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{return new F(function(){return _bw([0,_bv-48|0]);});}}break;case 16:if(48>_bv){if(97>_bv){if(65>_bv){return function(_bF){return new F(function(){return A(_bF,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{if(_bv>70){return function(_bG){return new F(function(){return A(_bG,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{return new F(function(){return _bw([0,(_bv-65|0)+10|0]);});}}}else{if(_bv>102){if(65>_bv){return function(_bH){return new F(function(){return A(_bH,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{if(_bv>70){return function(_bI){return new F(function(){return A(_bI,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{return new F(function(){return _bw([0,(_bv-65|0)+10|0]);});}}}else{return new F(function(){return _bw([0,(_bv-97|0)+10|0]);});}}}else{if(_bv>57){if(97>_bv){if(65>_bv){return function(_bJ){return new F(function(){return A(_bJ,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{if(_bv>70){return function(_bK){return new F(function(){return A(_bK,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{return new F(function(){return _bw([0,(_bv-65|0)+10|0]);});}}}else{if(_bv>102){if(65>_bv){return function(_bL){return new F(function(){return A(_bL,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{if(_bv>70){return function(_bM){return new F(function(){return A(_bM,[new T(function(){return B(A(_bs,[_W]));})]);});};}else{return new F(function(){return _bw([0,(_bv-65|0)+10|0]);});}}}else{return new F(function(){return _bw([0,(_bv-97|0)+10|0]);});}}}else{return new F(function(){return _bw([0,_bv-48|0]);});}}break;default:return E(_bm);}}};return function(_bN){return new F(function(){return A(_bq,[_bN,_f,function(_bO){var _bP=E(_bO);return _bP[0]==0?[2]:B(A(_bp,[_bP]));}]);});};},_bQ=[0,10],_bR=[0,1],_bS=[0,2147483647],_bT=function(_bU,_bV){while(1){var _bW=E(_bU);if(!_bW[0]){var _bX=_bW[1],_bY=E(_bV);if(!_bY[0]){var _bZ=_bY[1],_c0=addC(_bX,_bZ);if(!E(_c0[2])){return [0,_c0[1]];}else{_bU=[1,I_fromInt(_bX)];_bV=[1,I_fromInt(_bZ)];continue;}}else{_bU=[1,I_fromInt(_bX)];_bV=_bY;continue;}}else{var _c1=E(_bV);if(!_c1[0]){_bU=_bW;_bV=[1,I_fromInt(_c1[1])];continue;}else{return [1,I_add(_bW[1],_c1[1])];}}}},_c2=new T(function(){return B(_bT(_bS,_bR));}),_c3=function(_c4){var _c5=E(_c4);if(!_c5[0]){var _c6=E(_c5[1]);return _c6==(-2147483648)?E(_c2):[0, -_c6];}else{return [1,I_negate(_c5[1])];}},_c7=[0,10],_c8=[0,0],_c9=function(_ca){return [0,_ca];},_cb=function(_cc,_cd){while(1){var _ce=E(_cc);if(!_ce[0]){var _cf=_ce[1],_cg=E(_cd);if(!_cg[0]){var _ch=_cg[1];if(!(imul(_cf,_ch)|0)){return [0,imul(_cf,_ch)|0];}else{_cc=[1,I_fromInt(_cf)];_cd=[1,I_fromInt(_ch)];continue;}}else{_cc=[1,I_fromInt(_cf)];_cd=_cg;continue;}}else{var _ci=E(_cd);if(!_ci[0]){_cc=_ce;_cd=[1,I_fromInt(_ci[1])];continue;}else{return [1,I_mul(_ce[1],_ci[1])];}}}},_cj=function(_ck,_cl,_cm){while(1){var _cn=E(_cm);if(!_cn[0]){return E(_cl);}else{var _co=B(_bT(B(_cb(_cl,_ck)),B(_c9(E(_cn[1])[1]))));_cm=_cn[2];_cl=_co;continue;}}},_cp=function(_cq){var _cr=new T(function(){return B(_9C(B(_9C([0,function(_cs){return E(E(_cs)[1])==45?[1,B(_bn(_bQ,function(_ct){return new F(function(){return A(_cq,[[1,new T(function(){return B(_c3(B(_cj(_c7,_c8,_ct))));})]]);});}))]:[2];}],[0,function(_cu){return E(E(_cu)[1])==43?[1,B(_bn(_bQ,function(_cv){return new F(function(){return A(_cq,[[1,new T(function(){return B(_cj(_c7,_c8,_cv));})]]);});}))]:[2];}])),new T(function(){return [1,B(_bn(_bQ,function(_cw){return new F(function(){return A(_cq,[[1,new T(function(){return B(_cj(_c7,_c8,_cw));})]]);});}))];})));});return new F(function(){return _9C([0,function(_cx){return E(E(_cx)[1])==101?E(_cr):[2];}],[0,function(_cy){return E(E(_cy)[1])==69?E(_cr):[2];}]);});},_cz=function(_cA){return new F(function(){return A(_cA,[_P]);});},_cB=function(_cC){return new F(function(){return A(_cC,[_P]);});},_cD=function(_cE){return function(_cF){return E(E(_cF)[1])==46?[1,B(_bn(_bQ,function(_cG){return new F(function(){return A(_cE,[[1,_cG]]);});}))]:[2];};},_cH=function(_cI){return [0,B(_cD(_cI))];},_cJ=function(_cK){return new F(function(){return _bn(_bQ,function(_cL){return [1,B(_aN(_cH,_cz,function(_cM){return [1,B(_aN(_cp,_cB,function(_cN){return new F(function(){return A(_cK,[[5,[1,_cL,_cM,_cN]]]);});}))];}))];});});},_cO=function(_cP){return [1,B(_cJ(_cP))];},_cQ=function(_cR){return E(E(_cR)[1]);},_cS=function(_cT,_cU,_cV){while(1){var _cW=E(_cV);if(!_cW[0]){return false;}else{if(!B(A(_cQ,[_cT,_cU,_cW[1]]))){_cV=_cW[2];continue;}else{return true;}}}},_cX=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_cY=function(_cZ){return new F(function(){return _cS(_ak,_cZ,_cX);});},_d0=[0,8],_d1=[0,16],_d2=function(_d3){var _d4=function(_d5){return new F(function(){return A(_d3,[[5,[0,_d0,_d5]]]);});},_d6=function(_d7){return new F(function(){return A(_d3,[[5,[0,_d1,_d7]]]);});};return function(_d8){return E(E(_d8)[1])==48?E([0,function(_d9){switch(E(E(_d9)[1])){case 79:return [1,B(_bn(_d0,_d4))];case 88:return [1,B(_bn(_d1,_d6))];case 111:return [1,B(_bn(_d0,_d4))];case 120:return [1,B(_bn(_d1,_d6))];default:return [2];}}]):[2];};},_da=function(_db){return [0,B(_d2(_db))];},_dc=true,_dd=function(_de){var _df=new T(function(){return B(A(_de,[_d0]));}),_dg=new T(function(){return B(A(_de,[_d1]));});return function(_dh){switch(E(E(_dh)[1])){case 79:return E(_df);case 88:return E(_dg);case 111:return E(_df);case 120:return E(_dg);default:return [2];}};},_di=function(_dj){return [0,B(_dd(_dj))];},_dk=[0,92],_dl=function(_dm){return new F(function(){return A(_dm,[_bQ]);});},_dn=function(_do){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_1J(9,_do,_W));}))));});},_dp=function(_dq){var _dr=E(_dq);return _dr[0]==0?E(_dr[1]):I_toInt(_dr[1]);},_ds=function(_dt,_du){var _dv=E(_dt);if(!_dv[0]){var _dw=_dv[1],_dx=E(_du);return _dx[0]==0?_dw<=_dx[1]:I_compareInt(_dx[1],_dw)>=0;}else{var _dy=_dv[1],_dz=E(_du);return _dz[0]==0?I_compareInt(_dy,_dz[1])<=0:I_compare(_dy,_dz[1])<=0;}},_dA=function(_dB){return [2];},_dC=function(_dD){var _dE=E(_dD);if(!_dE[0]){return E(_dA);}else{var _dF=_dE[1],_dG=E(_dE[2]);return _dG[0]==0?E(_dF):function(_dH){return new F(function(){return _9C(B(A(_dF,[_dH])),new T(function(){return B(A(new T(function(){return B(_dC(_dG));}),[_dH]));}));});};}},_dI=function(_dJ){return [2];},_dK=function(_dL,_dM){var _dN=function(_dO,_dP){var _dQ=E(_dO);if(!_dQ[0]){return function(_dR){return new F(function(){return A(_dR,[_dL]);});};}else{var _dS=E(_dP);return _dS[0]==0?E(_dI):E(_dQ[1])[1]!=E(_dS[1])[1]?E(_dI):function(_dT){return [0,function(_dU){return E(new T(function(){return B(A(new T(function(){return B(_dN(_dQ[2],_dS[2]));}),[_dT]));}));}];};}};return function(_dV){return new F(function(){return A(_dN,[_dL,_dV,_dM]);});};},_dW=new T(function(){return B(unCStr("SOH"));}),_dX=[0,1],_dY=function(_dZ){return [1,B(_dK(_dW,function(_e0){return E(new T(function(){return B(A(_dZ,[_dX]));}));}))];},_e1=new T(function(){return B(unCStr("SO"));}),_e2=[0,14],_e3=function(_e4){return [1,B(_dK(_e1,function(_e5){return E(new T(function(){return B(A(_e4,[_e2]));}));}))];},_e6=function(_e7){return [1,B(_aN(_dY,_e3,_e7))];},_e8=new T(function(){return B(unCStr("NUL"));}),_e9=[0,0],_ea=function(_eb){return [1,B(_dK(_e8,function(_ec){return E(new T(function(){return B(A(_eb,[_e9]));}));}))];},_ed=new T(function(){return B(unCStr("STX"));}),_ee=[0,2],_ef=function(_eg){return [1,B(_dK(_ed,function(_eh){return E(new T(function(){return B(A(_eg,[_ee]));}));}))];},_ei=new T(function(){return B(unCStr("ETX"));}),_ej=[0,3],_ek=function(_el){return [1,B(_dK(_ei,function(_em){return E(new T(function(){return B(A(_el,[_ej]));}));}))];},_en=new T(function(){return B(unCStr("EOT"));}),_eo=[0,4],_ep=function(_eq){return [1,B(_dK(_en,function(_er){return E(new T(function(){return B(A(_eq,[_eo]));}));}))];},_es=new T(function(){return B(unCStr("ENQ"));}),_et=[0,5],_eu=function(_ev){return [1,B(_dK(_es,function(_ew){return E(new T(function(){return B(A(_ev,[_et]));}));}))];},_ex=new T(function(){return B(unCStr("ACK"));}),_ey=[0,6],_ez=function(_eA){return [1,B(_dK(_ex,function(_eB){return E(new T(function(){return B(A(_eA,[_ey]));}));}))];},_eC=new T(function(){return B(unCStr("BEL"));}),_eD=[0,7],_eE=function(_eF){return [1,B(_dK(_eC,function(_eG){return E(new T(function(){return B(A(_eF,[_eD]));}));}))];},_eH=new T(function(){return B(unCStr("BS"));}),_eI=[0,8],_eJ=function(_eK){return [1,B(_dK(_eH,function(_eL){return E(new T(function(){return B(A(_eK,[_eI]));}));}))];},_eM=new T(function(){return B(unCStr("HT"));}),_eN=[0,9],_eO=function(_eP){return [1,B(_dK(_eM,function(_eQ){return E(new T(function(){return B(A(_eP,[_eN]));}));}))];},_eR=new T(function(){return B(unCStr("LF"));}),_eS=[0,10],_eT=function(_eU){return [1,B(_dK(_eR,function(_eV){return E(new T(function(){return B(A(_eU,[_eS]));}));}))];},_eW=new T(function(){return B(unCStr("VT"));}),_eX=[0,11],_eY=function(_eZ){return [1,B(_dK(_eW,function(_f0){return E(new T(function(){return B(A(_eZ,[_eX]));}));}))];},_f1=new T(function(){return B(unCStr("FF"));}),_f2=[0,12],_f3=function(_f4){return [1,B(_dK(_f1,function(_f5){return E(new T(function(){return B(A(_f4,[_f2]));}));}))];},_f6=new T(function(){return B(unCStr("CR"));}),_f7=[0,13],_f8=function(_f9){return [1,B(_dK(_f6,function(_fa){return E(new T(function(){return B(A(_f9,[_f7]));}));}))];},_fb=new T(function(){return B(unCStr("SI"));}),_fc=[0,15],_fd=function(_fe){return [1,B(_dK(_fb,function(_ff){return E(new T(function(){return B(A(_fe,[_fc]));}));}))];},_fg=new T(function(){return B(unCStr("DLE"));}),_fh=[0,16],_fi=function(_fj){return [1,B(_dK(_fg,function(_fk){return E(new T(function(){return B(A(_fj,[_fh]));}));}))];},_fl=new T(function(){return B(unCStr("DC1"));}),_fm=[0,17],_fn=function(_fo){return [1,B(_dK(_fl,function(_fp){return E(new T(function(){return B(A(_fo,[_fm]));}));}))];},_fq=new T(function(){return B(unCStr("DC2"));}),_fr=[0,18],_fs=function(_ft){return [1,B(_dK(_fq,function(_fu){return E(new T(function(){return B(A(_ft,[_fr]));}));}))];},_fv=new T(function(){return B(unCStr("DC3"));}),_fw=[0,19],_fx=function(_fy){return [1,B(_dK(_fv,function(_fz){return E(new T(function(){return B(A(_fy,[_fw]));}));}))];},_fA=new T(function(){return B(unCStr("DC4"));}),_fB=[0,20],_fC=function(_fD){return [1,B(_dK(_fA,function(_fE){return E(new T(function(){return B(A(_fD,[_fB]));}));}))];},_fF=new T(function(){return B(unCStr("NAK"));}),_fG=[0,21],_fH=function(_fI){return [1,B(_dK(_fF,function(_fJ){return E(new T(function(){return B(A(_fI,[_fG]));}));}))];},_fK=new T(function(){return B(unCStr("SYN"));}),_fL=[0,22],_fM=function(_fN){return [1,B(_dK(_fK,function(_fO){return E(new T(function(){return B(A(_fN,[_fL]));}));}))];},_fP=new T(function(){return B(unCStr("ETB"));}),_fQ=[0,23],_fR=function(_fS){return [1,B(_dK(_fP,function(_fT){return E(new T(function(){return B(A(_fS,[_fQ]));}));}))];},_fU=new T(function(){return B(unCStr("CAN"));}),_fV=[0,24],_fW=function(_fX){return [1,B(_dK(_fU,function(_fY){return E(new T(function(){return B(A(_fX,[_fV]));}));}))];},_fZ=new T(function(){return B(unCStr("EM"));}),_g0=[0,25],_g1=function(_g2){return [1,B(_dK(_fZ,function(_g3){return E(new T(function(){return B(A(_g2,[_g0]));}));}))];},_g4=new T(function(){return B(unCStr("SUB"));}),_g5=[0,26],_g6=function(_g7){return [1,B(_dK(_g4,function(_g8){return E(new T(function(){return B(A(_g7,[_g5]));}));}))];},_g9=new T(function(){return B(unCStr("ESC"));}),_ga=[0,27],_gb=function(_gc){return [1,B(_dK(_g9,function(_gd){return E(new T(function(){return B(A(_gc,[_ga]));}));}))];},_ge=new T(function(){return B(unCStr("FS"));}),_gf=[0,28],_gg=function(_gh){return [1,B(_dK(_ge,function(_gi){return E(new T(function(){return B(A(_gh,[_gf]));}));}))];},_gj=new T(function(){return B(unCStr("GS"));}),_gk=[0,29],_gl=function(_gm){return [1,B(_dK(_gj,function(_gn){return E(new T(function(){return B(A(_gm,[_gk]));}));}))];},_go=new T(function(){return B(unCStr("RS"));}),_gp=[0,30],_gq=function(_gr){return [1,B(_dK(_go,function(_gs){return E(new T(function(){return B(A(_gr,[_gp]));}));}))];},_gt=new T(function(){return B(unCStr("US"));}),_gu=[0,31],_gv=function(_gw){return [1,B(_dK(_gt,function(_gx){return E(new T(function(){return B(A(_gw,[_gu]));}));}))];},_gy=new T(function(){return B(unCStr("SP"));}),_gz=[0,32],_gA=function(_gB){return [1,B(_dK(_gy,function(_gC){return E(new T(function(){return B(A(_gB,[_gz]));}));}))];},_gD=new T(function(){return B(unCStr("DEL"));}),_gE=[0,127],_gF=function(_gG){return [1,B(_dK(_gD,function(_gH){return E(new T(function(){return B(A(_gG,[_gE]));}));}))];},_gI=[1,_gF,_W],_gJ=[1,_gA,_gI],_gK=[1,_gv,_gJ],_gL=[1,_gq,_gK],_gM=[1,_gl,_gL],_gN=[1,_gg,_gM],_gO=[1,_gb,_gN],_gP=[1,_g6,_gO],_gQ=[1,_g1,_gP],_gR=[1,_fW,_gQ],_gS=[1,_fR,_gR],_gT=[1,_fM,_gS],_gU=[1,_fH,_gT],_gV=[1,_fC,_gU],_gW=[1,_fx,_gV],_gX=[1,_fs,_gW],_gY=[1,_fn,_gX],_gZ=[1,_fi,_gY],_h0=[1,_fd,_gZ],_h1=[1,_f8,_h0],_h2=[1,_f3,_h1],_h3=[1,_eY,_h2],_h4=[1,_eT,_h3],_h5=[1,_eO,_h4],_h6=[1,_eJ,_h5],_h7=[1,_eE,_h6],_h8=[1,_ez,_h7],_h9=[1,_eu,_h8],_ha=[1,_ep,_h9],_hb=[1,_ek,_ha],_hc=[1,_ef,_hb],_hd=[1,_ea,_hc],_he=[1,_e6,_hd],_hf=new T(function(){return B(_dC(_he));}),_hg=[0,1114111],_hh=[0,34],_hi=[0,39],_hj=function(_hk){var _hl=new T(function(){return B(A(_hk,[_eD]));}),_hm=new T(function(){return B(A(_hk,[_eI]));}),_hn=new T(function(){return B(A(_hk,[_eN]));}),_ho=new T(function(){return B(A(_hk,[_eS]));}),_hp=new T(function(){return B(A(_hk,[_eX]));}),_hq=new T(function(){return B(A(_hk,[_f2]));}),_hr=new T(function(){return B(A(_hk,[_f7]));});return new F(function(){return _9C([0,function(_hs){switch(E(E(_hs)[1])){case 34:return E(new T(function(){return B(A(_hk,[_hh]));}));case 39:return E(new T(function(){return B(A(_hk,[_hi]));}));case 92:return E(new T(function(){return B(A(_hk,[_dk]));}));case 97:return E(_hl);case 98:return E(_hm);case 102:return E(_hq);case 110:return E(_ho);case 114:return E(_hr);case 116:return E(_hn);case 118:return E(_hp);default:return [2];}}],new T(function(){return B(_9C([1,B(_aN(_di,_dl,function(_ht){return [1,B(_bn(_ht,function(_hu){var _hv=B(_cj(new T(function(){return B(_c9(E(_ht)[1]));}),_c8,_hu));return !B(_ds(_hv,_hg))?[2]:B(A(_hk,[new T(function(){var _hw=B(_dp(_hv));if(_hw>>>0>1114111){var _hx=B(_dn(_hw));}else{var _hx=[0,_hw];}var _hy=_hx,_hz=_hy,_hA=_hz;return _hA;})]));}))];}))],new T(function(){return B(_9C([0,function(_hB){return E(E(_hB)[1])==94?E([0,function(_hC){switch(E(E(_hC)[1])){case 64:return E(new T(function(){return B(A(_hk,[_e9]));}));case 65:return E(new T(function(){return B(A(_hk,[_dX]));}));case 66:return E(new T(function(){return B(A(_hk,[_ee]));}));case 67:return E(new T(function(){return B(A(_hk,[_ej]));}));case 68:return E(new T(function(){return B(A(_hk,[_eo]));}));case 69:return E(new T(function(){return B(A(_hk,[_et]));}));case 70:return E(new T(function(){return B(A(_hk,[_ey]));}));case 71:return E(_hl);case 72:return E(_hm);case 73:return E(_hn);case 74:return E(_ho);case 75:return E(_hp);case 76:return E(_hq);case 77:return E(_hr);case 78:return E(new T(function(){return B(A(_hk,[_e2]));}));case 79:return E(new T(function(){return B(A(_hk,[_fc]));}));case 80:return E(new T(function(){return B(A(_hk,[_fh]));}));case 81:return E(new T(function(){return B(A(_hk,[_fm]));}));case 82:return E(new T(function(){return B(A(_hk,[_fr]));}));case 83:return E(new T(function(){return B(A(_hk,[_fw]));}));case 84:return E(new T(function(){return B(A(_hk,[_fB]));}));case 85:return E(new T(function(){return B(A(_hk,[_fG]));}));case 86:return E(new T(function(){return B(A(_hk,[_fL]));}));case 87:return E(new T(function(){return B(A(_hk,[_fQ]));}));case 88:return E(new T(function(){return B(A(_hk,[_fV]));}));case 89:return E(new T(function(){return B(A(_hk,[_g0]));}));case 90:return E(new T(function(){return B(A(_hk,[_g5]));}));case 91:return E(new T(function(){return B(A(_hk,[_ga]));}));case 92:return E(new T(function(){return B(A(_hk,[_gf]));}));case 93:return E(new T(function(){return B(A(_hk,[_gk]));}));case 94:return E(new T(function(){return B(A(_hk,[_gp]));}));case 95:return E(new T(function(){return B(A(_hk,[_gu]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_hf,[_hk]));})));})));}));});},_hD=function(_hE){return new F(function(){return A(_hE,[_8]);});},_hF=function(_hG){var _hH=E(_hG);if(!_hH[0]){return E(_hD);}else{var _hI=_hH[2],_hJ=E(E(_hH[1])[1]);switch(_hJ){case 9:return function(_hK){return [0,function(_hL){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hK]));}));}];};case 10:return function(_hM){return [0,function(_hN){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hM]));}));}];};case 11:return function(_hO){return [0,function(_hP){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hO]));}));}];};case 12:return function(_hQ){return [0,function(_hR){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hQ]));}));}];};case 13:return function(_hS){return [0,function(_hT){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hS]));}));}];};case 32:return function(_hU){return [0,function(_hV){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hU]));}));}];};case 160:return function(_hW){return [0,function(_hX){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_hW]));}));}];};default:var _hY=u_iswspace(_hJ),_hZ=_hY;return E(_hZ)==0?E(_hD):function(_i0){return [0,function(_i1){return E(new T(function(){return B(A(new T(function(){return B(_hF(_hI));}),[_i0]));}));}];};}}},_i2=function(_i3){var _i4=new T(function(){return B(_i2(_i3));}),_i5=[1,function(_i6){return new F(function(){return A(_hF,[_i6,function(_i7){return E([0,function(_i8){return E(E(_i8)[1])==92?E(_i4):[2];}]);}]);});}];return new F(function(){return _9C([0,function(_i9){return E(E(_i9)[1])==92?E([0,function(_ia){var _ib=E(E(_ia)[1]);switch(_ib){case 9:return E(_i5);case 10:return E(_i5);case 11:return E(_i5);case 12:return E(_i5);case 13:return E(_i5);case 32:return E(_i5);case 38:return E(_i4);case 160:return E(_i5);default:var _ic=u_iswspace(_ib),_id=_ic;return E(_id)==0?[2]:E(_i5);}}]):[2];}],[0,function(_ie){var _if=E(_ie);return E(_if[1])==92?E(new T(function(){return B(_hj(function(_ig){return new F(function(){return A(_i3,[[0,_ig,_dc]]);});}));})):B(A(_i3,[[0,_if,_G]]));}]);});},_ih=function(_ii,_ij){return new F(function(){return _i2(function(_ik){var _il=E(_ik),_im=E(_il[1]);if(E(_im[1])==34){if(!E(_il[2])){return E(new T(function(){return B(A(_ij,[[1,new T(function(){return B(A(_ii,[_W]));})]]));}));}else{return new F(function(){return _ih(function(_in){return new F(function(){return A(_ii,[[1,_im,_in]]);});},_ij);});}}else{return new F(function(){return _ih(function(_io){return new F(function(){return A(_ii,[[1,_im,_io]]);});},_ij);});}});});},_ip=new T(function(){return B(unCStr("_\'"));}),_iq=function(_ir){var _is=u_iswalnum(_ir),_it=_is;return E(_it)==0?B(_cS(_ak,[0,_ir],_ip)):true;},_iu=function(_iv){return new F(function(){return _iq(E(_iv)[1]);});},_iw=new T(function(){return B(unCStr(",;()[]{}`"));}),_ix=new T(function(){return B(unCStr(".."));}),_iy=new T(function(){return B(unCStr("::"));}),_iz=new T(function(){return B(unCStr("->"));}),_iA=[0,64],_iB=[1,_iA,_W],_iC=[0,126],_iD=[1,_iC,_W],_iE=new T(function(){return B(unCStr("=>"));}),_iF=[1,_iE,_W],_iG=[1,_iD,_iF],_iH=[1,_iB,_iG],_iI=[1,_iz,_iH],_iJ=new T(function(){return B(unCStr("<-"));}),_iK=[1,_iJ,_iI],_iL=[0,124],_iM=[1,_iL,_W],_iN=[1,_iM,_iK],_iO=[1,_dk,_W],_iP=[1,_iO,_iN],_iQ=[0,61],_iR=[1,_iQ,_W],_iS=[1,_iR,_iP],_iT=[1,_iy,_iS],_iU=[1,_ix,_iT],_iV=function(_iW){return new F(function(){return _9C([1,function(_iX){return E(_iX)[0]==0?E(new T(function(){return B(A(_iW,[_bk]));})):[2];}],new T(function(){return B(_9C([0,function(_iY){return E(E(_iY)[1])==39?E([0,function(_iZ){var _j0=E(_iZ);switch(E(_j0[1])){case 39:return [2];case 92:return E(new T(function(){return B(_hj(function(_j1){return [0,function(_j2){return E(E(_j2)[1])==39?E(new T(function(){return B(A(_iW,[[0,_j1]]));})):[2];}];}));}));default:return [0,function(_j3){return E(E(_j3)[1])==39?E(new T(function(){return B(A(_iW,[[0,_j0]]));})):[2];}];}}]):[2];}],new T(function(){return B(_9C([0,function(_j4){return E(E(_j4)[1])==34?E(new T(function(){return B(_ih(_f,_iW));})):[2];}],new T(function(){return B(_9C([0,function(_j5){return !B(_cS(_ak,_j5,_iw))?[2]:B(A(_iW,[[2,[1,_j5,_W]]]));}],new T(function(){return B(_9C([0,function(_j6){return !B(_cS(_ak,_j6,_cX))?[2]:[1,B(_b9(_cY,function(_j7){var _j8=[1,_j6,_j7];return !B(_cS(_at,_j8,_iU))?B(A(_iW,[[4,_j8]])):B(A(_iW,[[2,_j8]]));}))];}],new T(function(){return B(_9C([0,function(_j9){var _ja=E(_j9),_jb=_ja[1],_jc=u_iswalpha(_jb),_jd=_jc;return E(_jd)==0?E(_jb)==95?[1,B(_b9(_iu,function(_je){return new F(function(){return A(_iW,[[3,[1,_ja,_je]]]);});}))]:[2]:[1,B(_b9(_iu,function(_jf){return new F(function(){return A(_iW,[[3,[1,_ja,_jf]]]);});}))];}],new T(function(){return [1,B(_aN(_da,_cO,_iW))];})));})));})));})));})));}));});},_jg=[0,0],_jh=function(_ji,_jj){return function(_jk){return new F(function(){return A(_hF,[_jk,function(_jl){return E(new T(function(){return B(_iV(function(_jm){var _jn=E(_jm);return _jn[0]==2?!B(_a9(_jn[1],_a8))?[2]:E(new T(function(){return B(A(_ji,[_jg,function(_jo){return [1,function(_jp){return new F(function(){return A(_hF,[_jp,function(_jq){return E(new T(function(){return B(_iV(function(_jr){var _js=E(_jr);return _js[0]==2?!B(_a9(_js[1],_a6))?[2]:E(new T(function(){return B(A(_jj,[_jo]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_jt=function(_ju,_jv,_jw){var _jx=function(_jy,_jz){return new F(function(){return _9C([1,function(_jA){return new F(function(){return A(_hF,[_jA,function(_jB){return E(new T(function(){return B(_iV(function(_jC){var _jD=E(_jC);if(_jD[0]==4){var _jE=E(_jD[1]);if(!_jE[0]){return new F(function(){return A(_ju,[_jD,_jy,_jz]);});}else{return E(E(_jE[1])[1])==45?E(_jE[2])[0]==0?E([1,function(_jF){return new F(function(){return A(_hF,[_jF,function(_jG){return E(new T(function(){return B(_iV(function(_jH){return new F(function(){return A(_ju,[_jH,_jy,function(_jI){return new F(function(){return A(_jz,[new T(function(){return [0, -E(_jI)[1]];})]);});}]);});}));}));}]);});}]):B(A(_ju,[_jD,_jy,_jz])):B(A(_ju,[_jD,_jy,_jz]));}}else{return new F(function(){return A(_ju,[_jD,_jy,_jz]);});}}));}));}]);});}],new T(function(){return [1,B(_jh(_jx,_jz))];}));});};return new F(function(){return _jx(_jv,_jw);});},_jJ=function(_jK,_jL){return [2];},_jM=function(_jN){var _jO=E(_jN);return _jO[0]==0?[1,new T(function(){return B(_cj(new T(function(){return B(_c9(E(_jO[1])[1]));}),_c8,_jO[2]));})]:E(_jO[2])[0]==0?E(_jO[3])[0]==0?[1,new T(function(){return B(_cj(_c7,_c8,_jO[1]));})]:[0]:[0];},_jP=function(_jQ){var _jR=E(_jQ);if(_jR[0]==5){var _jS=B(_jM(_jR[1]));return _jS[0]==0?E(_jJ):function(_jT,_jU){return new F(function(){return A(_jU,[new T(function(){return [0,B(_dp(_jS[1]))];})]);});};}else{return E(_jJ);}},_jV=function(_jW,_jX){return new F(function(){return _jt(_jP,_jW,_jX);});},_jY=[0,91],_jZ=[1,_jY,_W],_k0=function(_k1,_k2){var _k3=function(_k4,_k5){return [1,function(_k6){return new F(function(){return A(_hF,[_k6,function(_k7){return E(new T(function(){return B(_iV(function(_k8){var _k9=E(_k8);if(_k9[0]==2){var _ka=E(_k9[1]);if(!_ka[0]){return [2];}else{var _kb=_ka[2];switch(E(E(_ka[1])[1])){case 44:return E(_kb)[0]==0?!E(_k4)?[2]:E(new T(function(){return B(A(_k1,[_jg,function(_kc){return new F(function(){return _k3(_dc,function(_kd){return new F(function(){return A(_k5,[[1,_kc,_kd]]);});});});}]));})):[2];case 93:return E(_kb)[0]==0?E(new T(function(){return B(A(_k5,[_W]));})):[2];default:return [2];}}}else{return [2];}}));}));}]);});}];},_ke=function(_kf){return new F(function(){return _9C([1,function(_kg){return new F(function(){return A(_hF,[_kg,function(_kh){return E(new T(function(){return B(_iV(function(_ki){var _kj=E(_ki);return _kj[0]==2?!B(_a9(_kj[1],_jZ))?[2]:E(new T(function(){return B(_9C(B(_k3(_G,_kf)),new T(function(){return B(A(_k1,[_jg,function(_kk){return new F(function(){return _k3(_dc,function(_kl){return new F(function(){return A(_kf,[[1,_kk,_kl]]);});});});}]));})));})):[2];}));}));}]);});}],new T(function(){return [1,B(_jh(function(_km,_kn){return new F(function(){return _ke(_kn);});},_kf))];}));});};return new F(function(){return _ke(_k2);});},_ko=function(_kp,_kq){return new F(function(){return _k0(_jV,_kq);});},_kr=function(_ks){return function(_b6){return new F(function(){return _9s(new T(function(){return B(_jt(_jP,_ks,_aG));}),_b6);});};},_kt=new T(function(){return B(_k0(_jV,_aG));}),_ku=function(_jX){return new F(function(){return _9s(_kt,_jX);});},_kv=[0,_kr,_ku,_jV,_ko],_kw=function(_kx){return new F(function(){return _1J(0,E(_kx)[1],_W);});},_ky=function(_kz,_kA){return new F(function(){return _1J(0,E(_kz)[1],_kA);});},_kB=function(_kC,_kD){return new F(function(){return _7Q(_ky,_kC,_kD);});},_kE=function(_kF,_kG,_kH){return new F(function(){return _1J(E(_kF)[1],E(_kG)[1],_kH);});},_kI=[0,_kE,_kw,_kB],_kJ=new T(function(){return B(unCStr("GHC.Types"));}),_kK=new T(function(){return B(unCStr("Int"));}),_kL=new T(function(){var _kM=hs_wordToWord64(1521842780),_kN=_kM,_kO=hs_wordToWord64(1346191152),_kP=_kO;return [0,_kN,_kP,[0,_kN,_kP,_3a,_kJ,_kK],_W];}),_kQ=function(_kR){return E(_kL);},_kS=function(_kT){return E(E(_kT)[1]);},_kU=function(_kV){return E(E(_kV)[1]);},_kW=function(_kX){return E(E(_kX)[2]);},_kY=function(_kZ){return E(E(_kZ)[3]);},_l0=function(_l1,_l2){var _l3=new T(function(){return B(_kS(_l1));});return function(_l4){return new F(function(){return A(new T(function(){return B(_kU(_l3));}),[new T(function(){return B(A(_kW,[_l1,_l2]));}),function(_l5){return new F(function(){return A(new T(function(){return B(_kY(_l3));}),[[0,_l5,_l4]]);});}]);});};},_l6=function(_l7,_l8){return [0,_l7,function(_l9){return new F(function(){return _l0(_l8,_l9);});}];},_la=function(_lb,_lc,_ld,_le){return new F(function(){return A(_kU,[_lb,new T(function(){return B(A(_lc,[_le]));}),function(_lf){return new F(function(){return A(_ld,[new T(function(){return E(E(_lf)[1]);}),new T(function(){return E(E(_lf)[2]);})]);});}]);});},_lg=function(_lh,_li,_lj,_lk){return new F(function(){return A(_kU,[_lh,new T(function(){return B(A(_li,[_lk]));}),function(_ll){return new F(function(){return A(_lj,[new T(function(){return E(E(_ll)[2]);})]);});}]);});},_lm=function(_ln,_lo,_lp,_lq){return new F(function(){return _lg(_ln,_lo,_lp,_lq);});},_lr=function(_ls){return E(E(_ls)[4]);},_lt=function(_lu,_lv){return function(_lw){return E(new T(function(){return B(A(_lr,[_lu,_lv]));}));};},_lx=function(_ly){return [0,function(_lo,_lp,_lq){return new F(function(){return _la(_ly,_lo,_lp,_lq);});},function(_lo,_lp,_lq){return new F(function(){return _lm(_ly,_lo,_lp,_lq);});},function(_lz,_lA){return new F(function(){return A(new T(function(){return B(_kY(_ly));}),[[0,_lz,_lA]]);});},function(_lq){return new F(function(){return _lt(_ly,_lq);});}];},_lB=function(_lC,_lD,_lE){return new F(function(){return A(_kY,[_lC,[0,_lD,_lE]]);});},_lF=function(_lG){return E(E(_lG)[1]);},_lH=[0,10],_lI=function(_lJ,_lK){var _lL=E(_lK);if(!_lL[0]){return E(_f);}else{var _lM=_lL[1],_lN=E(_lL[2]);if(!_lN[0]){var _lO=E(_lM);return new F(function(){return _lP(_lH,_lO[3],_lO[4]);});}else{return function(_lQ){return new F(function(){return A(new T(function(){var _lR=E(_lM);return B(_lP(_lH,_lR[3],_lR[4]));}),[new T(function(){return B(A(_lJ,[new T(function(){return B(A(new T(function(){return B(_lI(_lJ,_lN));}),[_lQ]));})]));})]);});};}}},_lS=new T(function(){return B(unCStr("(->)"));}),_lT=new T(function(){return B(unCStr("GHC.Prim"));}),_lU=new T(function(){var _lV=hs_wordToWord64(4173248105),_lW=_lV,_lX=hs_wordToWord64(4270398258),_lY=_lX;return [0,_lW,_lY,[0,_lW,_lY,_3a,_lT,_lS],_W];}),_lZ=new T(function(){return E(E(_lU)[3]);}),_m0=new T(function(){return B(unCStr("[]"));}),_m1=new T(function(){var _m2=hs_wordToWord64(4033920485),_m3=_m2,_m4=hs_wordToWord64(786266835),_m5=_m4;return [0,_m3,_m5,[0,_m3,_m5,_3a,_kJ,_m0],_W];}),_m6=[1,_3b,_W],_m7=function(_m8){var _m9=E(_m8);if(!_m9[0]){return [0];}else{var _ma=E(_m9[1]);return [1,[0,_ma[1],_ma[2]],new T(function(){return B(_m7(_m9[2]));})];}},_mb=new T(function(){var _mc=E(_m1),_md=E(_mc[3]),_me=B(_1l(_mc[4],_m6));if(!_me[0]){var _mf=E(_md);}else{var _mg=B(_3F(new T(function(){return B(_3X(B(_40(_4f,[1,[0,_md[1],_md[2]],new T(function(){return B(_m7(_me));})]))));}))),_mf=E(_md);}var _mh=_mf,_mi=_mh;return _mi;}),_mj=[0,8],_mk=[0,32],_ml=function(_mm){return [1,_mk,_mm];},_mn=new T(function(){return B(unCStr(" -> "));}),_mo=[0,9],_mp=[0,93],_mq=[0,91],_mr=[0,41],_ms=[0,44],_mt=function(_mm){return [1,_ms,_mm];},_mu=[0,40],_mv=[0,0],_lP=function(_mw,_mx,_my){var _mz=E(_my);if(!_mz[0]){return function(_mA){return new F(function(){return _1l(E(_mx)[5],_mA);});};}else{var _mB=_mz[1],_mC=function(_mD){var _mE=E(_mx)[5],_mF=function(_mG){var _mH=new T(function(){return B(_lI(_ml,_mz));});return E(_mw)[1]<=9?function(_mI){return new F(function(){return _1l(_mE,[1,_mk,new T(function(){return B(A(_mH,[_mI]));})]);});}:function(_mJ){return [1,_1I,new T(function(){return B(_1l(_mE,[1,_mk,new T(function(){return B(A(_mH,[[1,_1H,_mJ]]));})]));})];};},_mK=E(_mE);if(!_mK[0]){return new F(function(){return _mF(_);});}else{if(E(E(_mK[1])[1])==40){var _mL=E(_mK[2]);if(!_mL[0]){return new F(function(){return _mF(_);});}else{if(E(E(_mL[1])[1])==44){return function(_mM){return [1,_mu,new T(function(){return B(A(new T(function(){return B(_lI(_mt,_mz));}),[[1,_mr,_mM]]));})];};}else{return new F(function(){return _mF(_);});}}}else{return new F(function(){return _mF(_);});}}},_mN=E(_mz[2]);if(!_mN[0]){var _mO=E(_mx),_mP=E(_mb),_mQ=hs_eqWord64(_mO[1],_mP[1]),_mR=_mQ;if(!E(_mR)){return new F(function(){return _mC(_);});}else{var _mS=hs_eqWord64(_mO[2],_mP[2]),_mT=_mS;if(!E(_mT)){return new F(function(){return _mC(_);});}else{return function(_mU){return [1,_mq,new T(function(){return B(A(new T(function(){var _mV=E(_mB);return B(_lP(_mv,_mV[3],_mV[4]));}),[[1,_mp,_mU]]));})];};}}}else{if(!E(_mN[2])[0]){var _mW=E(_mx),_mX=E(_lZ),_mY=hs_eqWord64(_mW[1],_mX[1]),_mZ=_mY;if(!E(_mZ)){return new F(function(){return _mC(_);});}else{var _n0=hs_eqWord64(_mW[2],_mX[2]),_n1=_n0;if(!E(_n1)){return new F(function(){return _mC(_);});}else{var _n2=new T(function(){var _n3=E(_mN[1]);return B(_lP(_mj,_n3[3],_n3[4]));}),_n4=new T(function(){var _n5=E(_mB);return B(_lP(_mo,_n5[3],_n5[4]));});return E(_mw)[1]<=8?function(_n6){return new F(function(){return A(_n4,[new T(function(){return B(_1l(_mn,new T(function(){return B(A(_n2,[_n6]));})));})]);});}:function(_n7){return [1,_1I,new T(function(){return B(A(_n4,[new T(function(){return B(_1l(_mn,new T(function(){return B(A(_n2,[[1,_1H,_n7]]));})));})]));})];};}}}else{return new F(function(){return _mC(_);});}}}},_n8=function(_n9,_na){return new F(function(){return A(_n9,[function(_){return new F(function(){return jsFind(toJSStr(E(_na)));});}]);});},_nb=[0],_nc=function(_nd){return E(E(_nd)[3]);},_ne=new T(function(){return [0,"value"];}),_nf=function(_ng){return E(E(_ng)[6]);},_nh=function(_ni){return E(E(_ni)[1]);},_nj=new T(function(){return B(unCStr("Char"));}),_nk=new T(function(){var _nl=hs_wordToWord64(3763641161),_nm=_nl,_nn=hs_wordToWord64(1343745632),_no=_nn;return [0,_nm,_no,[0,_nm,_no,_3a,_kJ,_nj],_W];}),_np=function(_nq){return E(_nk);},_nr=function(_ns){return E(_m1);},_nt=new T(function(){return B(_4i(_nr,_np));}),_nu=new T(function(){return B(A(_nt,[_]));}),_nv=function(_nw,_nx,_ny,_nz,_nA,_nB,_nC,_nD,_nE){var _nF=new T(function(){return B(A(_nz,[_nb]));});return new F(function(){return A(_nx,[new T(function(){return B(_n8(E(_nw)[2],_nE));}),function(_nG){var _nH=E(_nG);return _nH[0]==0?E(_nF):B(A(_nx,[new T(function(){return B(A(E(_nw)[2],[function(_){var _nI=jsGet(E(_nH[1])[1],E(_ne)[1]),_nJ=_nI;return [1,new T(function(){return fromJSStr(_nJ);})];}]));}),function(_nK){var _nL=E(_nK);if(!_nL[0]){return E(_nF);}else{var _nM=_nL[1];if(!E(new T(function(){var _nN=B(A(_nB,[_])),_nO=E(_nu),_nP=hs_eqWord64(_nN[1],_nO[1]),_nQ=_nP;if(!E(_nQ)){var _nR=false;}else{var _nS=hs_eqWord64(_nN[2],_nO[2]),_nT=_nS,_nR=E(_nT)==0?false:true;}var _nU=_nR,_nV=_nU;return _nV;}))){var _nW=function(_nX){return new F(function(){return A(_nz,[[1,_nM,new T(function(){return B(A(new T(function(){return B(_nf(_nD));}),[new T(function(){return B(A(new T(function(){return B(_nc(_nD));}),[new T(function(){return B(unAppCStr("can\'t read \"",new T(function(){return B(_1l(_nM,new T(function(){return B(unAppCStr("\" as type ",new T(function(){var _nY=B(A(_nB,[_]));return B(A(_lP,[_mv,_nY[3],_nY[4],_W]));})));})));})));})]));})]));})]]);});},_nZ=B(A(new T(function(){return B(A(_nh,[_nC,_O]));}),[_nM]));if(!_nZ[0]){return new F(function(){return _nW(_);});}else{var _o0=E(_nZ[1]);return E(_o0[2])[0]==0?E(_nZ[2])[0]==0?B(A(_nz,[[2,_o0[1]]])):B(_nW(_)):B(_nW(_));}}else{return new F(function(){return A(_nz,[[2,_nM]]);});}}}]));}]);});},_o1=1,_o2=function(_o3){return E(E(_o3)[9]);},_o4=function(_o5){return E(E(_o5)[2]);},_o6=function(_o7){return E(E(_o7)[2]);},_o8=function(_o9,_oa,_ob){var _oc=E(_ob);if(!_oc[0]){return [0];}else{var _od=_oc[1],_oe=B(A(_o9,[_])),_of=E(_nu),_og=hs_eqWord64(_oe[1],_of[1]),_oh=_og;if(!E(_oh)){return new F(function(){return A(_o6,[_oa,_od]);});}else{var _oi=hs_eqWord64(_oe[2],_of[2]),_oj=_oi;return E(_oj)==0?B(A(_o6,[_oa,_od])):E(_od);}}},_ok=function(_ol,_om,_on,_oo,_op){var _oq=new T(function(){return B(_o2(_ol));}),_or=new T(function(){return B(_kS(_om));}),_os=new T(function(){return B(_kY(_or));}),_ot=new T(function(){return B(_kY(_or));}),_ou=new T(function(){return B(_kY(_or));}),_ov=new T(function(){return B(_kY(_or));});return function(_ow,_ox,_oy){return function(_oz){return new F(function(){return A(new T(function(){return B(_kU(_or));}),[new T(function(){var _oA=E(_ow);return _oA[0]==0?B(A(new T(function(){return B(_kU(_or));}),[new T(function(){return B(A(_ot,[[0,_oz,_oz]]));}),function(_oB){var _oC=new T(function(){return E(E(_oB)[1]);}),_oD=new T(function(){return E(E(_oC)[2]);});return new F(function(){return A(new T(function(){return B(_kU(_or));}),[new T(function(){return B(A(_ou,[[0,_8,new T(function(){var _oE=E(_oC);return [0,_oE[1],new T(function(){return [0,E(_oD)[1]+1|0];}),_oE[3],_oE[4],_oE[5],_oE[6]];})]]));}),function(_oF){return new F(function(){return A(_os,[[0,[1,_1B,new T(function(){return B(_1l(B(_1J(0,E(_oD)[1],_W)),new T(function(){return E(E(_oC)[1]);})));})],new T(function(){return E(E(_oF)[2]);})]]);});}]);});}])):B(A(_os,[[0,_oA[1],_oz]]));}),function(_oG){var _oH=new T(function(){return E(E(_oG)[1]);});return new F(function(){return A(new T(function(){return B(_kU(_or));}),[new T(function(){var _oI=new T(function(){return E(E(_oG)[2]);});return B(A(_ot,[[0,_oI,_oI]]));}),function(_oJ){return new F(function(){return A(new T(function(){return B(_kU(_or));}),[new T(function(){return B(A(_ou,[[0,_8,new T(function(){var _oK=E(E(_oJ)[1]);return [0,_oK[1],_oK[2],_o1,_oK[4],_oK[5],_oK[6]];})]]));}),function(_oL){return new F(function(){return A(new T(function(){return B(_kU(_or));}),[new T(function(){return B(A(new T(function(){return B(_nv(new T(function(){return B(_l6(new T(function(){return B(_lx(_or));}),_om));}),function(_oM,_x,_oN){return new F(function(){return _la(_or,_oM,_x,_oN);});},function(_oM,_x,_oN){return new F(function(){return _lm(_or,_oM,_x,_oN);});},function(_x,_oN){return new F(function(){return _lB(_or,_x,_oN);});},function(_oN){return new F(function(){return _lt(_or,_oN);});},_on,_op,_ol,_oH));}),[new T(function(){return E(E(_oL)[2]);})]));}),function(_oO){var _oP=E(_oO),_oQ=_oP[2],_oR=E(_oP[1]);switch(_oR[0]){case 0:return new F(function(){return A(_ov,[[0,[0,new T(function(){return B(A(_oq,[_oH,_ox,new T(function(){return B(_o8(_on,_oo,_oy));}),_G,_P]));}),_P],_oQ]]);});break;case 1:return new F(function(){return A(_ov,[[0,[0,new T(function(){return B(A(new T(function(){return B(_o4(new T(function(){return B(_lF(_ol));})));}),[new T(function(){return B(A(_oq,[_oH,_ox,_oR[1],_G,_P]));}),_oR[2]]));}),_P],_oQ]]);});break;default:var _oS=_oR[1];return new F(function(){return A(_ov,[[0,[0,new T(function(){return B(A(_oq,[_oH,_ox,new T(function(){return B(_o8(_on,_oo,[1,_oS]));}),_G,_P]));}),[1,_oS]],_oQ]]);});}}]);});}]);});}]);});}]);});};};},_oT=new T(function(){return B(_ok(_6B,_8w,_kQ,_kI,_kv));}),_oU=new T(function(){return B(A(_oT,[_P,_2Z,_P]));}),_oV=new T(function(){return B(unCStr("keydown"));}),_oW=new T(function(){return B(unCStr("mousemove"));}),_oX=new T(function(){return B(unCStr("blur"));}),_oY=new T(function(){return B(unCStr("focus"));}),_oZ=new T(function(){return B(unCStr("change"));}),_p0=new T(function(){return B(unCStr("unload"));}),_p1=new T(function(){return B(unCStr("load"));}),_p2=new T(function(){return B(unCStr("keyup"));}),_p3=new T(function(){return B(unCStr("keypress"));}),_p4=new T(function(){return B(unCStr("mouseup"));}),_p5=new T(function(){return B(unCStr("mousedown"));}),_p6=new T(function(){return B(unCStr("dblclick"));}),_p7=new T(function(){return B(unCStr("click"));}),_p8=new T(function(){return B(unCStr("mouseout"));}),_p9=new T(function(){return B(unCStr("mouseover"));}),_pa=function(_pb){switch(E(_pb)[0]){case 0:return E(_p1);case 1:return E(_p0);case 2:return E(_oZ);case 3:return E(_oY);case 4:return E(_oX);case 5:return E(_oW);case 6:return E(_p9);case 7:return E(_p8);case 8:return E(_p7);case 9:return E(_p6);case 10:return E(_p5);case 11:return E(_p4);case 12:return E(_p3);case 13:return E(_p2);default:return E(_oV);}},_pc=[0],_pd=new T(function(){return B(unCStr("true"));}),_pe=new T(function(){return [0,"keydown"];}),_pf=new T(function(){return [0,"mousemove"];}),_pg=new T(function(){return [0,"blur"];}),_ph=new T(function(){return [0,"focus"];}),_pi=new T(function(){return [0,"change"];}),_pj=new T(function(){return [0,"unload"];}),_pk=new T(function(){return [0,"load"];}),_pl=new T(function(){return [0,"keyup"];}),_pm=new T(function(){return [0,"keypress"];}),_pn=new T(function(){return [0,"mouseup"];}),_po=new T(function(){return [0,"mousedown"];}),_pp=new T(function(){return [0,"dblclick"];}),_pq=new T(function(){return [0,"click"];}),_pr=new T(function(){return [0,"mouseout"];}),_ps=new T(function(){return [0,"mouseover"];}),_pt=function(_pu){switch(E(_pu)[0]){case 0:return E(_pk);case 1:return E(_pj);case 2:return E(_pi);case 3:return E(_ph);case 4:return E(_pg);case 5:return E(_pf);case 6:return E(_ps);case 7:return E(_pr);case 8:return E(_pq);case 9:return E(_pp);case 10:return E(_po);case 11:return E(_pn);case 12:return E(_pm);case 13:return E(_pl);default:return E(_pe);}},_pv=function(_pw,_px,_py,_pz,_){var _pA=B(A(_pw,[_pz,_])),_pB=_pA,_pC=E(_pB),_pD=_pC[1],_pE=B(_pa(_px)),_pF=jsGetAttr(_pD,toJSStr(_pE)),_pG=_pF;if(!B(_a9(fromJSStr(_pG),_pd))){var _pH=E(_py),_pI=jsSetCB(_pD,B(_pt(_px))[1],_py),_pJ=_pI,_pK=B(A(_9,[_f,_pC,_pE,_pd,_])),_pL=_pK;return _pC;}else{return _pC;}},_pM=new T(function(){return B(unCStr("OnLoad"));}),_pN=[0,_pM,_pc],_pO=function(_){var _=0,_pP=newMVar(),_pQ=_pP,_=putMVar(_pQ,_pN);return [0,_pQ];},_pR=new T(function(){return B(_H(_pO));}),_pS=function(_pT,_){var _pU=0,_pV=_pU;if(!E(_pV)){return new F(function(){return (function(_){var _pW=E(_pR)[1],_pX=takeMVar(_pW),_pY=_pX,_pZ=jsCatch(function(_){return new F(function(){return (function(_){return _pT;})();});},function(_q0,_){var _=putMVar(_pW,_pY);return new F(function(){return die(_q0);});}),_q1=_pZ,_=putMVar(_pW,_q1);return _8;})();});}else{var _q2=E(_pR)[1],_q3=takeMVar(_q2),_q4=_q3,_q5=jsCatch(function(_){return _pT;},function(_q6,_){var _=putMVar(_q2,_q4);return new F(function(){return die(_q6);});}),_q7=_q5,_=putMVar(_q2,_q7);return _8;}},_q8=function(_q9,_qa){var _qb=new T(function(){return B(_pa(_qa));}),_qc=[0,_qb,_pc];return function(_qd,_){var _qe=E(_qd),_qf=E(_qe[4]),_qg=_qf[1],_qh=_qf[2],_qi=B(A(_q9,[_qe,_])),_qj=_qi,_qk=E(_qj),_ql=E(_qk[1]),_qm=_ql[1];return [0,[0,new T(function(){var _qn=E(_qa);switch(_qn[0]){case 0:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_){var _qq=B(_pS(_qc,_)),_qr=_qq,_qs=B(A(_qg,[_])),_qt=_qs,_qu=E(_qt);if(!_qu[0]){return _8;}else{var _qv=B(A(_qh,[_qu[1],_])),_qw=_qv;return _8;}},_b6,_qp);});};break;case 1:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_){var _qx=B(_pS(_qc,_)),_qy=_qx,_qz=B(A(_qg,[_])),_qA=_qz,_qB=E(_qA);if(!_qB[0]){return _8;}else{var _qC=B(A(_qh,[_qB[1],_])),_qD=_qC;return _8;}},_b6,_qp);});};break;case 2:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_){var _qE=B(_pS(_qc,_)),_qF=_qE,_qG=B(A(_qg,[_])),_qH=_qG,_qI=E(_qH);if(!_qI[0]){return _8;}else{var _qJ=B(A(_qh,[_qI[1],_])),_qK=_qJ;return _8;}},_b6,_qp);});};break;case 3:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_){var _qL=B(_pS(_qc,_)),_qM=_qL,_qN=B(A(_qg,[_])),_qO=_qN,_qP=E(_qO);if(!_qP[0]){return _8;}else{var _qQ=B(A(_qh,[_qP[1],_])),_qR=_qQ;return _8;}},_b6,_qp);});};break;case 4:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_){var _qS=B(_pS(_qc,_)),_qT=_qS,_qU=B(A(_qg,[_])),_qV=_qU,_qW=E(_qV);if(!_qW[0]){return _8;}else{var _qX=B(A(_qh,[_qW[1],_])),_qY=_qX;return _8;}},_b6,_qp);});};break;case 5:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_qZ,_){var _r0=B(_pS([0,_qb,[2,E(_qZ)]],_)),_r1=_r0,_r2=B(A(_qg,[_])),_r3=_r2,_r4=E(_r3);if(!_r4[0]){return _8;}else{var _r5=B(A(_qh,[_r4[1],_])),_r6=_r5;return _8;}},_b6,_qp);});};break;case 6:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_r7,_){var _r8=B(_pS([0,_qb,[2,E(_r7)]],_)),_r9=_r8,_ra=B(A(_qg,[_])),_rb=_ra,_rc=E(_rb);if(!_rc[0]){return _8;}else{var _rd=B(A(_qh,[_rc[1],_])),_re=_rd;return _8;}},_b6,_qp);});};break;case 7:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_){var _rf=B(A(_qg,[_])),_rg=_rf,_rh=E(_rg);if(!_rh[0]){return _8;}else{var _ri=B(A(_qh,[_rh[1],_])),_rj=_ri;return _8;}},_b6,_qp);});};break;case 8:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_rk,_rl,_){var _rm=B(_pS([0,_qb,[1,_rk,E(_rl)]],_)),_rn=_rm,_ro=B(A(_qg,[_])),_rp=_ro,_rq=E(_rp);if(!_rq[0]){return _8;}else{var _rr=B(A(_qh,[_rq[1],_])),_rs=_rr;return _8;}},_b6,_qp);});};break;case 9:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_rt,_ru,_){var _rv=B(_pS([0,_qb,[1,_rt,E(_ru)]],_)),_rw=_rv,_rx=B(A(_qg,[_])),_ry=_rx,_rz=E(_ry);if(!_rz[0]){return _8;}else{var _rA=B(A(_qh,[_rz[1],_])),_rB=_rA;return _8;}},_b6,_qp);});};break;case 10:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_rC,_rD,_){var _rE=B(_pS([0,_qb,[1,_rC,E(_rD)]],_)),_rF=_rE,_rG=B(A(_qg,[_])),_rH=_rG,_rI=E(_rH);if(!_rI[0]){return _8;}else{var _rJ=B(A(_qh,[_rI[1],_])),_rK=_rJ;return _8;}},_b6,_qp);});};break;case 11:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_rL,_rM,_){var _rN=B(_pS([0,_qb,[1,_rL,E(_rM)]],_)),_rO=_rN,_rP=B(A(_qg,[_])),_rQ=_rP,_rR=E(_rQ);if(!_rR[0]){return _8;}else{var _rS=B(A(_qh,[_rR[1],_])),_rT=_rS;return _8;}},_b6,_qp);});};break;case 12:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_rU,_){var _rV=B(_pS([0,_qb,[3,_rU]],_)),_rW=_rV,_rX=B(A(_qg,[_])),_rY=_rX,_rZ=E(_rY);if(!_rZ[0]){return _8;}else{var _s0=B(A(_qh,[_rZ[1],_])),_s1=_s0;return _8;}},_b6,_qp);});};break;case 13:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_s2,_){var _s3=B(_pS([0,_qb,[3,_s2]],_)),_s4=_s3,_s5=B(A(_qg,[_])),_s6=_s5,_s7=E(_s6);if(!_s7[0]){return _8;}else{var _s8=B(A(_qh,[_s7[1],_])),_s9=_s8;return _8;}},_b6,_qp);});};break;default:var _qo=function(_b6,_qp){return new F(function(){return _pv(_qm,_qn,function(_sa,_){var _sb=B(_pS([0,_qb,[3,_sa]],_)),_sc=_sb,_sd=B(A(_qg,[_])),_se=_sd,_sf=E(_se);if(!_sf[0]){return _8;}else{var _sg=B(A(_qh,[_sf[1],_])),_sh=_sg;return _8;}},_b6,_qp);});};}return _qo;}),_ql[2]],_qk[2]];};},_si=new T(function(){return B(_q8(_oU,_2Y));}),_sj=[1,_8],_sk=function(_sl){return function(_b6,_qp){return new F(function(){return _1W(_si,function(_sm,_sn,_){var _so=E(E(_sm)[1]);if(!_so){return [0,[0,function(_sp,_){var _sq=B(_2S(_sp,_)),_sr=_sq,_ss=B(_0(_2X,_sp,_)),_st=_ss,_su=B(_2H(_0,new T(function(){return B(_1J(0,_sl,_W));}),_sp,_)),_sv=_su;return _sp;},_P],_sn];}else{return new F(function(){return _1W(function(_sw,_){return [0,[0,function(_sx,_){var _sy=B(_2H(_0,new T(function(){return B(_1J(0,_sl+_so|0,_W));}),_sx,_)),_sz=_sy,_sA=B(_2S(_sx,_)),_sB=_sA;return _sx;},_sj],_sw];},function(_sC){return E(new T(function(){return B(_sk(_sl+_so|0));}));},_sn,_);});}},_b6,_qp);});};},_sD=[0,112],_sE=[1,_sD,_W],_sF=function(_sG,_sH,_sI,_){var _sJ=jsCreateElem(toJSStr(_sE)),_sK=_sJ,_sL=jsAppendChild(_sK,E(_sI)[1]),_sM=[0,_sK],_sN=B(A(_sG,[_sH,_sM,_])),_sO=_sN;return _sM;},_sP=function(_sQ,_){var _sR=B(A(_sk,[0,_sQ,_])),_sS=_sR,_sT=E(_sS),_sU=E(_sT[1]);return [0,[0,function(_sV,_){var _sW=B(_sF(_0,_6,_sV,_)),_sX=_sW,_sY=B(A(_sU[1],[_sV,_])),_sZ=_sY;return _sV;},_sU[2]],_sT[2]];},_t0=new T(function(){return B(unCStr("(function(){return document.body;})"));}),_t1=function(_t2,_){var _t3=B(A(_L,[toJSStr(E(_t0)),_])),_t4=_t3;return new F(function(){return _12(_t2,[0,_t4],_);});},_t5=function(_){return new F(function(){return _t1(_sP,_);});},_t6=function(_){return new F(function(){return _t5(_);});};
var hasteMain = function() {B(A(_t6, [0]));};window.onload = hasteMain;