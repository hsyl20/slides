//#OPTIONS: CPP

#define FNV_PRIME 16777619

function h$hashable_fnv_hash_offset(str_a, str_o, offset, len, salt) {
  return h$hashable_fnv_hash(str_a, str_o + offset, len, salt);
}

function h$hashable_fnv_hash_offset(str_a, str_o, len, salt) {
  var hash = salt;
  while (len--) {
    hash = ((hash * FNV_PRIME) ^ (str_a.buf[str_o++])) >>> 0;
  }
  return hash;
}







//function h$bytestring_is_valid_utf8(src_a, src_o, len) {
//  return valid8(src_a.buf.slice(src_o,src_o+len)) ? 1 : 0;
//}

function valid8(buffer) {
    var code, i, j, len, mask, mode, n;
    var maxBytes = 4;
    var surrogates = false;
    mode = 0;
    for (i = j = 0, len = buffer.length; j < len; i = ++j) {
      n = buffer[i];
      if (mode) {
        if (0x80 !== (0xC0 & n)) {
          return;
        }
        code = code << 6 | n & 0x3F;
        if (--mode) {
          continue;
        }
        if (maxBytes < 5 && code > 0x0010FFFF) {
          return;
        }
        if (!surrogates && (0xD800 <= code && code <= 0xDFFF)) {
          return;
        }
        if (!(code >> mask)) {
          return;
        }
        continue;
      }
      if (!(n & 0x80)) {
        continue;
      }
      if (n === 0xFF || n === 0xFE || n === 0xC0 || n === 0xC1) {
        return;
      }
      if (!(n & 0x40)) {
        return;
      }
      mode = 1;
      mask = 0x20;
      while (n & mask) {
        mask >>= 1;
        mode++;
      }
      if (mode >= maxBytes) {
        return;
      }
      code = n & mask - 1;
      mask = 5 * mode + 1;
    }
    if (mode) {
      return;
    }
    return true;
};

