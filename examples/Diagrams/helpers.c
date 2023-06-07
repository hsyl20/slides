#define FNV_PRIME 16777619
#define FNV_SIGNED int32_t
#define FNV_UNSIGNED uint32_t

FNV_UNSIGNED hashable_fnv_hash(const unsigned char* str, FNV_SIGNED len, FNV_UNSIGNED salt) {

  FNV_UNSIGNED hash = salt;
  while (len--) {
    hash = (hash * FNV_PRIME) ^ *str++;
  }

  return hash;
}

/* Used for ByteArray#s. We can't treat them like pointers in
   native Haskell, but we can in unsafe FFI calls.
 */
FNV_UNSIGNED hashable_fnv_hash_offset(const unsigned char* str, FNV_SIGNED offset, FNV_SIGNED len, FNV_UNSIGNED salt) {
  return hashable_fnv_hash(str + offset, len, salt);
}

int bytestring_is_valid_utf8(uint8_t const *const src, size_t const len);
