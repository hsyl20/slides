#define FNV_PRIME 16777619

uint32_t hashable_fnv_hash_offset(const unsigned char* str, int32_t offset, int32_t len, uint32_t salt) {
  return hashable_fnv_hash(str + offset, len, salt);
}

uint32_t hashable_fnv_hash(const unsigned char* str, int32_t len, uint32_t salt) {
  uint32_t hash = salt;
  while (len--) {
    hash = (hash * FNV_PRIME) ^ *str++;
  }
  return hash;
}




int bytestring_is_valid_utf8(uint8_t const *const src, size_t const len);
