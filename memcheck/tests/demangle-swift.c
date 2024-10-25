// Same principle as demangle-rust.c, but for Swift symbols.

#include <stdlib.h>

int $sSo13NSFileManagerC10FoundationE28withFileSystemRepresentation3for_xSS_xSPys4Int8VGSgKXEtKlFSDySo0A12AttributeKeyaypG_Tg5(int *p)
{
  free(p);
  free(p);
  return 1;
}

int $ss26_SwiftDeferredNSDictionaryC12bridgeValuess20__BridgingHashBufferCSgyF(int* p)
{
  return $sSo13NSFileManagerC10FoundationE28withFileSystemRepresentation3for_xSS_xSPys4Int8VGSgKXEtKlFSDySo0A12AttributeKeyaypG_Tg5(p);
}

int $sSS7cStringSSSPys4Int8VG_tcfC(int* p)
{
  return $ss26_SwiftDeferredNSDictionaryC12bridgeValuess20__BridgingHashBufferCSgyF(p);
}

int $s10Foundation16_FileManagerImplV16attributesOfItem6atPathSDySo18NSFileAttributeKeyaypGSS_tKFAHSPys4Int8VGSgKXEfU_TA(size_t s)
{
  return $sSS7cStringSSSPys4Int8VG_tcfC(malloc(s));
}

int _TtBv4Bf16_(void)
{
  return $s10Foundation16_FileManagerImplV16attributesOfItem6atPathSDySo18NSFileAttributeKeyaypGSS_tKFAHSPys4Int8VGSgKXEfU_TA(sizeof(int));
}

int $ss6SimpleHr(void)
{
  return _TtBv4Bf16_();
}

int main(void)
{
   return $ss6SimpleHr();
}
