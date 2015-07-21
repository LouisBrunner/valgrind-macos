/* Test for PKCS#11 calls. */ 

#include <stdio.h>
#include <security/cryptoki.h>
#include <security/pkcs11.h>

int main(void)
{
   CK_RV ret = C_Initialize(NULL);
   if (ret != CKR_OK) {
      fprintf(stderr, "Initialize: %lu\n", ret);
      return 1;
   }

   CK_ULONG slot_count;
   ret = C_GetSlotList(0, NULL, &slot_count);
   if (ret != CKR_OK) {
      fprintf(stderr, "GetSlotList(NULL): %lu\n", ret);
      return 1;
   }

   CK_SLOT_ID_PTR slots = malloc(slot_count * sizeof(CK_SLOT_ID));
   if (slots == NULL) {
      fprintf(stderr, "malloc(slots)\n");
      return 1;
   }

   ret = C_GetSlotList(0, slots, &slot_count);
   if (ret != CKR_OK) {
      fprintf(stderr, "GetSlotList(slots): %lu\n", ret);
      return 1;
   }

   CK_ULONG i;
   for (i = 0; i < slot_count; i++) {
      CK_SLOT_ID slot_id = slots[i];

      CK_ULONG mech_count;
      ret = C_GetMechanismList(slot_id, NULL, &mech_count);
      if (ret != CKR_OK) {
         fprintf(stderr, "GetMechanismList(NULL): %lu\n", ret);
         return 1;
      }

      CK_MECHANISM_TYPE_PTR mechs = malloc(mech_count * sizeof(CK_MECHANISM_TYPE));
      if (slots == NULL) {
         fprintf(stderr, "malloc(mechs)\n");
         return 1;
      }

      ret = C_GetMechanismList(slot_id, mechs, &mech_count);
      if (ret != CKR_OK) {
         fprintf(stderr, "GetMechanismList(mechs): %lu\n", ret);
         return 1;
      }

      free(mechs);
   }

   free(slots);
   C_Finalize(NULL_PTR);
   return 0;
}

