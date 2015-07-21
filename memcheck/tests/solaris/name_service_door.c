/* Tests for name switch cache daemon (nscd) door wrapper. */

#include "config.h"

#include <assert.h>
#include <ctype.h>
#include <door.h>
#include <fcntl.h>
#include <inttypes.h>
#include <nss_dbdefs.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>
#include <sys/mman.h>

#if defined(SOLARIS_NSCD_DOOR_SYSTEM_VOLATILE)
#define DOOR_FILE "/system/volatile/name_service_door"
#else
#define DOOR_FILE "/var/run/name_service_door"
#endif

#define HEADER(file, test_name) \
   fprintf(file, "---------------------------------------------------------\n"  \
                 "%s\n"                                                         \
                 "---------------------------------------------------------\n", \
                 test_name);


static long x0;

/* It's possible that the system allocated a new memory for rbuf.
   Unmap it if it is the case. */
static int handle_rbuf(door_arg_t *params, void *buf)
{
   if (params->rbuf != buf) {
      if (munmap(params->rbuf, params->rsize) != 0) {
         perror("munmap");
         return EINVAL;
      }
   }

   return 0;
}

__attribute__((noinline))
static int test_app_small_request(int did)
{
   /* Set call parameters. */
   size_t buf_size = sizeof(uint32_t);
   char *buf = malloc(buf_size);
   assert(buf != NULL);

   nss_pheader_t *header = (nss_pheader_t *) buf;
   header->nsc_callnumber = x0 + NSCD_GETENT;

   door_arg_t params;
   params.data_ptr = buf;
   params.data_size = buf_size;
   params.desc_ptr = NULL;
   params.desc_num = 0;
   params.rbuf = buf;
   params.rsize = buf_size;

   /* Make the call. */
   if (door_call(did, &params) != 0) {
      return errno;
   }

   return handle_rbuf(&params, buf);
}

__attribute__((noinline))
static int test_app_uninitialized_request(int did)
{
   /* Set call parameters. */
   size_t buf_size = sizeof(nss_pheader_t) + sizeof(nss_dbd_t);
   char *buf = malloc(buf_size);
   assert(buf != NULL);

   nss_pheader_t *header = (nss_pheader_t *) buf;
   header->nsc_callnumber = x0 + NSCD_GETENT;
   header->dbd_off = x0 + sizeof(nss_pheader_t);
   header->dbd_len = x0 + sizeof(nss_dbd_t);
   nss_dbd_t *dbd = (nss_dbd_t *) (buf + sizeof(nss_pheader_t));
   dbd->flags = x0;
   dbd->o_name = x0 + 100;
   dbd->o_config_name = x0 + 100;
   dbd->o_default_config = x0 + 100;
   header->key_off = x0 + sizeof(nss_pheader_t) + sizeof(nss_dbd_t);
   header->key_len = x0 + 1; // one byte past the end of 'buf'
   header->pbufsiz = x0 + 10000000;

   door_arg_t params;
   params.data_ptr = buf;
   params.data_size = buf_size;
   params.desc_ptr = NULL;
   params.desc_num = 0;
   params.rbuf = buf;
   params.rsize = buf_size;

   /* Make the call. */
   if (door_call(did, &params) != 0) {
      return errno;
   }

   /* Check definedness of response attributes ... */
   int x = 0;
   if (header->p_status != NSS_SUCCESS) x = -1; else x = -2;
   if (header->p_herrno != 0) x = -2; else x = -3;
   if (header->key_off != 0) x = -4; else x = -5;
   if (header->key_len != 0) x = -6; else x = -7;
   if (header->data_off != 0) x = -8; else x = -9;
   if (header->data_len != 0) x = -10; else x = -11;
   /* ... and now one which is not defined. */
   if (header->reserved1 != 0) x = -12; else x = -13;

   handle_rbuf(&params, buf);
   return x;
}

__attribute__((noinline))
static int test_app_proto_icmp(int did)
{
   door_arg_t params;
   char buf[16384];

   /* Set call parameters. */
   nss_pheader_t *header = (nss_pheader_t *) buf;
   header->nsc_callnumber = NSCD_SEARCH;
   header->p_ruid = getuid();
   header->p_euid = geteuid();
   header->p_version = NSCD_HEADER_REV;
   header->p_status = 0;
   header->p_errno = 0;
   header->p_herrno = 0;
   header->libpriv = 0;
   header->nss_dbop = NSS_DBOP_PROTOCOLS_BYNAME;

   size_t name_len = strlen(NSS_DBNAM_PROTOCOLS);
   size_t default_config_len = strlen(NSS_FILES_ONLY);
   header->dbd_off = sizeof(nss_pheader_t);
   header->dbd_len = sizeof(nss_dbd_t) + name_len + 1 + default_config_len + 1;
   nss_dbd_t *dbd = (nss_dbd_t *) (buf + sizeof(nss_pheader_t));
   dbd->o_name = sizeof(nss_dbd_t);
   dbd->o_config_name = 0;
   dbd->o_default_config = dbd->o_name + name_len + 1;
   dbd->flags = 0;
   strcpy(buf + header->dbd_off + dbd->o_name, NSS_DBNAM_PROTOCOLS);
   strcpy(buf + header->dbd_off + dbd->o_default_config,
          NSS_DEFCONF_PROTOCOLS);

   name_len = strlen("icmp");
   header->key_off = header->dbd_off + ROUND_UP(header->dbd_len, sizeof(nssuint_t));
   header->key_len = name_len + 1;
   strcpy(buf + header->key_off, "icmp");

   header->data_off = header->key_off + ROUND_UP(header->key_len, sizeof(nssuint_t));
   header->data_len = 0;
   header->pbufsiz = header->data_off + header->data_len;

   params.data_ptr = buf;
   params.data_size = header->pbufsiz;
   params.desc_ptr = NULL;
   params.desc_num = 0;
   params.rbuf = buf;
   params.rsize = sizeof(buf);

   /* Sanity checks on the nss_pheader_t header. */
   assert(header->p_version == NSCD_HEADER_REV);
   assert(header->dbd_off == sizeof(nss_pheader_t));
   assert((params.data_size & 3) == 0);
   assert((header->dbd_off & 3) == 0);
   assert((header->key_off & 3) == 0);
   assert((header->data_off & 3) == 0);
   assert(header->data_off == params.data_size);
   nssuint_t l1 = header->key_off - header-> dbd_off;
   assert(l1 >= header->dbd_len);
   nssuint_t l2 = header->data_off - header->key_off;
   assert(l2 >= header->key_len);
   assert(sizeof(nss_pheader_t) + l1 + l2 == header->data_off);
   assert(header->data_off + header->data_len == header->pbufsiz);

   /* Make the call. */
   if (door_call(did, &params) != 0) {
      return errno;
   }

   /* Print response attributes. */
   HEADER(stdout, "app_proto_icmp");
   printf("status=%u\n", header->p_status);
   printf("errno=%u\n", header->p_errno);
   printf("herrno=%u\n", header->p_herrno);
   printf("bufsiz=%" PRIu64 "\n", header->pbufsiz);
   printf("dbd_off=%" PRIu64 " dbd_len=%" PRIu64 "\n",
          header->dbd_off, header->dbd_len);
   printf("key_off=%" PRIu64 " key_len=%" PRIu64 "\n",
          header->key_off, header->key_len);
   printf("data_off=%" PRIu64 " data_len=%" PRIu64 "\n",
          header->data_off, header->data_len);
   printf("ext_off=%" PRIu64 " ext_len=%" PRIu64 "\n",
          header->ext_off, header->ext_len);
   printf("key=%s\n", buf + header->key_off);

   /* Parse response proto data. */
   char *p = buf + header->data_off;
   char *limit = p + header->data_len;

   while ((p < limit) && isspace(*p))
      p++;
   char *name_start = p;
   while ((p < limit) && !isspace(*p))
      p++; // skip over the name
   name_len = p - name_start;

   while ((p < limit) && isspace(*p))
      p++;
   char *number_start = p;
   do {
      p++; // skip over the proto number
   } while ((p < limit) && !isspace(*p));
   size_t number_len = p - number_start;

   while ((p < limit) && isspace(*p))
      p++;
   char *aliases_start = p;
   while ((p < limit) && !isspace(*p))
      p++; // skip over the aliases
   size_t aliases_len = p - aliases_start;

   printf("data: name=%.*s number=%.*s aliases=%.*s\n",
      (int) name_len, name_start, (int) number_len, number_start,
      (int) aliases_len, aliases_start);

   return handle_rbuf(&params, buf);
}

int main(int argc, const char *argv[])
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   int did = open(DOOR_FILE, O_RDONLY);
   if (did < 0) {
      perror("open " DOOR_FILE);
      fprintf(stderr, "Make sure the name service switch daemon (nscd) "
              "is running.\n");
      return 1;
   }

   struct door_info info;
   if (door_info(did, &info) != 0) {
      perror("door_info " DOOR_FILE);
      close(did);
      return 1;
   }

   HEADER(stderr, "app_small_request");
   test_app_small_request(did);

   HEADER(stderr, "app_uninitialized_request");
   test_app_uninitialized_request(did);

   HEADER(stderr, "app_proto_icmp");
   test_app_proto_icmp(did);

   close(did);

   return 0;
}

