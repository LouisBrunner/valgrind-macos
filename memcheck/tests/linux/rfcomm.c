#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include "../../memcheck.h"

/* user space headers might not be there, define things ourselves. */
typedef struct {
   uint8_t b[6];
} __attribute__((packed)) vui_bdaddr_t;

struct vui_sockaddr_rc {
	sa_family_t	rc_family;
	vui_bdaddr_t	rc_bdaddr;
	uint8_t		rc_channel;
};

#define VUI_AF_BLUETOOTH 31
#define VUI_BTPROTO_RFCOMM 3

#define VUI_BDADDR_ANY (&(vui_bdaddr_t) {{0, 0, 0, 0, 0, 0}})

int
main (int argc, char **argv)
{
  int nSocket;

  nSocket = socket(VUI_AF_BLUETOOTH, SOCK_STREAM, VUI_BTPROTO_RFCOMM);

  if (nSocket < 0)
    {
      // Not supported, nothing to test...
      return 1;
    }

  struct vui_sockaddr_rc aAddr;

  // Store correct values in aAddr but marking it undefined
  // so as to generate errors. We need to have deterministic
  // undefined values to have a reproducible test.
  aAddr.rc_family = VUI_AF_BLUETOOTH;
  aAddr.rc_bdaddr = *VUI_BDADDR_ANY;
  aAddr.rc_channel = 5;
  VALGRIND_MAKE_MEM_UNDEFINED(&aAddr, sizeof(aAddr));
  // We re-assign below each piece one by one, so as to
  // have the piece marked initialised.


  // Ignore return values.

  // Everything uninit (family, ...)
  bind(nSocket, (struct sockaddr *) &aAddr, sizeof(aAddr));

  // Same but with an unknown family (we hope :)
  aAddr.rc_family = 12345;
  // (reset everything to uninit)
  VALGRIND_MAKE_MEM_UNDEFINED(&aAddr, sizeof(aAddr));
  bind(nSocket, (struct sockaddr *) &aAddr, sizeof(aAddr));

  aAddr.rc_family = VUI_AF_BLUETOOTH;
  // uninit bdaddr and channel.
  bind(nSocket, (struct sockaddr *) &aAddr, sizeof(aAddr));

  aAddr.rc_bdaddr = *VUI_BDADDR_ANY;
  // uninit channel.
  bind(nSocket, (struct sockaddr *) &aAddr, sizeof(aAddr));

  aAddr.rc_channel = 5;
  // Everything correctly init.
  bind(nSocket, (struct sockaddr *) &aAddr, sizeof(aAddr));

  return 0;
}
