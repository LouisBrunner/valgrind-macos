/* Basic ioctl scalar tests. */

#define __EXTENSIONS__ 1

#include "scalar.h"

#include <net/if.h>
#include <sys/crypto/ioctl.h>
#include <sys/dditypes.h>
#include <sys/devinfo_impl.h>
#include <sys/dtrace.h>
#include <sys/filio.h>
#include <sys/stat.h>		/* for _ST_FSTYPSZ */
#include <sys/mntio.h>
#include <sys/mnttab.h>
#include <sys/pool_impl.h>
#include <sys/sockio.h>
#include <sys/stropts.h>
#include <sys/termios.h>

/* pools */
__attribute__((noinline))
static void sys_ioctl_POOL_STATUSQ(void)
{
   GO(SYS_ioctl, "(POOL_STATUSQ) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + POOL_STATUSQ, x0); FAIL;
}

/* mntio */
__attribute__((noinline))
static void sys_ioctl_MNTIOC_GETEXTMNTENT(void)
{
   GO(SYS_ioctl, "(MNTIOC_GETEXTMNTENT) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + MNTIOC_GETEXTMNTENT, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_MNTIOC_GETEXTMNTENT_2(void)
{
   struct mntentbuf embuf;

   embuf.mbuf_emp = (void *) (x0 + 1);
   embuf.mbuf_buf = (void *) (x0 + 1);
   embuf.mbuf_bufsize = x0 + 1;

   GO(SYS_ioctl, "(MNTIOC_GETEXTMNTENT) 4s 2m");
   SY(SYS_ioctl, x0 - 1, x0 + MNTIOC_GETEXTMNTENT, &embuf + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_MNTIOC_GETEXTMNTENT_3(void)
{
   struct extmnttab mnt;
   struct mntentbuf embuf;

   mnt.mnt_special = (void *) (x0 + 1);
   mnt.mnt_mountp = (void *) (x0 + 1);
   mnt.mnt_fstype = (void *) (x0 + 1);
   mnt.mnt_mntopts = (void *) (x0 + 1);
   mnt.mnt_time = (void *) (x0 + 1);

   embuf.mbuf_emp = x0 + &mnt;
   embuf.mbuf_buf = (void *) (x0 + 1);
   embuf.mbuf_bufsize = x0 + 1;

   GO(SYS_ioctl, "(MNTIOC_GETEXTMNTENT) 5s 6m");
   SY(SYS_ioctl, x0 - 1, x0 + MNTIOC_GETEXTMNTENT, &embuf + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_MNTIOC_GETMNTANY(void)
{
   GO(SYS_ioctl, "(MNTIOC_GETMNTANY) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + MNTIOC_GETMNTANY, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_MNTIOC_GETMNTANY_2(void)
{
   struct mntentbuf embuf;

   embuf.mbuf_emp = (void *) (x0 + 1);
   embuf.mbuf_buf = (void *) (x0 + 1);
   embuf.mbuf_bufsize = x0 + 1;

   GO(SYS_ioctl, "(MNTIOC_GETMNTANY) 4s 2m");
   SY(SYS_ioctl, x0 - 1, x0 + MNTIOC_GETMNTANY, &embuf + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_MNTIOC_GETMNTANY_3(void)
{
   struct mnttab mnt;
   struct mntentbuf embuf;

   mnt.mnt_special = (void *) (x0 + 1);
   mnt.mnt_mountp = (void *) (x0 + 1);
   mnt.mnt_fstype = (void *) (x0 + 1);
   mnt.mnt_mntopts = (void *) (x0 + 1);
   mnt.mnt_time = (void *) (x0 + 1);

   embuf.mbuf_emp = x0 + (struct extmnttab *) &mnt;
   embuf.mbuf_buf = (void *) (x0 + 1);
   embuf.mbuf_bufsize = x0 + 1;

   GO(SYS_ioctl, "(MNTIOC_GETMNTANY) 5s 6m");
   SY(SYS_ioctl, x0 - 1, x0 + MNTIOC_GETMNTANY, &embuf + x0); FAIL;
}

/* termio/termios */
__attribute__((noinline))
static void sys_ioctl_TCGETA(void)
{
   GO(SYS_ioctl, "(TCGETA) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + TCGETA, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TCGETS(void)
{
   GO(SYS_ioctl, "(TCGETS) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + TCGETS, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TCSETS(void)
{
   GO(SYS_ioctl, "(TCSETS) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + TCSETS, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TCSETSW(void)
{
   GO(SYS_ioctl, "(TCSETSW) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + TCSETSW, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TCSETSF(void)
{
   GO(SYS_ioctl, "(TCSETSF) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + TCSETSF, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TIOCGWINSZ(void)
{
   GO(SYS_ioctl, "(TIOCGWINSZ) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + TIOCGWINSZ, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TIOCSWINSZ(void)
{
   GO(SYS_ioctl, "(TIOCSWINSZ) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + TIOCSWINSZ, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TIOCGPGRP(void)
{
   GO(SYS_ioctl, "(TIOCGPGRP) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + TIOCGPGRP, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TIOCSPGRP(void)
{
   GO(SYS_ioctl, "(TIOCSPGRP) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + TIOCSPGRP, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TIOCGSID(void)
{
   GO(SYS_ioctl, "(TIOCGSID) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + TIOCGSID, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TIOCNOTTY(void)
{
   GO(SYS_ioctl, "(TIOCNOTTY) 2s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + TIOCNOTTY); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_TIOCSCTTY(void)
{
   GO(SYS_ioctl, "(TIOCSCTTY) 2s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + TIOCSCTTY); FAIL;
}

/* STREAMS */
__attribute__((noinline))
static void sys_ioctl_I_PUSH(void)
{
   GO(SYS_ioctl, "(I_PUSH) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + I_PUSH, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_I_FLUSH(void)
{
   GO(SYS_ioctl, "(I_FLUSH) 3s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + I_FLUSH, x0 + FLUSHR); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_I_STR(void)
{
   GO(SYS_ioctl, "(I_STR) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + I_STR, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_I_STR_2(void)
{
   struct strioctl str;

   str.ic_cmd = x0;
   str.ic_timout = x0;
   str.ic_len = x0 + 1;
   str.ic_dp = (void *) (x0 + 1);

   GO(SYS_ioctl, "(I_STR) 4s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + I_STR, &str + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_I_FIND(void)
{
   GO(SYS_ioctl, "(I_FIND) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + I_FIND, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_I_PEEK(void)
{
   GO(SYS_ioctl, "(I_PEEK) 3s 7m");
   SY(SYS_ioctl, x0 - 1, x0 + I_PEEK, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_I_PEEK_2(void)
{
   struct strpeek peek;

   peek.ctlbuf.maxlen = x0 + 1;
   peek.ctlbuf.len = x0;
   peek.ctlbuf.buf = (void*)(x0 + 1);
   peek.databuf.maxlen = x0 + 1;
   peek.databuf.len = x0;
   peek.databuf.buf = (void*)(x0 + 1);
   peek.flags = x0;

   GO(SYS_ioctl, "(I_PEEK) 3s 7m");
   SY(SYS_ioctl, x0 - 1, x0 + I_PEEK, &peek + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_I_CANPUT(void)
{
   GO(SYS_ioctl, "(I_CANPUT) 3s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + I_CANPUT, x0); FAIL;
}

/* sockio */
__attribute__((noinline))
static void sys_ioctl_SIOCGIFCONF(void)
{
   GO(SYS_ioctl, "(SIOCGIFCONF), 3s 2m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGIFCONF, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGIFCONF_2(void)
{
   struct ifconf ifc;
   char buf[] = "";

   ifc.ifc_len = x0 + 1;
   ifc.ifc_buf = (void *) (x0 + buf);

   GO(SYS_ioctl, "(SIOCGIFCONF), 5s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGIFCONF, &ifc + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGIFFLAGS(void)
{
   GO(SYS_ioctl, "(SIOCGIFFLAGS) 3s 2m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGIFFLAGS, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGIFFLAGS_2(void)
{
   struct ifreq ifr;

   ifr.ifr_name[0] = x0 + 'l';
   ifr.ifr_name[1] = x0 + 'o';
   ifr.ifr_name[2] = x0 + '0';
   ifr.ifr_name[3] = x0 + '\0';

   GO(SYS_ioctl, "(SIOCGIFFLAGS), 4s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGIFFLAGS, &ifr + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGIFNETMASK(void)
{
   GO(SYS_ioctl, "(SIOCGIFNETMASK) 3s 2m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGIFNETMASK, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGIFNETMASK_2(void)
{
   struct ifreq ifr;

   ifr.ifr_name[0] = x0 + 'l';
   ifr.ifr_name[1] = x0 + 'o';
   ifr.ifr_name[2] = x0 + '0';
   ifr.ifr_name[3] = x0 + '\0';

   GO(SYS_ioctl, "(SIOCGIFNETMASK), 4s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGIFNETMASK, &ifr + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGIFNUM(void)
{
   int ifnum;

   GO(SYS_ioctl, "(SIOCGIFNUM) 3s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGIFNUM, &ifnum + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGIFNUM_2(void)
{
   GO(SYS_ioctl, "(SIOCGIFNUM) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGIFNUM, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGLIFBRDADDR(void)
{
   GO(SYS_ioctl, "(SIOCGLIFBRDADDR) 3s 2m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGLIFBRDADDR, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGLIFBRDADDR_2(void)
{
   struct lifreq lifr;

   lifr.lifr_name[0] = x0 + 'l';
   lifr.lifr_name[1] = x0 + 'o';
   lifr.lifr_name[2] = x0 + '0';
   lifr.lifr_name[3] = x0 + '\0';

   GO(SYS_ioctl, "(SIOCGLIFBRDADDR), 4s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGLIFBRDADDR, &lifr + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGLIFCONF(void)
{
   GO(SYS_ioctl, "(SIOCGLIFCONF), 3s 4m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGLIFCONF, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGLIFCONF_2(void)
{
   struct lifconf lifc;
   char buf[] = "";

   lifc.lifc_len = x0 + 1;
   lifc.lifc_buf = (void *) (x0 + buf);
   lifc.lifc_family = x0 + 1;
   lifc.lifc_flags = x0 + 0;

   GO(SYS_ioctl, "(SIOCGLIFCONF), 7s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGLIFCONF, &lifc + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGLIFFLAGS(void)
{
   GO(SYS_ioctl, "(SIOCGLIFFLAGS) 3s 2m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGLIFFLAGS, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGLIFFLAGS_2(void)
{
   struct lifreq lifr;

   lifr.lifr_name[0] = x0 + 'l';
   lifr.lifr_name[1] = x0 + 'o';
   lifr.lifr_name[2] = x0 + '0';
   lifr.lifr_name[3] = x0 + '\0';

   GO(SYS_ioctl, "(SIOCGLIFFLAGS), 4s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGLIFFLAGS, &lifr + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGLIFNETMASK(void)
{
   GO(SYS_ioctl, "(SIOCGLIFNETMASK) 3s 2m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGLIFNETMASK, x0 - 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGLIFNETMASK_2(void)
{
   struct lifreq lifr;

   lifr.lifr_name[0] = x0 + 'l';
   lifr.lifr_name[1] = x0 + 'o';
   lifr.lifr_name[2] = x0 + '0';
   lifr.lifr_name[3] = x0 + '\0';

   GO(SYS_ioctl, "(SIOCGLIFNETMASK), 4s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGLIFNETMASK, &lifr + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_SIOCGLIFNUM(void)
{
   struct lifnum lifn;

   lifn.lifn_family = x0;
   lifn.lifn_flags = x0;

   GO(SYS_ioctl, "(SIOCGLIFNUM) 4s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + SIOCGLIFNUM, &lifn + x0); FAIL;
}

/* filio */
__attribute__((noinline))
static void sys_ioctl_FIOSETOWN(void)
{
   pid_t pid;

   GO(SYS_ioctl, "(FIOSETOWN) 4s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + FIOSETOWN, &pid + x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_FIOGETOWN(void)
{
   GO(SYS_ioctl, "(FIOGETOWN) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + FIOGETOWN, x0 + 1); FAIL;
}

/* crypto */
__attribute__((noinline))
static void sys_ioctl_CRYPTO_GET_PROVIDER_LIST(void)
{
   GO(SYS_ioctl, "(CRYPTO_GET_PROVIDER_LIST) 3s 1m");
   SY(SYS_ioctl, x0 - 1, x0 + CRYPTO_GET_PROVIDER_LIST, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_CRYPTO_GET_PROVIDER_LIST_2(void)
{
   crypto_get_provider_list_t pl;

   pl.pl_count = x0 + 1;

   GO(SYS_ioctl, "(CRYPTO_GET_PROVIDER_LIST) 4s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + CRYPTO_GET_PROVIDER_LIST, &pl + x0); FAIL;
}

/* dtrace */
__attribute__((noinline))
static void sys_ioctl_DTRACEHIOC_REMOVE(void)
{
   GO(SYS_ioctl, "(DTRACEHIOC_REMOVE) 3s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + DTRACEHIOC_REMOVE, x0); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_DTRACEHIOC_ADDDOF(void)
{
   dof_helper_t dh;

   dh.dofhp_mod[0] = x0 + 'D';
   dh.dofhp_mod[1] = x0 + '\0';
   dh.dofhp_addr = x0;
   dh.dofhp_dof = x0;

   GO(SYS_ioctl, "(DTRACEHIOC_ADDDOF) 6s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + DTRACEHIOC_ADDDOF, x0 + &dh); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_DINFOUSRLD(void)
{
   GO(SYS_ioctl, "(DINFOUSRLD) 3s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + DINFOUSRLD, x0 + 1); FAIL;
}

__attribute__((noinline))
static void sys_ioctl_DINFOIDENT(void)
{
   GO(SYS_ioctl, "(DINFOIDENT) 2s 0m");
   SY(SYS_ioctl, x0 - 1, x0 + DINFOIDENT); FAIL;
}

int main(void)
{
   /* Uninitialised, but we know px[0] is 0x0. */
   long *px = malloc(sizeof(long));
   x0 = px[0];

   /* pools */
   sys_ioctl_POOL_STATUSQ();

   /* mntio */
   sys_ioctl_MNTIOC_GETEXTMNTENT();
   sys_ioctl_MNTIOC_GETEXTMNTENT_2();
   sys_ioctl_MNTIOC_GETEXTMNTENT_3();
   sys_ioctl_MNTIOC_GETMNTANY();
   sys_ioctl_MNTIOC_GETMNTANY_2();
   sys_ioctl_MNTIOC_GETMNTANY_3();

   /* termio/termios */
   sys_ioctl_TCGETA();
   sys_ioctl_TCGETS();
   sys_ioctl_TCSETS();
   sys_ioctl_TCSETSW();
   sys_ioctl_TCSETSF();
   sys_ioctl_TIOCGWINSZ();
   sys_ioctl_TIOCSWINSZ();
   sys_ioctl_TIOCGPGRP();
   sys_ioctl_TIOCSPGRP();
   sys_ioctl_TIOCGSID();
   sys_ioctl_TIOCNOTTY();
   sys_ioctl_TIOCSCTTY();

   /* STREAMS */
   sys_ioctl_I_PUSH();
   sys_ioctl_I_FLUSH();
   sys_ioctl_I_STR();
   sys_ioctl_I_STR_2();
   sys_ioctl_I_FIND();
   sys_ioctl_I_PEEK();
   sys_ioctl_I_PEEK_2();
   sys_ioctl_I_CANPUT();

   /* sockio */
   sys_ioctl_SIOCGIFCONF();
   sys_ioctl_SIOCGIFCONF_2();
   sys_ioctl_SIOCGIFFLAGS();
   sys_ioctl_SIOCGIFFLAGS_2();
   sys_ioctl_SIOCGIFNETMASK();
   sys_ioctl_SIOCGIFNETMASK_2();
   sys_ioctl_SIOCGIFNUM();
   sys_ioctl_SIOCGIFNUM_2();
   sys_ioctl_SIOCGLIFBRDADDR();
   sys_ioctl_SIOCGLIFBRDADDR_2();
   sys_ioctl_SIOCGLIFCONF();
   sys_ioctl_SIOCGLIFCONF_2();
   sys_ioctl_SIOCGLIFFLAGS();
   sys_ioctl_SIOCGLIFFLAGS_2();
   sys_ioctl_SIOCGLIFNETMASK();
   sys_ioctl_SIOCGLIFNETMASK_2();
   sys_ioctl_SIOCGLIFNUM();

   /* filio */
   sys_ioctl_FIOSETOWN();
   sys_ioctl_FIOGETOWN();

   /* crypto */
   sys_ioctl_CRYPTO_GET_PROVIDER_LIST();
   sys_ioctl_CRYPTO_GET_PROVIDER_LIST_2();

   /* dtrace */
   sys_ioctl_DTRACEHIOC_REMOVE();
   sys_ioctl_DTRACEHIOC_ADDDOF();

   /* devinfo */
   sys_ioctl_DINFOUSRLD();
   sys_ioctl_DINFOIDENT();

   return 0;
}

