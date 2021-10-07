/pthread_create_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /pthread_create \(hg_intercepts.c/)) {
      print "   by 0x........: pthread_create@* (hg_intercepts.c:...)";
      print;
   } else {
      sub(/pthread_create/, "pthread_create@*");
      print;
   }
   next;
}
/pthread_cond_timedwait_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /pthread_cond_timedwait \(hg_intercepts.c/)) {
      print "   by 0x........: pthread_cond_timedwait@* (hg_intercepts.c:...)";
      print;
   } else {
      sub(/pthread_cond_timedwait/, "pthread_cond_timedwait@*");
      print;
   }
   next;
}
/pthread_cond_wait_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /pthread_cond_wait \(hg_intercepts.c/)) {
      print "   by 0x........: pthread_cond_wait@* (hg_intercepts.c:...)";
      print;
   } else {
      sub(/pthread_cond_wait/, "pthread_cond_wait@*");
      print;
   }
   next;
}
/pthread_cond_destroy_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /pthread_cond_destroy \(hg_intercepts.c/)) {
      print "   by 0x........: pthread_cond_destroy@* (hg_intercepts.c:...)";
      print;
   } else {
      sub(/pthread_cond_destroy/, "pthread_cond_destroy@*");
      print;
   }
   next;
}
/pthread_cond_signal_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /pthread_cond_signal \(hg_intercepts.c/)) {
      print "   by 0x........: pthread_cond_signal (hg_intercepts.c:...)";
      print;
   } else {
      print;
   }
   next;
}
/mutex_lock_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /pthread_mutex_lock \(hg_intercepts.c/)) {
      print "   by 0x........: pthread_mutex_lock (hg_intercepts.c:...)";
      print;
   } else {
      print;
   }
   next;
}
/mutex_unlock_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /pthread_mutex_unlock \(hg_intercepts.c/)) {
      print "   by 0x........: pthread_mutex_unlock (hg_intercepts.c:...)";
      print;
   } else {
      print;
   }
   next;
}
/pthread_rwlock_unlock_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /pthread_rwlock_unlock \(hg_intercepts.c/)) {
      print "   by 0x........: pthread_rwlock_unlock (hg_intercepts.c:...)";
      print;
   } else {
      print;
   }
   next;
}
/pthread_rwlock_init_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /pthread_rwlock_init \(hg_intercepts.c/)) {
      print "   by 0x........: pthread_rwlock_init (hg_intercepts.c:...)";
      print;
   } else {
      print;
   }
   next;
}
/sem_init_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /sem_init \(hg_intercepts.c/)) {
      print "   by 0x........: sem_init (hg_intercepts.c:...)";
      print;
   } else {
      print;
   }
   next;
}
/sem_wait_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /sem_wait \(hg_intercepts.c/)) {
      print "   by 0x........: sem_wait (hg_intercepts.c:...)";
      print;
   } else {
      print;
   }
   next;
}
/sem_post_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /sem_post \(hg_intercepts.c/)) {
      print "   by 0x........: sem_post (hg_intercepts.c:...)";
      print;
   } else {
      print;
   }
   next;
}
/sem_destroy_WRK \(hg_intercepts.c/ {
   print;
   getline;
   if (!match($0, /sem_destroy \(hg_intercepts.c/)) {
      print "   by 0x........: sem_destroy (hg_intercepts.c:...)";
      print;
   } else {
      print;
   }
   next;
}
{print;}
