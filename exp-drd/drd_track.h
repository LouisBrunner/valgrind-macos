void drd_post_thread_join(DrdThreadId joiner, DrdThreadId joinee);
void drd_pre_mutex_init(Addr mutex, SizeT size);
void drd_post_mutex_destroy(Addr mutex, SizeT size);
void drd_pre_mutex_lock(DrdThreadId tid, Addr mutex, const SizeT size);
void drd_post_mutex_lock(DrdThreadId tid, Addr mutex, const SizeT size);
void drd_pre_mutex_unlock(DrdThreadId tid, Addr mutex);
void drd_post_cond_init(Addr cond, SizeT s);
void drd_pre_cond_destroy(Addr cond, SizeT s);
