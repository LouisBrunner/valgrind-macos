void drd_post_thread_join(DrdThreadId joiner, DrdThreadId joinee);
void drd_pre_mutex_init(Addr mutex, SizeT size, const MutexT mutex_type);
void drd_post_mutex_destroy(Addr mutex, const MutexT mutex_type);
void drd_pre_mutex_lock(DrdThreadId tid, Addr mutex, const SizeT size,
                        const MutexT mutex_type);
void drd_post_mutex_lock(DrdThreadId tid, Addr mutex, const SizeT size,
                         const MutexT mutex_type);
void drd_pre_mutex_unlock(const DrdThreadId tid, const Addr mutex,
                          const MutexT mutex_type);
void drd_post_cond_init(Addr cond, SizeT s);
void drd_pre_cond_destroy(Addr cond);
