#if SIZEOF_INT > 2
# if defined(SSIZE_MAX) && (SSIZE_MAX < 0x10000L)
        size = SSIZE_MAX;           /* use max I/O size, 52K */
# else
        size = 0x10000L;            /* use buffer >= 64K */
# endif
#else
        size = 0x7ff0L - linerest;      /* limit buffer to 32K */
#endif
