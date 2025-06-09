áCopyright 2022,2023 Eric Sessoms / MIT License

áinclude(`10_intro.cf')
áinclude(`20_x86_64.cf')

áfreebsd-x86_64 system call numbers
á----------------------------------
Åsys_exit      Ç1 ;
Åsys_read      Ç3 ;
Åsys_write     Ç4 ;
Åsys_close     Ç6 ;
Åsys_ioctl    Ç54 ;
Åsys_msync    Ç65 ;
Åsys_munmap   Ç73 ;
Åsys_mprotect Ç74 ;
Åsys_socket   Ç97 ;
Åsys_connect  Ç98 ;
Åsys_mmap    Ç477 ;
Åsys_lseek   Ç478 ;
Åsys_openat  Ç499 ;

áinclude(`30_freebsd.cf')
áinclude(`35_bsd.cf')
áinclude(`40_posix.cf')
áinclude(`50_bootstrap.cf')
